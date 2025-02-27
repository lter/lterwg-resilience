library(aqp)
library(soilDB)
library(terra)
library(sf)
library(glue)
library(gridExtra)
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)
sites <- read.csv("data/site_summary_info.csv",fileEncoding = "UTF-8-BOM")

## use lat lon to query mukey
outres <- vector(mode = "list", length = nrow(sites))
names(outres) <- sites$site_id
for(i in 1:nrow(sites)){
  mylon <- sites$longitude[i]
  mylat <- sites$latitude[i]
  q <- glue("SELECT mukey, muname
  FROM mapunit
  WHERE mukey IN (
  SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('point({mylon} {mylat})')
  )")
  res <- SDA_query(q)
  if(!is.null(res)){
    res$site_id <- sites$site_id[i]
    outres[[i]] <- res
    rm(res)
  }
}

resdf <- do.call("bind_rows",outres)
siteres <- left_join(sites, resdf)


# make a list
surgolist <- vector(mode = "list", length = nrow(siteres))
names(surgolist) <- siteres$site_id

# composite SQL WHERE clause

for(i in 1:nrow(siteres)){
  mymukey <- siteres$mukey[i]
  if(!is.na(mymukey)){
    fetchdf <- fetchSDA(WHERE = glue("mukey IN '{mymukey}'"),duplicates = TRUE)
    surgolist[[i]] <- fetchdf  
  }
}
siteres$hasSURGO <- lapply(surgolist, function(x)!is.null(x)) %>% unlist()

## get number of components per mukey
numcomps<- purrr::map(surgolist, 
                       function(x)if(length(x)>0){ nrow(x@site)}else{NA}
)
siteres$numcomps <- unlist(numcomps)

table(siteres$numcomps)

## save
#save(siteres, surgolist, file = "data/surgoObjects_20250225.RData")

#View(siteres)

## useful info:
## taxonomy:
## relates to parent material
surgolist$CAF@site$taxorder
## hydrogrp. 
surgolist$CAF@site$hydgrp

# problemfiles
##a lot of sites had mukeys, but the fetchSDA didn't work. 
problemdf <- siteres[which(is.na(siteres$numcomps)),]
## a couple examples to troubleshoot
WHERE_ok = "mukey = '68483'" # a mukey that worked
WHERE_problem = "mukey = '2234409'" # a mukey that didn't work. 


fetchdf <- try(fetchSDA(WHERE = "mukey = '2234409'",duplicates = TRUE))

single.mukey <- try(fetchSDA_spatial(x = "2234409",add.fields = "site.hydgrp"))

test1 <- get_component_from_SDA(
  WHERE = WHERE_ok,
  duplicates = TRUE,
  childs = TRUE,
  droplevels = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL
)

# didn't work
test2 <- get_component_from_SDA(
  WHERE = WHERE_problem,
  duplicates = TRUE,
  childs = TRUE,
  droplevels = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL
)

## pulled a piece of the get_component_from_SDA function
## that works for all mukeys
get_basic_SDA <- function(WHERE, duplicates = TRUE){
  es.vars <- "ecoclasstypename, ecoclassref, ecoclassid, ecoclassname"
  co.vars <- "co.cokey, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, erocl, earthcovkind1, earthcovkind2, elev_r, slope_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, hydgrp,  nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, tfact, wei, weg, corcon, corsteel, frostact, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition"
  vars <- paste0(unlist(strsplit(co.vars, "earthcovkind2,")), 
                 collapse = paste0("earthcovkind2, ", es.vars, ","))
  q.component <- paste("SELECT", if (duplicates == FALSE) 
    "DISTINCT"
    else "mu.mukey AS mukey,", "mu.nationalmusym,", 
    vars, "FROM\n     legend  l                      INNER JOIN\n     mapunit mu ON mu.lkey = l.lkey INNER JOIN", 
    if (duplicates == FALSE) {
      "(SELECT nationalmusym AS nationalmusym2, MIN(mukey) AS mukey2\n      FROM mapunit\n      GROUP BY nationalmusym\n     ) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym INNER JOIN"
    }
    else "", "(SELECT", vars, ", mukey AS mukey2\n      FROM\n      component  co                        LEFT OUTER JOIN\n      coecoclass ce ON ce.cokey = co.cokey AND\n                       ecoclasstypename IN ('NRCS Rangeland Site', 'NRCS Forestland Site')\n      ) AS co ON co.mukey2 =", 
    if (duplicates == FALSE) 
      "mu2.mukey2"
    else "mu.mukey", "WHERE", WHERE, "ORDER BY nationalmusym, comppct_r DESC, compname;")
  d.component <- SDA_query(q.component)
  return(d.component) 
}

get_basic_SDA(WHERE = WHERE_problem)# this works.

## get a new list.
surgolist2 <- purrr::imap(siteres$mukey, ~ {get_basic_SDA(WHERE = glue::glue("mukey = '{.x}'"))}
                            )
# convert to a data frame.
surgodf <- do.call('rbind', surgolist2) 
surgodf <- left_join(surgodf, siteres[,c("site_id","network","mukey")],by = "mukey")

### how common is it for components in  a single map unit to have varying taxonomic order and hydrogroup?
## only count components that are flagged as major based on percent of total
soilbasics <- surgodf %>% group_by(site_id, network) %>%
                summarize(num.hg = length(unique(hydgrp[!is.na(hydgrp) & majcompflag=="Yes"])),
                          num.tax = length(unique(taxorder[!is.na(taxorder)& majcompflag=="Yes"])),
                          nummajor = sum(majcompflag=="Yes"),
                          prvtotal = sum(comppct_r))
table(soilbasics$num.hg)# fairly common to have >1 hydrogroup
table(soilbasics$num.tax)# less common to have multiple taxonomic orders. 
table(soilbasics$nummajor)# most sites have one major component.
table(surgodf$site_id, surgodf$hydgrp)

## filter for only major components 
surgodf2 <- surgodf[which(surgodf$majcompflag=="Yes"),] 
dupsites <- soilbasics$site_id[which(soilbasics$num.hg>1 | soilbasics$num.tax > 1)]
## examine ones with multiple different.
#View(surgodf2[which(surgodf2$site_id %in% dupsites),c("site_id","mukey","comppct_r","majcompflag","hydgrp","taxorder","taxsuborder")])
#View(surgodf2)

## solution: add up component percentage with the same hydrogroup
# and the same taxonomic order (separately). Pick the category with the highest percent. 
hgdf <- surgodf2 %>% group_by(site_id, hydgrp)%>%
              summarize(hydgrp_comppct = sum(comppct_r))%>%
              group_by(site_id) %>%
              filter(hydgrp_comppct== max(hydgrp_comppct))
taxdf <- surgodf2 %>% group_by(site_id, taxorder)%>%
  summarize(taxorder_comppct = sum(comppct_r))%>%
  group_by(site_id) %>%
  filter(taxorder_comppct== max(taxorder_comppct))
taxsubdf <- surgodf2 %>% group_by(site_id, taxsuborder)%>%
  summarize(taxsuborder_comppct = sum(comppct_r))%>%
  group_by(site_id) %>%
  filter(taxsuborder_comppct== max(taxsuborder_comppct))

soildf <- left_join(site_info, siteres[,c("site_id","mukey")]) %>%
            left_join(hgdf) %>%
            left_join(taxdf) %>%
            left_join(taxsubdf) %>%
            subset(select = -c(start_yr,end_yr,sort))
#write.csv(soildf, file = "data/tidy_data/soilvars.csv",row.names = FALSE)

# googledrive::drive_upload(media = file.path("data", "tidy_data","soilvars.csv"), overwrite = T,
#                           path = googledrive::as_id(dir.tidy_data))


### fiddly stuff below here.
get_cointerp_from_SDA(
  WHERE = WHERE_problem,
  mrulename = NULL,
  duplicates = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_legend_from_SDA(WHERE = WHERE_problem, droplevels = TRUE, stringsAsFactors = NULL)

get_lmuaoverlap_from_SDA(
  WHERE = WHERE_ok,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_mapunit_from_SDA(WHERE = WHERE_ok, droplevels = TRUE, stringsAsFactors = NULL)
get_mapunit_from_SDA(WHERE = WHERE_problem, droplevels = TRUE, stringsAsFactors = NULL)

get_chorizon_from_SDA(
  WHERE = WHERE_ok,
  duplicates = TRUE,
  childs = TRUE,
  nullFragsAreZero = TRUE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)# doesn't work

fetchSDA(
  WHERE = WHERE_ok,
  duplicates = FALSE,
  childs = TRUE,
  nullFragsAreZero = TRUE,
  rmHzErrors = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_cosoilmoist_from_SDA(
  WHERE = WHERE_ok,
  duplicates = FALSE,
  impute = TRUE,
  stringsAsFactors = NULL
)

get_cosoilmoist_from_SDA(
  WHERE = WHERE_problem,
  duplicates = FALSE,
  impute = TRUE,
  stringsAsFactors = NULL
)
