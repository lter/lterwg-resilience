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
sites <- read.csv("data/site-coordinates.csv",fileEncoding = "UTF-8-BOM")
## limit to US and Canada
sitesToExclude <- sites$site_id[which(sites$project_id=="NutNet" &
                                        !grepl("(.us$)|(.ca$)",sites$site_id))]

sites <- sites[which(!sites$site_id %in% sitesToExclude),]

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

for(i in 1:nrow(siteres)){
  mymukey <- siteres$mukey[i]
  if(!is.na(mymukey)){
    fetchdf <- fetchSDA(WHERE = glue("mukey = '{mymukey}'"),duplicates = TRUE)
    surgolist[[i]] <- fetchdf  
  }
}

siteres$hasSURGO <- lapply(surgolist, function(x)!is.null(x)) %>% unlist()

## get number of components per mukey
numcomps <- purrr::map(surgolist, 
                       function(x)if(length(x)>0){ nrow(x@site)}else{NA}
)
siteres$numcomps <- unlist(numcomps)

table(siteres$numcomps)

## save
#save(siteres, surgolist, file = "data/surgoObjects_20250225.RData")

