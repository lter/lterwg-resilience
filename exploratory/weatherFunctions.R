# 2024-02-27, KEM

# Functions used to calculate weather indices. 
# Saturated vapor pressure------
swat_satv <- function(Tc){exp(
  (16.78*Tc - 116.9)/(Tc + 237.3) 
)}
comment(swat_satv) <- c("Source: SWAT 2005 technical manual eq. 1:2.3.2.",
                        "Input: Tc = mean daily temperature, deg.C.",
                        "Output: Saturated vapor pressure, kPa.")

tetens_satv <- function(Tc){
  ifelse(Tc>0,
         0.61078* exp(
           (17.27*Tc)/(Tc + 237.3)),
         0.61078*exp(21.875*Tc/(Tc + 265.5))
  )
}
comment(tetens_satv)<- c("Source: Tetens and Magnus,https://en.wikipedia.org/wiki/Tetens_equation",
                         "Input: Tc = mean daily temperature, deg.C.",
                         "Output: Saturated vapor pressure, kPa")
tetens2_satv <- function(Tc){
  ifelse(Tc>0,
         0.61094* exp(
           (17.625*Tc)/(Tc + 243.04)),
         0.61121*exp(22.587*Tc/(Tc + 273.86))
  )
}
comment(tetens2_satv) <- c("Source: Improved Magnus formula, Huang 2018, Eq. 3 and 4, DOI:10.1175/JAMC-D-17-0334.1.",
                           "Input: Tc = mean daily temperature, deg. C",
                           "Output: Saturated vapor pressure, kPa.")
huang_satv <- function(Tc){
  outnum <- ifelse(Tc > 0, 
                   exp(34.494 - 4924.99/(Tc + 237.1))/(Tc + 105)^1.57,
                   exp(43.494 - 6545.8/(Tc + 278))/(Tc + 868)^2
  ) # in Pa
  return(outnum/1000) # in kPa
}
comment(huang_satv) <- c("Source: Huang 2018, eq. 18, DOI:10.1175/JAMC-D-17-0334.1",
                         "Input: Tc = mean daily temperature, deg. C",
                         "Output: Saturated vapor pressure, kPa")
# VPD--------
VPD <- function(es, ea){
  return(es - ea)
}
comment(VPD) <- c("Source: difference between saturated vapor pressure and air vapor pressure.",
                  "Input: es = saturated vapor pressure, kPa. ea = air vapor pressure.",
                  "Output: Vapor pressure deficit (VPD), kPa.")

# PET-------
# Potential evaporation: rate at which evapotranspiration would occur
# from a large area uniformly covered with growing vegetation and with 
# an unlimited supply of water, and no heat stres.
hamonPET <- function(Tc,H, es){
  (2.1*(H^2)*es)/(273.2 + Tc)
}
comment(hamonPET) <- c("Source: Hamon algorithm. https://qed.epa.gov/hms/hydrology/evapotranspiration/algorithms/.",
                       "Input: Tc = mean daily temperature, deg. C, 
                       H = number of daylight hours,
                       es = saturated vapor pressure, kPa",
                       "Output: Potential evapotranspiration (inches/day)")
hargreavesPET <- function(Tmax, Tmin, Tc,H0){
  0.0023*H0 *((Tmax-Tmin)^0.5)*(Tc + 17.8)
}

comment(hargreavesPET) <- c("Source: Hargreaves algorithm. https://qed.epa.gov/hms/hydrology/evapotranspiration/algorithms/.",
                       "Input: Tc = mean daily temperature, deg. C,
                       Tmax = max daily temperature, deg. C,
                       Tmin = min daily temperature, deg. C,
                       H0 = extraterrestrial radiation, MJm-2d-1",
                       "Output: Potential evapotranspiration (inches/day)")

# Heat stress units--------
#added up for all days with Tmax > 30
HSU <- function(Tmaxvec = rnorm(7,30,1), thresh=30){
  TmaxOverThresh <- Tmaxvec[which(Tmaxvec > thresh)]
  outHSU <- sum(TmaxOverThresh - thresh)
  return(outHSU)
}
comment(HSU) <- c("Source Heat Stress Units from Teasdale & Cavigelli, 2017, DOI:10.1038/s41598-017-00775-8.",
                  "Input: Tmaxvec = vector of daily max temperatures (deg C), spanning a continuous time range
                  thresh = threshold for high temperature, set at 30degC based on source.",
                  "Output: For all sum(Tmax -30), for all Tmax > 30",
                  "Note: Since this is a simple summation, HSU can be added up for days in a week, then for weeks in a critical period.")
  