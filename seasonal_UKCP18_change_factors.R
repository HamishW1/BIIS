#===============================================================================
#                     UKCP18 Seasonal Change Factors
#===============================================================================

#   In this script we will extract the trend from UKCP18 on a seasonal level
#   and use this to provide a delta style change factor to the era5 observations.

#-------------------------------------------------------------------------------
#                         Establish environment
#-------------------------------------------------------------------------------

library(raster)
library(ncdf4)
library(tidyverse)
library(sf)
library(data.table)
library(doSNOW)
library(foreach)
library(parallel)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#   Read in UKCP18 data

psl <- list()
wind <- list()

models <- dir("UKCP18/seasonal/pressure")        # Fetch models

fn <- list.files("UKCP18/seasonal/pressure/01")  # Create file names
fn1 <- strsplit(fn, split = "_01_")[[1]][1]
fn2 <- strsplit(fn, split = "_01_")[[1]][2]
wfn1 <- gsub(pattern = "psl", replacement = "sfcWind", fn1)

for(i in 1:length(models)){ # Read in
  
  psl[[i]] <- brick(paste0("UKCP18/seasonal/pressure/", models[i],"/",
                    fn1, "_", models[i], "_", fn2))
  
  wind[[i]] <- brick(paste0("UKCP18/seasonal/sfcWind/", models[i],"/",
                     wfn1, "_", models[i], "_", fn2))
  
}

#   Read in rasters to which we will apply change factors. These have already
#   been converted to UKCP18 daily format and resolution.

psl_e <- brick("era5/processed/daily_mean/era5_processed_pressure.nc")
wind_e <- brick("era5/processed/daily_mean/era5_processed_wind_speed.nc")

#-------------------------------------------------------------------------------
#                             Wrangle UKCP18
#-------------------------------------------------------------------------------

#   Now split UKCP18 into twenty year blocks

w1980 <- list()
w2000 <- list()
w2020 <- list()
w2040 <- list()
w2060 <- list() 

p1980 <- list()
p2000 <- list()
p2020 <- list()
p2040 <- list()
p2060 <- list() 



for(i in 1:length(models)){
  
  w1980[[i]] <- wind[[i]][[1:80]]
  w2000[[i]] <- wind[[i]][[81:160]]
  w2020[[i]] <- wind[[i]][[161:240]]
  w2040[[i]] <- wind[[i]][[241:320]]
  w2060[[i]] <- wind[[i]][[321:400]] 
  
  p1980[[i]] <- psl[[i]][[1:80]]
  p2000[[i]] <- psl[[i]][[81:160]]
  p2020[[i]] <- psl[[i]][[161:240]]
  p2040[[i]] <- psl[[i]][[241:320]]
  p2060[[i]] <- psl[[i]][[321:400]]  
  
}

#   I need to produce indexes to apply the seasons too

x = 1:7200
n = 80
obs_ind <- split(x, sort(x%%n))

w_ind <- list()
s_ind <- list()
su_ind <- list()
a_ind <- list()

#   Create indexs for seasons

for(i in seq(from = 1, to = 80, by = 4)){
  
  w_ind[[i]] <- obs_ind[[i]]
  s_ind[[i]] <- obs_ind[[i+1]]
  su_ind[[i]] <- obs_ind[[i+2]]
  a_ind[[i]] <- obs_ind[[i+3]]
  
}

#   Now remove null list elements

w_ind <- w_ind[-which(sapply(w_ind, is.null))]
s_ind <- s_ind[-which(sapply(s_ind, is.null))]
su_ind <- su_ind[-which(sapply(su_ind, is.null))]
a_ind <- a_ind[-which(sapply(a_ind, is.null))]

#   Create projection index ()

pw_ind <- seq(from = 1, to = 80, by = 4)
ps_ind <- seq(from = 2, to = 80, by = 4)
psu_ind <- seq(from = 3, to = 80, by = 4)
pa_ind <- seq(from = 4, to = 80, by = 4)

#   Create list to store outputs in ( this gonna be hella ugly )

#Wind
cfw2000 <- list()
cfw2020 <- list()
cfw2040 <- list()
cfw2060 <- list()

#Pressure
cfp2000 <- list()
cfp2020 <- list()
cfp2040 <- list()
cfp2060 <- list()

for(i in 1:4){ # These are to store the separate seasons in.
  
  for(j in 1:12){ # These are to store the seperate models in
    
    # Wind
    cfw2000[[i]] <- list()
    cfw2020[[i]] <- list()
    cfw2040[[i]] <- list()
    cfw2060[[i]] <- list()
    cfw2000[[i]][[j]] <- list()
    cfw2020[[i]][[j]] <- list()
    cfw2040[[i]][[j]] <- list()
    cfw2060[[i]][[j]] <- list()
    
    # Pressure
    cfp2000[[i]] <- list()
    cfp2020[[i]] <- list()
    cfp2040[[i]] <- list()
    cfp2060[[i]] <- list()
    cfp2000[[i]][[j]] <- list()
    cfp2020[[i]][[j]] <- list()
    cfp2040[[i]][[j]] <- list()
    cfp2060[[i]][[j]] <- list()
    
  }
}

#   Now calculate change factors

for(i in 1:12){ # Models
    
  for(j in 1:20){ # Years in decade
    
    print(paste0("Working on seasonal change factors for model ", i, " year ", j, " in 2000-2020"))
    
    # Wind
    cfw2000[[1]][[i]][[j]] <- (w2000[[i]][[pw_ind]] - w1980[[i]][[pw_ind]]) + wind_e[[w_ind[[j]]]] # Winter
    cfw2000[[2]][[i]][[j]] <- (w2000[[i]][[ps_ind]] - w1980[[i]][[ps_ind]]) + wind_e[[s_ind[[j]]]] # Spring
    cfw2000[[3]][[i]][[j]] <- (w2000[[i]][[psu_ind]] - w1980[[i]][[psu_ind]]) + wind_e[[su_ind[[j]]]] # Summer
    cfw2000[[4]][[i]][[j]] <- (w2000[[i]][[pa_ind]] - w1980[[i]][[pa_ind]]) + wind_e[[a_ind[[j]]]] # Autumn
    
    # Pressure
    cfp2000[[1]][[i]][[j]] <- (p2000[[i]][[pw_ind]] - p1980[[i]][[pw_ind]]) + psl_e[[w_ind[[j]]]] # Winter
    cfp2000[[2]][[i]][[j]] <- (p2000[[i]][[ps_ind]] - p1980[[i]][[ps_ind]]) + psl_e[[s_ind[[j]]]] # Spring
    cfp2000[[3]][[i]][[j]] <- (p2000[[i]][[psu_ind]] - p1980[[i]][[psu_ind]]) + psl_e[[su_ind[[j]]]] # Summer
    cfp2000[[4]][[i]][[j]] <- (p2000[[i]][[pa_ind]] - p1980[[i]][[pa_ind]]) + psl_e[[a_ind[[j]]]] # Autumn
    
    print(paste0("Working on seasonal change factors for model ", i, " year ", j, " in 2020-2040"))
    
    # Wind
    cfw2020[[1]][[i]][[j]] <- (w2020[[i]][[pw_ind]] - w1980[[i]][[pw_ind]]) + wind_e[[w_ind[[j]]]] # w2020 is the projection
    cfw2020[[2]][[i]][[j]] <- (w2020[[i]][[ps_ind]] - w1980[[i]][[ps_ind]]) + wind_e[[s_ind[[j]]]] # w1980 is the baseline
    cfw2020[[3]][[i]][[j]] <- (w2020[[i]][[psu_ind]] - w1980[[i]][[psu_ind]]) + wind_e[[su_ind[[j]]]] # wind_e are observations
    cfw2020[[4]][[i]][[j]] <- (w2020[[i]][[pa_ind]] - w1980[[i]][[pa_ind]]) + wind_e[[a_ind[[j]]]]  
    
    # Pressure
    cfp2020[[1]][[i]][[j]] <- (p2020[[i]][[pw_ind]] - p1980[[i]][[pw_ind]]) + psl_e[[w_ind[[j]]]] # Projections are seasons
    cfp2020[[2]][[i]][[j]] <- (p2020[[i]][[ps_ind]] - p1980[[i]][[ps_ind]]) + psl_e[[s_ind[[j]]]] # Baseline are seasons
    cfp2020[[3]][[i]][[j]] <- (p2020[[i]][[psu_ind]] - p1980[[i]][[psu_ind]]) + psl_e[[su_ind[[j]]]] # Observations are days
    cfp2020[[4]][[i]][[j]] <- (p2020[[i]][[pa_ind]] - p1980[[i]][[pa_ind]]) + psl_e[[a_ind[[j]]]] 
    
    print(paste0("Working on seasonal change factors for model ", i, " year ", j, " in 2040-2060"))
    
    # Wind
    cfw2040[[1]][[i]][[j]] <- (w2040[[i]][[pw_ind]] - w1980[[i]][[pw_ind]]) + wind_e[[w_ind[[j]]]] # the model index selects
    cfw2040[[2]][[i]][[j]] <- (w2040[[i]][[ps_ind]] - w1980[[i]][[ps_ind]]) + wind_e[[s_ind[[j]]]] # all the specified seasons for
    cfw2040[[3]][[i]][[j]] <- (w2040[[i]][[psu_ind]] - w1980[[i]][[psu_ind]]) + wind_e[[su_ind[[j]]]] # twenty years - so pw_ind
    cfw2040[[4]][[i]][[j]] <- (w2040[[i]][[pa_ind]] - w1980[[i]][[pa_ind]]) + wind_e[[a_ind[[j]]]] # selects every winter, ps_ind
                                                                                              # every spring etc
    # Pressure
    cfp2040[[1]][[i]][[j]] <- (p2040[[i]][[pw_ind]] - p1980[[i]][[pw_ind]]) + psl_e[[w_ind[[j]]]] # This model index selects the 
    cfp2040[[2]][[i]][[j]] <- (p2040[[i]][[ps_ind]] - p1980[[i]][[ps_ind]]) + psl_e[[s_ind[[j]]]] # season for both the baseline
    cfp2040[[3]][[i]][[j]] <- (p2040[[i]][[psu_ind]] - p1980[[i]][[psu_ind]]) + psl_e[[su_ind[[j]]]] # and the projection.
    cfp2040[[4]][[i]][[j]] <- (p2040[[i]][[pa_ind]] - p1980[[i]][[pa_ind]]) + psl_e[[a_ind[[j]]]] 
    
    print(paste0("Working on seasonal change factors for model ", i, " year ", j, " in 2060-2080"))
    
    # Wind
    cfw2060[[1]][[i]][[j]] <- (w2060[[i]][[pw_ind]] - w1980[[i]][[pw_ind]]) + wind_e[[w_ind[[j]]]] # The observation index selects 
    cfw2060[[2]][[i]][[j]] <- (w2060[[i]][[ps_ind]] - w1980[[i]][[ps_ind]]) + wind_e[[s_ind[[j]]]] # every day in a season across
    cfw2060[[3]][[i]][[j]] <- (w2060[[i]][[psu_ind]] - w1980[[i]][[psu_ind]]) + wind_e[[su_ind[[j]]]] # twenty years. So w_ind
    cfw2060[[4]][[i]][[j]] <- (w2060[[i]][[pa_ind]] - w1980[[i]][[pa_ind]]) + wind_e[[a_ind[[j]]]] # selects 1:90 for the first year
    
    # Pressure
    cfp2060[[1]][[i]][[j]] <- (p2060[[i]][[pw_ind]] - p1980[[i]][[pw_ind]]) + psl_e[[w_ind[[j]]]] # w_ind would then select 361:450
    cfp2060[[2]][[i]][[j]] <- (p2060[[i]][[ps_ind]] - p1980[[i]][[ps_ind]]) + psl_e[[s_ind[[j]]]] # for the second year - matching
    cfp2060[[3]][[i]][[j]] <- (p2060[[i]][[psu_ind]] - p1980[[i]][[psu_ind]]) + psl_e[[su_ind[[j]]]] # the layers on the rater brick
    cfp2060[[4]][[i]][[j]] <- (p2060[[i]][[pa_ind]] - p1980[[i]][[pa_ind]]) + psl_e[[a_ind[[j]]]]
      
  }
}

# Now I need to rejoin them as individual decades etc

#   Lists to store in

w2000 <- list()
w2020 <- list()
w2040 <- list()
w2060 <- list()

p2000 <- list()
p2020 <- list()
p2040 <- list()
p2060 <- list()


for(i in 1:12){
  w2000[[i]] <- list()
  w2020[[i]] <- list()
  w2040[[i]] <- list()
  w2060[[i]] <- list()
  
  p2000[[i]] <- list()
  p2020[[i]] <- list()
  p2040[[i]] <- list()
  p2060[[i]] <- list()
  
}


for(i in 1:length(models)){ # Now store them in lists of year by model
  
  for(j in 1:20){
    # Wind                          seas/model/year
    w2000[[i]][[j]] <- list(cfw2000[[1]][[i]][[j]],   #Winter
                            cfw2000[[2]][[i]][[j]],   #Spring
                            cfw2000[[3]][[i]][[j]],   #Summer
                            cfw2000[[4]][[i]][[j]])   #Autumn
    # Pressure
    p2000[[i]][[j]] <- list(cfp2000[[1]][[i]][[j]],   #Winter
                            cfp2000[[2]][[i]][[j]],   #Spring
                            cfp2000[[3]][[i]][[j]],   #Summer
                            cfp2000[[4]][[i]][[j]])   #Autumn
    
    # Wind                          seas/model/year
    w2020[[i]][[j]] <- list(cfw2020[[1]][[i]][[j]],   #Winter
                            cfw2020[[2]][[i]][[j]],   #Spring
                            cfw2020[[3]][[i]][[j]],   #Summer
                            cfw2020[[4]][[i]][[j]])   #Autumn
    
    # Pressure
    p2020[[i]][[j]] <- list(cfp2020[[1]][[i]][[j]],   #Winter
                            cfp2020[[2]][[i]][[j]],   #Spring
                            cfp2020[[3]][[i]][[j]],   #Summer
                            cfp2020[[4]][[i]][[j]])   #Autumn
    
    # Wind                          seas/model/year
    w2040[[i]][[j]] <- list(cfw2040[[1]][[i]][[j]],   #Winter
                            cfw2040[[2]][[i]][[j]],   #Spring
                            cfw2040[[3]][[i]][[j]],   #Summer
                            cfw2040[[4]][[i]][[j]])   #Autumn
    
    # Pressure
    p2040[[i]][[j]] <- list(cfp2040[[1]][[i]][[j]],   #Winter
                            cfp2040[[2]][[i]][[j]],   #Spring
                            cfp2040[[3]][[i]][[j]],   #Summer
                            cfp2040[[4]][[i]][[j]])   #Autumn
    
    # Wind                          seas/model/year
    w2060[[i]][[j]] <- list(cfw2060[[1]][[i]][[j]],   #Winter
                            cfw2060[[2]][[i]][[j]],   #Spring
                            cfw2060[[3]][[i]][[j]],   #Summer
                            cfw2060[[4]][[i]][[j]])   #Autumn
    
    # Pressure
    p2060[[i]][[j]] <- list(cfp2060[[1]][[i]][[j]],   #Winter
                            cfp2060[[2]][[i]][[j]],   #Spring
                            cfp2060[[3]][[i]][[j]],   #Summer
                            cfp2060[[4]][[i]][[j]])   #Autumn
  }
}


#   Now remove obsolete lists

rm(cfp2000, cfp2020, cfp2040, cfp2060,
   cfw2000, cfw2020, cfw2040, cfw2060)

#   Now collapse lists and form bricks

for(i in 1:12){
  
  
  w2000[[i]] <- unlist(w2000[[i]])
  p2000[[i]] <- unlist(p2000[[i]])
  w2020[[i]] <- unlist(w2020[[i]])
  p2020[[i]] <- unlist(p2020[[i]])
  w2040[[i]] <- unlist(w2040[[i]])
  p2040[[i]] <- unlist(p2040[[i]])
  w2060[[i]] <- unlist(w2060[[i]])
  p2060[[i]] <- unlist(p2060[[i]])
  
  print(paste0("Forming bricks for 2000-2020 for model ", i))
  
  w2000[[i]] <- brick(w2000[[i]])
  p2000[[i]] <- brick(p2000[[i]])

  print(paste0("Forming bricks for 2020-2040 for model ", i))  
  
  w2020[[i]] <- brick(w2020[[i]])
  p2020[[i]] <- brick(p2020[[i]])
  
  print(paste0("Forming bricks for 2040-2060 for model ", i))
  
  w2040[[i]] <- brick(w2040[[i]])
  p2040[[i]] <- brick(p2040[[i]])
  
  print(paste0("Forming bricks for 2060-2080 for model ", i))
  
  w2060[[i]] <- brick(w2060[[i]])
  p2060[[i]] <- brick(p2060[[i]])

}

#   Pressures values are too high - divide by ten to correct

for(i in 1:12){
  
  print(i)
  
  p2000[[i]] <- p2000[[i]] / 10
  p2020[[i]] <- p2020[[i]] / 10
  p2040[[i]] <- p2040[[i]] / 10
  p2060[[i]] <- p2060[[i]] / 10
  
}

#   Wind values may occasionally be below zero due to change factors. Need to 
#   set the minimum value as zero.

for(i in 1:12){
  
  print(paste0("Working on 2000-2020 for model ", i ))
  w2000[[i]] <- clamp(w2000[[i]], lower = 0, useValues = T)
  print(paste0("Working on 2020-2040 for model ", i ))
  w2020[[i]] <- clamp(w2000[[i]], lower = 0, useValues = T)
  print(paste0("Working on 2040-2060 for model ", i ))
  w2040[[i]] <- clamp(w2000[[i]], lower = 0, useValues = T)
  print(paste0("Working on 2060-2080 for model ", i ))
  w2060[[i]] <- clamp(w2000[[i]], lower = 0, useValues = T)
  
}

#   Write to disk 

for(i in 1:12){
  
  print(i)
  
  #   Wind
  writeRaster(w2000[[i]], paste0("UKCP18/seasonal/change_factors/sfcWind/", models[i],
                                 "/cf_sfcWind_", models[i], "_2000-2020.nc"),overwrite = T)
  writeRaster(w2020[[i]],paste0("UKCP18/seasonal/change_factors/sfcWind/", models[i],
                                "/cf_sfcWind_", models[i], "_2020-2040.nc"),overwrite = T)
  writeRaster(w2040[[i]],paste0("UKCP18/seasonal/change_factors/sfcWind/", models[i],
                                "/cf_sfcWind_", models[i], "_2040-2060.nc"),overwrite = T)
  writeRaster(w2060[[i]],paste0("UKCP18/seasonal/change_factors/sfcWind/", models[i],
                                "/cf_sfcWind_", models[i], "_2060-2080.nc"),overwrite = T)
  
  #   Pressure
  #writeRaster(p2000[[i]], paste0("UKCP18/seasonal/change_factors/pressure/", models[i],
   #                              "/cf_pressure_", models[i], "_2000-2020.nc"))
  #writeRaster(p2020[[i]],paste0("UKCP18/seasonal/change_factors/pressure/", models[i],
     #                           "/cf_pressure_", models[i], "_2020-2040.nc"))
  #writeRaster(p2040[[i]],paste0("UKCP18/seasonal/change_factors/pressure/", models[i],
    #                            "/cf_pressure_", models[i], "_2040-2060.nc"))
  #writeRaster(p2060[[i]],paste0("UKCP18/seasonal/change_factors/pressure/", models[i],
  #                              "/cf_pressure_", models[i], "_2060-2080.nc"))

}

#   Remove unneeded

rm(p1980, p2000, p2020, p2040, p2060, check, wind_e, psl_e, psl)

#-------------------------------------------------------------------------------
#                       Calculating significant wave
#-------------------------------------------------------------------------------


#   Read in data

#   Fetch
day_fetch <- brick("era5/processed/daily_mean/era5_processed_fetch_500.nc")

#   Bathymetry
bth <- raster("wave/bathymetry/gebco_2022.tif")
bth <- projectRaster(from = bth, to = w2020[[1]])    # Reproject to OSGB
bth <- resample(bth, w2020[[1]])                     # Resample to res of UKCP18
bth <- abs(bth)                                  # Remove negatives


#   Define function to calculate significant wave 
#   With depth Bretschneider's method

swd <- function(ws, fetch){
  
  (ws^2 / 9.80665) * 0.283 * tanh(0.53 * (( 9.80665 * bth) / ws^2)^0.75) * 
    
    tanh(0.0125 * ((9.80665 * fetch) / ws^2)^0.42 / tanh(0.530 * ((9.80665 * bth) / ws^2)))^0.75
  
}

wave2000 <- list()
wave2020 <- list()
wave2040 <- list()
wave2060 <- list()

for(i in 1:12){
  
  print(paste0("2000 ", i))
  
  wave2000[[i]] <- swd(w2000[[i]], day_fetch) 
  
  print(paste0("2020 ", i))
  
  wave2020[[i]] <- swd(w2020[[i]], day_fetch) 
  
  print(paste0("2040 ", i))
  
  wave2040[[i]] <- swd(w2040[[i]], day_fetch) 
  
  print(paste0("2060 ", i))
  
  wave2060[[i]] <- swd(w2060[[i]], day_fetch) 
  
}

  
#   Write to disk 

for(i in 1:12){
  
  writeRaster(wave2000[[i]], paste0("wave/ukcp18/significant_wave/change_factor/",
                                    models[i], "/cf_500_wind_wave_", models[i],"_2000-2020.nc"))
  
  writeRaster(wave2020[[i]], paste0("wave/ukcp18/significant_wave/change_factor/",
                                    models[i], "/cf_500_wind_wave_", models[i],"_2020-2040.nc"))
  
  writeRaster(wave2040[[i]], paste0("wave/ukcp18/significant_wave/change_factor/",
                                    models[i], "/cf_500_wind_wave_", models[i],"_2040-2060.nc"))
  
  writeRaster(wave2060[[i]], paste0("wave/ukcp18/significant_wave/change_factor/",
                                    models[i], "/cf_500_wind_wave_", models[i],"_2060-2080.nc"))
  
  
}




