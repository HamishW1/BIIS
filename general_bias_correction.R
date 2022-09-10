#===============================================================================
#                         General Bias Correction
#===============================================================================

#   This script will be (hopefully..) the last effort at bias correction for the
#   UKCP18 variables in regard to the ERA5 variables.

#   To do this I will use the general method of bias correction as described in
#   Hawkins, E, Osborne, TM, Ho, CK and Challinor, AJ (2013) Calibration and bias
#   correction of climate projections for crop modelling: An idealised case study over
#   Europe. Agricultural and Forest Meteorology, 170. 19 - 31.

#   This method takes the following code form:

#   mean(era) + calc(era, fun = sd) / calc(base, fun = sd) * (wind - mean(base))

#   This translates as:

#   mean(observations) + standard_dev(observations) / standard_dev(baseline) * (projections - mean(baseline))

#   In testing this method has performed very well. Note that all supplied varaibles require the full 
#   twenty year run - so twenty years of daily obs, twenty years of daily baseline proj and twenty 
#   years of daily projections.

#   Using seasonal means etc produces anomalous values.

#-------------------------------------------------------------------------------
#                           Establish Environment
#-------------------------------------------------------------------------------

library(raster)
library(ncdf4)
library(tidyverse)
library(sf)
library(data.table)

options(scipen =999999999)
rasterOptions(memfrac = 0.7)

rasterOptions(progress = 'text',timer=TRUE) # Shows progess for raster functions

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#   Read in observations for wind and pressure

#   Establish looping vars for wind
models <- dir("UKCP18/wind/sfcWind")
fn1 <- strsplit(split = "_01_", dir("UKCP18/wind/sfcWind/01"))[[1]][1]
fn2 <- vector()

for(i in 1:5){
  fn2[i] <- strsplit(split = "_01_", dir("UKCP18/wind/sfcWind/01"))[[i]][2]
}

#   Read in wind
wind <- list()
for(i in 1:12){ wind[[i]] <- list() }
for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    wind[[i]][[j]] <- brick(paste0("UKCP18/wind/sfcWind/", models[i], "/",
                                   fn1, "_", models[i], "_", fn2[j]))
    
  }
}

#   Establish looping vars for pressure
fn1 <- strsplit(split = "_01_", dir("UKCP18/pressure/01"))[[1]][1]

for(i in 1:5){
  fn2[i] <- strsplit(split = "_01_", dir("UKCP18/pressure/01"))[[i]][2]  
}

#   Read in pressure
psl <- list()
for(i in 1:12){ psl[[i]] <- list() }
for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    print(paste(i,j))
    
    psl[[i]][[j]] <- brick(paste0("UKCP18/pressure/", models[i], "/",
                                   fn1, "_", models[i], "_", fn2[j]))
    
  }
}

#   Establish baselines
b_psl <- list()
b_wind <- list()    

for(i in 1:12){
  
  b_psl[[i]] <- psl[[i]][[1]]
  b_wind[[i]] <- wind[[i]][[1]]
  
}

#   Remove baselines from main lists

for(i in 1:12){
  
  psl[[i]][[1]] <- NULL
  wind[[i]][[1]] <- NULL
  
}


#------------------------------------------------------------------------------
#   THIS NO LONGER NEEDS RUN NOW THE DATASETS HAVE BEEN MERGED PRIOR

#   Now combine these decades into 20 year bricks to match era5 obs
       # Lists to store data in

for(i in 1:length(models)){ # Store data in 20 year bricks 
    
    print(paste0("Working on model ", i, " for pressure.."))
    
    #   Stack first
    b_psl[[i]] <- stack(psl[[i]][[1]], psl[[i]][[2]])
    b_psl[[i]] <- brick(b_psl[[i]]) # Then brick
    
    print(paste0("Working on model ", i, " for wind.."))
    
    #   As above
    b_wind[[i]] <- stack(wind[[i]][[1]], wind[[i]][[2]])
    b_wind[[i]] <- brick(b_wind[[i]]) # Then brick
    
}

for(i in 1:12){  # Remove baseline values from main list
  for(j in 1:2){
    psl[[i]][[j]] <- NULL
    wind[[i]][[j]] <- NULL
  }
}

#   Now establish 20 year bricks for the reminder

for(i in 1:12){
  for(j in 1:4){ # 20 year bricks for projections. Follows as baseline above.
    
    print(paste0("Working on model ", i, " timeslice ", j, " for pressure.."))
    #   Stack first
    psl[[i]][[j]] <- stack(psl[[i]][[seq(from=1,to=7,by=2)[j]]],
                             psl[[i]][[seq(from=2,to=8,by=2)[j]]])
    psl[[i]][[j]] <- brick(psl[[i]][[j]]) # Then brick
    
    print(paste0("Working on model ", i, " timeslice ", j, " for wind.."))
    #   As above
    wind[[i]][[j]] <- stack(wind[[i]][[seq(from=1,to=7,by=2)[j]]],
                              wind[[i]][[seq(from=2,to=8,by=2)[j]]])
    wind[[i]][[j]] <- brick(wind[[i]][[j]])
    
  }
}

for(i in 1:12){  # Remove unneeded values from the main list
  for(j in 5:8){
    psl[[i]][[j]] <- NULL
    wind[[i]][[j]] <- NULL
  }
}

#   Compress list to remove empty lists
#psl <- psl[-which(sapply(psl, is.null))]
#wind <- wind[-which(sapply(wind, is.null))]

#   Now read in observational values for use as change factors

o_psl <- brick("era5/processed/daily_mean/era5_psl_daily_1980-2000.nc")
o_wind <- brick("era5/processed/daily_mean/era5_processed_wind_speed.nc")

#-------------------------------------------------------------------------------
#                         Calculate Change Factors
#-------------------------------------------------------------------------------

#   Now calculate bias corrections as described below:

#mean(observations) + standard_dev(observations) / standard_dev(baseline) * (projections - mean(baseline))

#   Lists for data storage
bc_p <- list()
#bc_w <- list()
for(i in 1:12){
  bc_p[[i]] <- list()
#  bc_w[[i]] <- list()
}

for(i in 1:12){    # Now run and general bias corrections 
  for(j in 1:4){
    
    print(paste0("Bias correcting model ", i, " timeslice ", j, " for pressure.."))
    
    bc_p[[i]][[j]] <- mean(o_psl) + 
      calc(o_psl, fun = sd) / calc(b_psl[[i]], fun = sd) * 
      (psl[[i]][[j]] - mean(b_psl[[i]]))
    
    #print(paste0("Bias correcting model ", i, " timeslice ", j, " for wind.."))
    
    #bc_w[[i]][[j]] <- mean(o_wind) + 
    #  calc(o_wind, fun = sd) / calc(b_wind[[i]], fun = sd) * 
    #  (wind[[i]][[j]] - mean(b_wind[[i]]))
    
    
  }
}

#-------------------------------------------------------------------------------
#                             Write to disk
#-------------------------------------------------------------------------------

#   Now write the bias corrections to disk - will maintain these in the 
#   same change factor folders as previous for clarity 

years <- c("2000-2020","2020-2040","2040-2060","2060-2080")

for(i in 1:12){
  for(j in 1:4){
    
    print(paste0("Writing model ", i, " year ", j, " to disk.."))
    
    writeRaster(bc_p[[i]][[j]], paste0("UKCP18/change_factors/pressure/",
                                       models[i], "/", "bias_corrected_pressure_",
                                       models[i],"_", years[j], ".nc"), overwrite = T)
    
    #writeRaster(bc_w[[i]][[j]], paste0("UKCP18/change_factors/wind/",
    #                                   models[i], "/", "bias_corrected_sfcWind_",
    #                                   models[i],"_", years[j], ".nc"))
    
  }
}

#   Now write out the original data in its new 20 year increments 

for(i in 1:12){    # Writing out future projections
  for(j in 1:4){
    
    writeRaster(psl[[i]][[j]], paste0("UKCP18/pressure/",
                                       models[i], "/", "psl_",
                                       models[i],"_", years[j], ".nc"))
    
    writeRaster(wind[[i]][[j]], paste0("UKCP18/wind/sfcWind/",
                                       models[i], "/", "sfcWind_",
                                       models[i],"_", years[j], ".nc"))
    
  }
}

for(i in 1:12){   # Writing out baseline projections
  
  writeRaster(b_psl[[i]], paste0("UKCP18/pressure/",
                                  models[i], "/", "psl_",
                                  models[i],"_1980-2000.nc"))
  
  writeRaster(b_wind[[i]], paste0("UKCP18/pressure/",
                                 models[i], "/", "sfcWind_",
                                 models[i],"_1980-2000.nc"))
  
}


#===============================================================================
#                             ###   End   ###
#===============================================================================




