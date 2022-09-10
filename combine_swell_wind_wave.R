#===============================================================================
#                     Combining swell and wind wave
#===============================================================================

#   In this script I will first test that the significant wave module is 
#   producing results that are in line with the observed record from ERA5
#   reanalysis.

#   Should this be acceptable, I will then combine the the significant wave
#   module, the tidal module, and sea level rise from atmospheric pressure
#   to create a combined storm surge model.

#   From here I will use a resampled version of the sea level rise dataset 
#   and add it to the storm surge dataset. 

#   From there I can begin the analysis of model fit to the UKCP18 storm surge
#   dataset. Likewise, if I recall correctly UKCP18 has some local historical
#   and future case studies, so I may well be able to compare the model to these.

#-------------------------------------------------------------------------------
#                         Establish envirronment
#-------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(ncdf4)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")


#   Read in significant wave
#---------------------------

models <- dir("wave/ukcp18/significant_wave/raw/") # Fetch models

#   Fetch elements of file names for read in

fn1 <- strsplit(dir("wave/ukcp18/significant_wave/raw/01"),
                split = "_01_")[[1]][1]

fn2 <- vector()
for(i in 1:4){
  
  fn2[i] <- strsplit(dir("wave/ukcp18/significant_wave/raw/01"),
                     split = "_01_")[[i]][2]
  
}

#   Read in significant wave

wave <- list()
for(i in 1:4){ wave[[i]] <- list()}

for(i in 1:length(fn2)){
  
  for(j in 1:length(models)){
    
    wave[[i]][[j]] <- brick(paste0("wave/ukcp18/significant_wave/raw/",
                                   models[j], "/", fn1, "_", models[j], "_", fn2[i]))
    
  }
}


#   Read in swell 
#----------------

fn2 <- vector()

for(i in 1:length(dir("era5/change_factors/swell"))){
  
  fn2[i] <- strsplit(dir("era5/change_factors/swell"), split = "swell_" )[[i]][2]
  
}

swl <- list()

for(i in 1:length(fn2)){
  
  swl[[i]] <- brick(paste0("era5/change_factors/swell/swell_", fn2[i]))
  
}

#   Read in secondary swell
#--------------------------

fn2 <- vector()

for(i in 1:length(dir("era5/change_factors/secondary_swell"))){
  
  fn2[i] <- strsplit(dir("era5/change_factors/secondary_swell"), split = "secondary_swell" )[[i]][2]
  
}

swl2 <- list()

for(i in 1:length(fn2)){
  
  swl2[[i]] <- brick(paste0("era5/change_factors/secondary_swell/secondary_swell", fn2[i]))
  
}

#   Read in ERA5 comparison dataset

era5 <- brick("era5/processed/daily_mean/era5_processed_daily_mean_combi_sig_wave_2000-2020.nc")

#-------------------------------------------------------------------------------
#                       Combine swell and wind wave
#-------------------------------------------------------------------------------

#   Reclassify rasters so that NA's are considered to be zero

for(i in 1:length(swl)){ # Swell
  
  print(i)
  swl[[i]] <- reclassify(swl[[i]], cbind(NA, 0), right = FALSE)
  swl2[[i]] <- reclassify(swl2[[i]], cbind(NA, 0), right = FALSE)
  
}

for(i in 1:length(wave)){
  for(j in 1:length(models)){
   
   print(paste0("Timeslice ", i, " model ", j))
   wave[[i]][[j]] <-  reclassify(wave[[i]][[j]], cbind(NA, 0), right = FALSE)
    
  }
}

#   List to store data in

combi <- list()
for(i in 1:length(models)){ combi[[i]] <- list() }

#   Now combine

year <- c("2000-2020", "2020-2040", "2040-2060", "2060-2080")

for(i in 1:4){
  
  for(j in 1:length(models)){
    
    print(paste0("Calculatuing combined significant wave for ", year[i], " model ", j, ".."))
    
    combi[[i]][[j]] <- sqrt((wave[[i]][[j]])^2 + (swl[[i]])^2 + (swl2[[i]])^2)
    
    print(paste0("Writing year ", year[i], " model ", j, " to disk.."))
    
    writeRaster(combi[[i]][[j]], paste0("wave/ukcp18/significant_wave/combined/",
                                        models[j],"/combined_sig_wave_", models[j],
                                        "_", year[i], ".nc"),
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                       Process ERA5 comparison
#-------------------------------------------------------------------------------

#   This has since been done  no need to run it again

#   Calculate mean daily value

#x = 1:58440
#n = 7305
#indices <- split(x, sort(x%%n))

#era <- list()

#for(i in 1:7305){
  
 # era[[i]] <- mean(era5[[indices[[i]]]])
  
#}

#era <- brick(era)

#   Fit to UKCP18 calendar

#leap <- seq(from = 29, by = 1460, length.out = 5) # length.out accounts for the 5 years

#   Now we index to remove these years and convert to normal years

#era <- era[[-leap]]

#may <- seq(from = 151, to = 7300, by = 365)     # Define the days which will be
#jly <- seq(from = 211, to = 7280, by = 364)     # removed from the dataset
#aug <- seq(from = 241, to = 7260, by = 363)
#oct <- seq(from = 301, to = 7240, by = 362)
#dec <- seq(from = 361, to = 7220, by = 361)

#era <- era[[-may]]    # Now we sequentially remove the extra days
#era <- era[[-jly]]    # to leave us with 3600.
#era <- era[[-aug]]
#era <- era[[-oct]]
#era <- era[[-dec]]

#era <- brick(era)     # Convert back to brick 

#rm(era5) # Remove redundant data set

#   Now repoject and resample to UKCP18

#era <- projectRaster(era, swl[[1]], crs = swl[[1]])
#era <- resample(era, swl[[1]])

#   Write to disk

#writeRaster(era, "era5/processed/daily_mean/era5_processed_daily_mean_combi_sig_wave_2000-2020.nc")

#-------------------------------------------------------------------------------
#                       Compare era5 and model
#-------------------------------------------------------------------------------

#   Quick compare

plot(combi[[1]][[2]][[1:9]], xlab = "Model") # Using 2nd model as performs best
plot(era5[[1:9]], xlab = "Obs")

#   Check percentiles

era_p <- calc(era5, fun = function(x){ # era5
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 1),  na.rm = T)})

combi_p <- calc(combi[[1]][[2]], fun = function(x){ # model
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 1),  na.rm = T)})

plot(era_p, xlab = "Obs")
plot(combi_p, xlab = "Model")

#   Hmm okay - we are seeing an element of bias in the significant wave gen at 
#   the top percentiles - it is low. Lets find the best performing model

rmse <- list()
for(i in 1:12){ rmse[[i]] <- list() }

for(i in 1:12){
  
  print(i)
  rmse[[i]] <- sqrt(mean(era5 - combi[[1]][[i]])^2)
  
  writeRaster(rmse[[i]], paste0("C:/Users/hamis/Desktop/Uni/Dissertation/results/significant_wave/",
                                "RMSE_", models[i],"combi_sig_wave_2000-2020.nc"),
              overwrite = T)
  
}
