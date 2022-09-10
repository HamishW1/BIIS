#===============================================================================
#                             Storm Surge
#===============================================================================

#   In this script we will combine the effects on sea level from tide, pressure
#   significant wave, and sea level rise.

#   Methodology for this will be pretty simple - we will simply add all the 
#   data together, and name that increase in storm surge on a given day.

#   In future it would be good to intergrate more specfic bathymetry etc 
#   and actually calculate overtopping rates - but at this point I am a bit
#   strapped for time, so this will have to do!

#-------------------------------------------------------------------------------
#                           Establish Environment
#-------------------------------------------------------------------------------

library(raster)
library(ncdf4)
library(sf)
library(tidyverse)
library(data.table)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#   Read in datasets

# Tide - in 20 year bricks

tide <- brick("tides/final/idw_tide_ire.nc") # Tide including ireland
t <- list()
for(i in 1:20){ t[[i]] <- tide}
tide <- brick(t)

UKtide <-  brick("tides/final/UKCP18_newlyn_val.nc") # Just UK tide in newlyn datum
t <- list()
for(i in 1:20){ t[[i]] <- UKtide}
UKtide <- brick(t)

#   Mean high water (so we can see dif in MHW)

mhw <- raster("tides/final/mean_tide_ire.tif") # All UK 

# Sea level

slr <- brick("UKCP18/sea_level/processed/sl_anom_2007-2100.nc") 

#   Read in surge data

models <- dir("surge")
fn1 <- strsplit(dir("surge/01"), split = "_01_")[[1]][1]
fn2 <- vector()
for(i in 1:4){fn2[i] <- strsplit(dir("surge/01"), split = "_01_")[[i]][2]}

#   Store
surge <- list()
for(i in 1:length(models)){ surge[[i]] <- list() }

#   Read in
for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    print(paste0("model ", i , " time ", j))
    surge[[i]][[j]] <- brick(paste0("surge/", models[i], "/",
                                  fn1, "_", models[i], "_", fn2[j]))

  }
}


#  Read in Significant wave data

fn1 <- strsplit(dir("wave/ukcp18/significant_wave/combined/01"), split = "_01_")[[1]][1]
fn2 <- vector()
for(i in 1:4){fn2[i] <- strsplit(dir("wave/ukcp18/significant_wave/combined/01"), split = "_01_")[[i]][2]}

# Store
wave <- list()
for(i in 1:length(models)){ wave[[i]] <- list() }

#   Read in
for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    print(paste0("model ", i , " time ", j))
    wave[[i]][[j]] <- brick(paste0("wave/ukcp18/significant_wave/combined/", models[i], "/",
                                  fn1, "_", models[i], "_", fn2[j]))
  }
}

#-------------------------------------------------------------------------------
#                       Extreme moving water (emw)
#-------------------------------------------------------------------------------

#   Here we combine with surge, wind wave, and tide, before writing to disk as 
#   a record separate from sea level rise

emw <- list()
for(i in 1:12){ emw[[i]] <- list()}

for(i in 1:12){
  for(j in 1:4){
    
    print(paste0("Combining wave surge and tide for model ", i, " time ", j))
    emw[[i]][[j]] <- wave[[i]][[j]] + surge[[i]][[j]] + tide
    
    #   Now adjust to give difference above mhw
    
    emw[[i]][[j]] <- emw[[i]][[j]] - mhw
    
    writeRaster(emw[[i]][[j]], paste0("extreme_water/extreme_moving_water/no_slr/",
                                      models[i], "/", "emw_no_slr_", models[i], "_",
                                      fn2[j]), overwrite = T)
    
  }
}


#-------------------------------------------------------------------------------
#                       Extreme still water (esw)
#-------------------------------------------------------------------------------

#   Here we combine surge and tide, before writing to disk as 
#   a record separate from sea level rise

for(i in 1:12){
  for(j in 1:4){
    
    print(paste0("Combining surge and tide for model ", i, " time ", j))
    esw[[i]][[j]] <- surge[[i]][[j]] + tide
    
    #   Now adjust to give difference above mhw
    
    esw[[i]][[j]] <- esw[[i]][[j]] - mhw
    
    writeRaster(esw[[i]][[j]], paste0("extreme_water/extreme_still_water/no_slr/",
                                      models[i], "/", "esw_no_slr_", models[i], "_",
                                      fn2[j]), overwrite = T)
    
  }
}

#   Now calculate extreme still water for newlyn, so we can compare directly
#   like for like with UKCP18

mwh_uk <- mean(UKtide)

nesw <- list()
for(i in 1:12){ nesw[[i]] <- list() }

for(i in 1:12){
  for(j in 1:4){
    
    print(paste0("Combining surge and tide for model ", i, " time ", j))
    nesw[[i]][[j]] <- surge[[i]][[j]] + UKtide
    nesw[[i]][[j]] <- nesw[[i]][[j]] - mhw_uk
    writeRaster(nesw[[i]][[j]], paste0("extreme_water/newlyn/no_slr/",
                                      models[i], "/", "esw_newlyn_", models[i], "_",
                                      fn2[j]), overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                         Combine with slr 
#-------------------------------------------------------------------------------

#   Now we want to fit to the SLR data we have - this starts in 2007 and runs to
#   2100, so we need to ditch the first 4 years and the last 20

slr <- slr[[5:74]]

#   Now I need to cut the first ten years of surge, as this is not represented
#   in the slr record.

for(i in 1:12){
  
  print(i)
  emw[[i]][[1]] <- emw[[i]][[1]][[3601:7200]]
  esw[[i]][[1]] <- esw[[i]][[1]][[3601:7200]]
  nesw[[i]][[1]] <- nesw[[i]][[1]][[3601:7200]]
  
}

x = 1:3600
n = 10
indices <- split(x, sort(x%%n))

emw1 <- list()
esw1 <- list()
nesw1 <- list()

for(i in 1:length(models)){
  
  emw1[[i]] <- list()
  esw1[[i]] <- list()
  nesw1[[i]] <- list()
  
}

for(i in 1:length(models)){
    
    emw1[[i]][[1]] <- list()
    esw1[[i]][[1]] <- list()
    nesw1[[i]][[1]] <- list()
    
}

for(i in 1:length(models)){
  for(j in 1:length(indices)){ # Running here for the first 10 years 
     
      print(paste0("Adding slr for model ", i, " year ", j, " for 2010-2020.."))
      
      emw1[[i]][[1]][[j]] <- emw[[i]][[1]][[indices[[j]]]] + slr[[j]]
      esw1[[i]][[1]][[j]] <- esw[[i]][[1]][[indices[[j]]]] + slr[[j]]
      nesw1[[i]][[1]][[j]] <- nesw[[i]][[1]][[indices[[j]]]]  + slr[[j]]
      
  }
}

#   Now we recombine the year intervals back into 10 year bricks

for(i in 1:length(models)){
  
  print(paste0("Recombining into decades for model ", i, ".."))
  
  emw1[[i]][[1]] <- brick(emw1[[i]][[1]])
  esw1[[i]][[1]] <- brick(esw1[[i]][[1]])
  nesw1[[i]][[1]] <- brick(nesw1[[i]][[1]])
  
}

#   Now replace the orginal dataset with the new sl added ones we have just 
#   generated 

for(i in 1:length(models)){
  
  emw[[i]][[1]] <- emw1[[i]][[1]]
  esw[[i]][[1]] <- esw1[[i]][[1]]
  nesw[[i]][[1]] <- nesw1[[i]][[1]]
  
}

#   And now we run again with the 20 year rasterbricks 

x = 1:7200
n = 20
indices <- split(x, sort(x%%n))

emw1 <- list()
esw1 <- list()
nesw1 <- list()

for(i in 1:length(models)){
  
  emw1[[i]] <- list()
  esw1[[i]] <- list()
  nesw1[[i]] <- list()
  
}

for(i in 1:length(models)){
  for(j in 1:3){
  
    emw1[[i]][[j]] <- list()
    esw1[[i]][[j]] <- list()
    nesw1[[i]][[j]] <- list()
    
  } 
}

#   Split sea level rise into 20 year slots to run against the 20 year 
#   decades 

slr20 <- slr[[11:30]]
slr40 <- slr[[31:50]]
slr60 <- slr[[51:70]]

slr <- list(slr20, slr40, slr60)

for(i in 1:length(models)){
  for(j in 1:length(indices)){ # Running here for 20 year blocks for 2020-2080
    
    print(paste0("Adding slr for model ", i, " year ", j, ".."))
    
    #   For 2020 - 2040
    emw1[[i]][[1]][[j]] <- emw[[i]][[2]][[indices[[j]]]] + slr[[1]][[j]]
    esw1[[i]][[1]][[j]] <- esw[[i]][[2]][[indices[[j]]]] + slr[[1]][[j]]
    nesw1[[i]][[1]][[j]] <- nesw[[i]][[2]][[indices[[j]]]]  + slr[[1]][[j]]
    
    #   For 2040 - 2060
    emw1[[i]][[2]][[j]] <- emw[[i]][[3]][[indices[[j]]]] + slr[[2]][[j]]
    esw1[[i]][[2]][[j]] <- esw[[i]][[3]][[indices[[j]]]] + slr[[2]][[j]]
    nesw1[[i]][[2]][[j]] <- nesw[[i]][[3]][[indices[[j]]]]  + slr[[2]][[j]]
    
    #   For 2060 - 2080
    emw1[[i]][[3]][[j]] <- emw[[i]][[4]][[indices[[j]]]] + slr[[3]][[j]]
    esw1[[i]][[3]][[j]] <- esw[[i]][[4]][[indices[[j]]]] + slr[[3]][[j]]
    nesw1[[i]][[3]][[j]] <- nesw[[i]][[4]][[indices[[j]]]]  + slr[[3]][[j]]
    
  }
}

#   Now we recombine the year intervals into 20 year bricks

for(i in 1:length(models)){
  for(j in 1:3){
   
    print(paste0("Recombining into decades for model ", i, " bi-decade ", j, ".."))
    
    emw1[[i]][[j]] <- brick(emw1[[i]][[j]])
    esw1[[i]][[j]] <- brick(esw1[[i]][[j]])
    nesw1[[i]][[j]] <- brick(nesw1[[i]][[j]]) 
    
  }
}

#   Replace the original none slr bricks with sl added bricks

for(i in 1:length(models)){
  
  emw[[i]][[2]] <- emw1[[i]][[1]]
  esw[[i]][[2]] <- esw1[[i]][[1]]
  nesw[[i]][[2]] <- nesw1[[i]][[1]]
  
  emw[[i]][[3]] <- emw1[[i]][[2]]
  esw[[i]][[3]] <- esw1[[i]][[2]]
  nesw[[i]][[3]] <- nesw1[[i]][[2]]
  
  emw[[i]][[4]] <- emw1[[i]][[3]]
  esw[[i]][[4]] <- esw1[[i]][[3]]
  nesw[[i]][[4]] <- nesw1[[i]][[3]]
  
}

#   And now we write to disk 

fn2[1] <- "2010-2020.nc"

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    print(paste0("Writing data for model ", i, " ", fn2[j]))
    
    writeRaster(emw[[i]][[j]], paste0("extreme_water/extreme_moving_water/with_slr/",
                                      models[i], "/", "emw_with_slr_", models[i], "_",
                                      fn2[j]), overwrite = T)
    
    writeRaster(esw[[i]][[j]], paste0("extreme_water/extreme_still_water/with_slr/",
                                      models[i], "/", "esw_with_slr_", models[i], "_",
                                      fn2[j]), overwrite = T)
    
    writeRaster(nesw[[i]][[j]], paste0("extreme_water/newlyn/with_slr/",
                                       models[i], "/", "esw_newlyn_with_slr_", models[i], "_",
                                       fn2[j]), overwrite = T)
    
    
  }
}







