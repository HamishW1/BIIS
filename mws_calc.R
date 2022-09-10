#===============================================================================
#                           UKCP18 Wind Speed
#===============================================================================

#   In this script we will calculate windspeed for UKCP18 using the same 
#   formula that we used for ERA5 - that will ensure consistency

#-------------------------------------------------------------------------------
#                           Establish environment
#-------------------------------------------------------------------------------

#   Read in libraries
#--------------------

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(waver)
library(data.table)
library(terra)

#   Set working directory
#------------------------

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind")

#   Read in data

models <- dir("east") # Fetch model names

u <- list() # Lists to store in
v <- list()

for(i in 1:12){
  
  u[[i]] <- list() # Lists within lists for models
  v[[i]] <- list()
  
}

fn <- dir("east/01") # Fetch file name
fn <- strsplit(fn, split = "_01_") # Split at model
un <- fn[[1]][1] # First element of name for u

fn2 <- vector() # To store in
for(i in 1:10){
  
  fn2[i] <- fn[[i]][2] # Fetch second half of file name - years
  
}

vn <- gsub(pattern = "uas", replacement = "vas", un) # replace u for v for v name

paste0(un, "_", models[i], "_", fn2[1]) # looks like this 

#   Now read in ensemble

uas <- list() # Lists to store in
vas <- list()

for(i in 1:10){
  
  uas[[i]] <- list()
  vas[[i]] <- list()
  
}


for(i in 11:12){
  
  for(j in 1:10){
    
    print(paste0("Reading in model ", i, " for year ", fn2[j]))
    
    # Read in files for year and model
    uas[[j]][[i]] <- brick(paste0("east/", models[i], "/", un, "_", models[i], "_", fn2[j])) 
    vas[[j]][[i]] <- brick(paste0("west/", models[i], "/",vn, "_", models[i], "_", fn2[j])) 
    
  }
}

#-------------------------------------------------------------------------------
#                           Calculate windspeed
#-------------------------------------------------------------------------------


#   Calulate wind speed using the same method as that in the era5 script

ws <- list()
for(i in 1:10){ ws[[i]] <- list() }

for(i in 11:12){
  
  for(j in 1:10){
    
    print(paste0("Calculating wind speed for model ", i, " year ", fn2[j]))
    
    ws[[j]][[i]] <- sqrt(uas[[j]][[i]]^2 + vas[[j]][[i]]^2)
    
  }
}

#   Write to disk 

wn <- gsub(pattern = "uas", replacement = "sfcWind", fn[[1]][[1]])

for(i in 11:12){
  
  for(j in 1:10){
    
    print(paste0("Writing mws for model ", i, " for year ", fn2[j]))
    
    writeRaster(ws[[j]][[i]], paste0("sfcWind/",models[i], "/", wn, "_", models[i], "_", fn2[j])) 
    
  }
}












