#===============================================================================
#                           Calculating Fetch
#===============================================================================

#   Calculate fetch for each grid square and each day of a raster brick


#-------------------------------------------------------------------------------
#                         Establishing Environment
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

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave")

#   UKCP18 wind data - for template and calculations (only selecting top layer)
#------------------------------------------------------------------------------

template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind/east/01/uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

#   Read in fetch and rasterise
#------------------------------

fetch_pts <- st_read("wind/fetch_500.gpkg")

fetch_pts <- st_transform(fetch_pts, crs = crs(template)) # Convert crs

#   List for storage

pts_list <- list()

for(i in 2:17){
  
  pts_list[[i]] <- fetch_pts[,i] # Selecting each angle as a layer
  
}

pts_list[[1]] <- NULL # Removing empty field

#   Now rasterise

fetch <- list() # For storage

for(i in 1:length(pts_list)){
  
  mid <- pts_list[[i]] # Halfway dataset
  
  mid[is.na(mid)] <- 0 # Remove NAs
  
  namer <- colnames(mid)[1] # Fetch the name of degree we are working on
  
  fetch[[i]] <- rasterize(mid, template, field = namer) # Rasterise selected field
  
  fetch[[i]] <- as.integer(fetch[[i]]) # Remove decimals
  
  rm(mid) # Delete halfway dataset
  
}

fetch <- brick(fetch) # Convert to raster brick

#   Read in grid for mask 
#------------------------

grid <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/grids/12km_land_mask.gpkg")

grid <- rasterize(grid, template) # Rasterise
grid <- grid / grid               # Divide by itself to give values of 1
plot(grid)                        # Check

#   Read in bathymetry for Bretschneiders method
##----------------------------------------------

bth <- raster("bathymetry/gebco_2022.tif")

bth <- projectRaster(from = bth, to = template) # Reproject to UKCP18

bth <- resample(bth, template) #   Resample to res of UKCP18

bth <- abs(bth) #   Remove negatives 

#-------------------------------------------------------------------------------
#                 Definining Bretschneider's methods as functions
#-------------------------------------------------------------------------------

#   With depth Bretschneider's method

swd <- function(ws, fetch){
  
  (ws^2 / 9.80665) * 0.283 * tanh(0.53 * (( 9.80665 * bth) / ws^2)^0.75) * 
    
    tanh(0.0125 * ((9.80665 * fetch) / ws^2)^0.42 / tanh(0.530 * ((9.80665 * bth) / ws^2)))^0.75
  
}


#-------------------------------------------------------------------------------
#               Calculate wind direction and select correct fetch
#-------------------------------------------------------------------------------

#   Note that this all will have to be looped over all models

#   Read in wind direction datasets (need to loop this over each model)

#   First port of call is to do this for the hindcast datasets in line 
#   with era5 (1980 t0 2000)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind")

#   Create list of models

models <- dir("east/")

#   Fetch name of files

efn <- dir("east/01/")[1:2]
wfn <- dir("west/01")[1:2]

#   Split names of files to allow looping

efn1 <- strsplit(efn, "_01_")[[1]][1] # First element of east
wfn1 <- strsplit(wfn, "_01_")[[1]][1] # First element of west

fn2 <- list()

for(i in 1:2){
  
  fn2[[i]] <- strsplit(wfn, "_01_")[[i]][2] # End element of both after model
  
}

#   Example

print(paste0(efn1,models[1],fn2[1]))

#   Now read both files in to lists

east <- list()
west <- list()

for(i in 1:2){
  
  east[[i]] <- list()
  west[[i]] <- list()
  
}

for(i in 1:2){
  
  for(j in 1:length(models)){
    
    print(paste0("Reading in files from model ", models[j], " for ", fn2[i]))
    
    east[[i]][[j]] <- brick(paste0("east/", models[j], "/", efn1, "_", models[j], "_", fn2[i]))
    
    west[[i]][[j]] <- brick(paste0("west/", models[j], "/", wfn1, "_", models[j], "_", fn2[i])) 
    
  }
}

#   Convert wind speed from easterly and northerly components of wind to 
#   direction in degrees

dirc <- list()

for(i in 1:2){
  
  dirc[[i]] <- list()
  
}

for(i in 1:length(models)){
  
  for(j in 1:2){
    
    print(paste0("Converting east and north vectors to degrees for model ", models[i],
                 " and file ", fn2[j]))
    
    dirc[[j]][[i]] <- 180 + 180 * atan2(west[[j]][[i]],east[[j]][[i]]) / pi
    dirc[[j]][[i]] <- as.integer(dirc[[j]][[i]])
    
  }
}

#   Remove unneeded rasters

rm(west, east)
gc(full = T)


#   The below instructs R to assign each pixel a fetch value dependent on 
#   which section of the compass it falls into.

#   This uses the unsupported ifelse function for raster as overlay fails.
#   Has been checked and works

fetcher <- function(drct, fch){
  
  raster:::.ifel(drct >= 349.1 & drct <= 12, fch[[1]], 
                 
                 raster:::.ifel(drct >= 12.1 & drct <= 34, fch[[2]],
                                
                                raster:::.ifel(drct >= 34.1 & drct <= 56, fch[[3]],
                                               
                                               raster:::.ifel(drct >= 56.1 & drct <= 79, fch[[4]], 
                                                              
                                                              raster:::.ifel(drct >= 79.1 & drct <= 101, fch[[5]],
                                                                             
                                                                             raster:::.ifel(drct >= 101.1 & drct <= 124, fch[[6]],
                                                                                            
                                                                                            raster:::.ifel(drct >= 124.1 & drct <= 146, fch[[7]], 
                                                                                                           
                                                                                                           raster:::.ifel(drct >= 146.1 & drct <= 169, fch[[8]],
                                                                                                                          
                                                                                                                          raster:::.ifel(drct >= 169.1 & drct <= 191, fch[[9]],
                                                                                                                                         
                                                                                                                                         raster:::.ifel(drct >= 191.1 & drct <= 214, fch[[10]],
                                                                                                                                                        
                                                                                                                                                        raster:::.ifel(drct >= 214.1 & drct <= 236, fch[11],
                                                                                                                                                                       
                                                                                                                                                                       raster:::.ifel(drct >= 236.1 & drct <= 259, fch[12],
                                                                                                                                                                                      
                                                                                                                                                                                      raster:::.ifel(drct >= 259.1 & drct <= 281, fch[[13]],
                                                                                                                                                                                                     
                                                                                                                                                                                                     raster:::.ifel(drct >= 281.1 & drct <= 304, fch[[14]],
                                                                                                                                                                                                                    
                                                                                                                                                                                                                    raster:::.ifel(drct >= 304.1 & drct <= 326, fch[[15]],
                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                   raster:::.ifel(drct >= 326.1 & drct <= 349, fch[[16]],0))))))))))))))))
  
}

#   Now we loop the fetcher function over the full dirc dataset to provide
#   a calulation of fetch for each day

#   Create list of lists

day_fetch <- list()

for(i in 1:2){
  
  day_fetch[[i]] <- list()
  
}

for(j in 1:2){
  
  for(i in 1:length(models)){
    
    day_fetch[[j]][[i]] <- list()
    
  } 
}

#   Now calculate fetch over all models

for(j in 1:2){
  
  for(i in 1:length(models)){
    
    for(z in 1:3600){
      
      print(paste0("Working on fetch for day ", z, " model ", models[i],
                   " year ", fn2[j],  ".."))
      
      day_fetch[[j]][[i]][[z]] <- fetcher(dirc[[j]][[i]][[z]], fetch)
      
    }
  }
}

#   Convert to brick

for(j in 1:2){
  
  for(i in 1:12){
    
    print(paste0("converting model ", i, " year ", fn2[j], " to brick.."))
    
    day_fetch[[j]][[i]] <- brick(day_fetch[[j]][[i]])
    
  }
}

#   Write to disk 

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18/fetch/fetch_250")

years <- c("1980-1990.nc", "1990-2000.nc")

for(j in 1:2){
  
  for(i in 1:12){
    
    print(paste0("Writing UKCP18_500_fetch_", models[i], "_", years[j], " to disk.."))
    
    writeRaster(day_fetch[[j]][[i]], 
                
                paste0(models[i],"/UKCP18_500_fetch_", models[i], "_", years[j]))
    
  }
}