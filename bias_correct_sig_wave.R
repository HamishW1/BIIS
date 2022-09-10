#===============================================================================
#                     Bias corrected significant wave
#===============================================================================

#   This uses data from the general bias correction script, and pre generated 
#   data on fetch at 500km.

#   # UPDATE - FOLLOWING WORK ON RESULTS EXAMINING THE EFFECTS OF BIAS CORRECTION
#     IT HAS BEEN DECIDED TO USE THE RAW UKCP18 VALUES.

#   THE REST OF THE SCRIPT HAS BEEN UPDATED TO REFLECT THIS.

#-------------------------------------------------------------------------------
#                         Establishing Environment
#-------------------------------------------------------------------------------

#   Read in libraries
#--------------------

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(data.table)
library(terra)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#-------------------------------------------------------------------------------
#                               Read in data
#-------------------------------------------------------------------------------

#   wind speed
models <- dir("UKCP18/wind/sfcWind")
fn <- strsplit(dir("UKCP18/wind/sfcWind/01"), split = "_01_")[[1]][1]

fn2 <- vector()
for(i in 1:5){
  
  fn2[i] <- strsplit(dir("UKCP18/wind/sfcWind/01"), split = "_01_")[[i]][2]

}

fn2 <- fn2[-1] # Remove baseline

ws <- list()
for(i in 1:12){ ws[[i]] <- list() }

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    ws[[i]][[j]] <- brick(paste0("UKCP18/wind/sfcWind/", models[i],"/",
                                 fn, "_", models[i], "_", fn2[j]))
  }
}


#   Now direction

#   # BEEN RUN - PART OF FETCH. DOES NOT NEED RUN AGAIN

#fn <- strsplit(dir("wave/ukcp18/direction/01"), split = "_01_")[[1]][1]
#fn2 <- vector()

#for(i in 1:4){
  
#  fn2[i] <- strsplit(dir("wave/ukcp18/direction/01"), split = "_01_")[[i]][2]
  
#}

#dir <- list()
#for(i in 1:12){ dir[[i]] <- list() }

#for(i in 1:length(models)){
#  for(j in 1:length(fn2)){
    
#    dir[[i]][[j]] <- brick(paste0("wave/ukcp18/direction/", models[i],"/",
 #                                fn, "_", models[i], "_", fn2[j]))
    
#  }
#}

#   Now fetch

fn <- strsplit(dir("wave/ukcp18/fetch/01"), split = "01_")[[1]][1]

fn2 <- vector()
for(i in 1:4){
  
  fn2[i] <- strsplit(dir("wave/ukcp18/fetch/01"), split = "01_")[[i]][2]
  
}

fetch <- list()
for(i in 1:12){ fetch[[i]] <- list() }

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
      fetch[[i]][[j]] <- brick(paste0("wave/ukcp18/fetch/", models[i],"/",
                                 fn, models[i], "_", fn2[j]))
  }
}


#   Now read bathymetry

bth <- raster("wave/bathymetry/gebco_2022.tif")

bth <- clamp(bth, upper = 0, useValue = F) # Remove above sea level

bth <- projectRaster(from = bth, to = ws[[1]][[1]]) # Reproject to UKCP18

bth <- resample(bth, ws[[1]][[1]]) #   Resample to res of UKCP18

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
#                             Calculate fetch
#-------------------------------------------------------------------------------

#   THIS HAS BEEN RUN - DOES NOT NEED RUN TWICE.

#   Horrible fetch calculation method

#   Read in fetch calculated points

pts <- st_read("wave/wind/fetch_500.gpkg")

#   Convert points to raster

pts <- st_transform(pts, crs = crs(dir[[1]][[1]])) # Convert crs

pts_list <- list() # List for storage

for(i in 2:17){ # Spliting points corresponding to directions
  
  pts_list[[i]] <- pts[,i]
  
}

pts_list[[1]] <- NULL # Remove empty value

#   Now rasterise

fetch <- list()

for(i in 1:length(pts_list)){
  
  mid <- pts_list[[i]]
  
  mid[is.na(mid)] <- 0
  
  namer <- colnames(mid)[1]
  
  fetch[[i]] <- rasterize(mid, dir[[1]][[1]], field = namer)
  
  fetch[[i]] <- as.integer(fetch[[i]])
  
  rm(mid)
  
}

fetch <- brick(fetch)

#   Define raster for each fetch direction
#--------------
f1 <- fetch[[1]]
f2 <- fetch[[2]]
f3 <- fetch[[3]]
f4 <- fetch[[4]]
f5 <- fetch[[5]]
f6 <- fetch[[6]]
f7 <- fetch[[7]]
f8 <- fetch[[8]]
f9 <- fetch[[9]]
f10 <- fetch[[10]]
f11 <- fetch[[11]]
f12 <- fetch[[12]]
f13 <- fetch[[13]]
f14 <- fetch[[14]]
f15 <- fetch[[15]]
f16 <- fetch[[16]]
#--------------

#   Define calculation (reallly ugly..)

fetcher <- function(x) { overlay(x,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16, fun =   
                                   function(drct,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16) { 
                                     
                                     ifelse(drct>=349.1,f1,
      
      ifelse(drct<=12, f1,
             
             ifelse(drct>=12.1&drct<=34,f2,
                    
                    ifelse(drct>=34.1&drct<=56,f3,
                           
                           ifelse(drct>=56.1&drct<=79,f4,
                                  
                                  ifelse(drct>=79.1&drct<=101,f5,
   
   ifelse(drct>=101.1&drct<=124,f6,
          
          ifelse(drct>=124.1&drct<=146,f7,
                 
                 ifelse(drct>=146.1&drct<=169,f8,
                        
                        ifelse(drct>=169.1&drct<=191,f9,
                               
                               ifelse(drct>=191.1&drct<=214,f10,

ifelse(drct>=214.1&drct<=236,f11,
       
       ifelse(drct>=236.1&drct<=259,f12,
              
              ifelse(drct>=259.1&drct<=281,f13,
                     
                     ifelse(drct>=281.1&drct<=304,f14,
                            
                            ifelse(drct>=304.1&drct<=326,f15,
                                   
                                   ifelse(drct>=326.1&drct<=349,f16,1)))))))))))))))))
                                   } )
}

#   Caculate fetch direction

fetch <- list()
for(i in 1:12)( fetch[[i]] <- list())

for(i in 1:12){
  for(j in 1:4){
    
    print(paste0("Cacluatling fetch for model ", i ," time ", j, ".."))
    
    fetch[[i]][[j]] <- fetcher(dir[[1]][[1]])
    
    # Write to disk
    writeRaster(fetch[[i]][[j]], paste0("wave/ukcp18/fetch/",
                                        models[i],"/fetch_500", models[i], "_", fn2[j]),
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                     Calculate significant wave height
#-------------------------------------------------------------------------------

#   Now run quick (haha) calculation of significant wave height

rasterOptions(progress = "", timer = F)

#   Run calculation

wave <- list()
for(i in 1:12){ wave[[i]] <- list() }

for(i in 1:12){
  
  for(j in 1:4){
    
    print(paste0("Working on signifcant wave for model ", models[i],
                 " year ", fn2[j],  ".."))
    
    # Run equation for significant wave
    wave[[i]][[j]] <- swd(ws[[i]][[j]], fetch[[i]][[j]])
    
    print(paste0("Writing significant wave for model ", models[i], " year ", fn2[j], " to disk.."))
    
    # Write to disk
    writeRaster(wave[[i]][[j]], paste0("wave/ukcp18/significant_wave/raw/",
                                       models[i],"/UKCP18_500_wave_", models[i], "_", fn2[j]),
                overwrite = T)
    
  }
}


#===============================================================================
#                           ###   END   ###
#===============================================================================