#===============================================================================
#                   Calculating Extreme Still Water
#===============================================================================

#   In this script we will calculate extreme still water - essentially surge
#   with wave effects removed.

#   Extreme still water (ESW) is defined as the combination of dynamic effects of 
#   surge with the static.

#   Static amplification (SA) is produced through the inverse barometer model, in 
#   which a  1 mb change in pressure from standard pressure (1013mb) results in 
#   a change in sea level of 1cm.

#   Dynamic amplifcation (DA) is a product of wind and depth

#   The below equation produces a 1 dimensional representation of surge. This 
#   will be directly comparable to UKCP18

# h = surge (m)
# x = fetch (m)
# wvel = wind speed (m per second)
# MSL = depth (m)

# c = y-intercept of free surface slope (zero default)
# wStr = wind stress coefficent - default is 2 * 10^-3
# pAir = Density of Air. Default as 1.225
# pSW = Density of (Sea) Water. Default as 1025
# g = Acceleration due to Gravity. Default as 9.80665
# SA is water height difference as a result of the inverse barometer model

#h  = X * (pAir/pSW) * ((wStr * (wVel^2)) / (g*(MSL + SA))) + C

#   As an example: 

# Fetch   PAir    pSW      WStr     WVel      Gravity   Bth  SA     Slope 
500000 * (1.225 / 1025) * ((10^-3 * (14^2)) / (9.80665 * (5 + 0.1))) + 0

#   The results can then be directly compared to the UKCP18 projections. They 
#   will probably be worse due to coarse res, and no assumption of drag 
#   factors, but will be serviceable.

#-------------------------------------------------------------------------------
#                           Establish Environment 
#-------------------------------------------------------------------------------

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

library(raster)
library(tidyverse)
library(ncdf4)
library(data.table)
library(sf)

#   Define fixed variables

PAir <- 1.225
pSW <- 1025
WStr <- 2*10^-3
g <- 9.80665

#   Define function

surgeFun <- function(fetch, WVel, depth, SA){
  
  # Fetch   PAir    pSW      WStr     WVel      Gravity   Bth  SA     Slope 
  fetch * (PAir / pSW) * ((WStr * (WVel)) / (g * (depth + SA))) + 0
  
}

#   Read in wind speed

models <- dir("UKCP18/wind/sfcWind")
fn <- strsplit(dir("UKCP18/wind/sfcWind/01/"),split = "_01_")[[1]][1]

fn2 <- vector()
for(i in 1:5){ fn2[i] <- strsplit(dir("UKCP18/wind/sfcWind/01/"),split = "_01_")[[i]][2] }
fn2 <- fn2[-1]

ws <- list()
for(i in 1:length(models)){ ws[[i]] <- list() }

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    ws[[i]][[j]] <- brick(paste0("UKCP18/wind/sfcWind/", models[i], "/",
                                 fn, "_", models[i], "_", fn2[j]))
    
  }
}


#   Read in fetch

fn <- strsplit(dir("wave/ukcp18/fetch/01/"), split = "01_")[[1]][1]

fch <- list()
for(i in 1:length(models)){ fch[[i]] <- list() }

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    fch[[i]][[j]] <- brick(paste0("wave/ukcp18/fetch/", models[i], "/",
                                  fn, models[i], "_", fn2[j])) 
    
  }
}


#   Read in bathymetry

bth <- raster("wave/bathymetry/gebco_2022.tif")
bth <- clamp(bth, upper = 1, useValue = F)      # Set postive values to NA
bth <- projectRaster(from = bth, to = ws[[1]][[1]]) # Reproject to UKCP18
bth <- resample(bth, ws[[1]][[1]], method = "bilinear") #   Resample to res of UKCP18
bth <- abs(bth) # Remove negs

#   Read in static amplification

fn <- strsplit(dir("UKCP18/pressure_sea_level/01/"), split = "_01_")[[1]][1]

psl <- list()
for(i in 1:length(models)){ psl[[i]] <- list() }

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    psl[[i]][[j]] <- brick(paste0("UKCP18/pressure_sea_level/", models[i], "/",
                                  fn, "_", models[i], "_", fn2[j]))
    
    psl[[i]][[j]] <- psl[[i]][[j]] / 100 # Convert to m 
    
  }
}

 #-------------------------------------------------------------------------------
#                           Calculate Surge 
#-------------------------------------------------------------------------------

#   Here we will use the equation defined above to calculate surge for each 
#   model and each time period.

surge <- list()
for(i in 1:length(models)){ surge[[i]] <- list()}

for(i in 1:length(models)){
  for(j in 1:length(fn2)){
    
    print(paste0("Calculating surge for model ", i , " year ", fn2[j], ".."))
    
    surge[[i]][[j]] <- surgeFun(fetch = fch[[i]][[j]],
                                WVel = ws[[i]][[j]],
                                depth = bth,
                                SA = psl[[i]][[j]]) # Note this is deviating - see methods
    
    
    surge[[i]][[j]] <- surge[[i]][[j]] + psl[[i]][[j]] # Add on static amplification
    
    print(paste0("Writing surge for model ", i , " year ", fn2[j], " to disk.."))
    
    writeRaster(surge[[i]][[j]], paste0("surge/", models[i], "/surge_",
                                        models[i], "_", fn2[j]),
                overwrite = T)
    
  }
}


#===============================================================================
#                             ###   END   ###
#===============================================================================
