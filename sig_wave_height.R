#===============================================================================
#                     Calculating Significant Wave Height
#===============================================================================

#   In this script I will calculate significant wave height using wind speed,
#   wind direction (fetch), and depth.

#   Theory as is follows:

#     1. Wave height is a product of wind 
#     2. The stronger the wind the larger the waves
#     3. The longer the distance the wind blows over the waves (fetch) the
#        larger the waves
#     4. The deeper the water, the less friction, the larger the waves.

#   This theory is translated into hard data using Bretschneider's method
#   which includes depth.

#   Methods for producing the data to feed in the model are as follows:

#     1. Utilize UKCP18 mean wind speed to provide the wind speed input.
#     2. Use UKCP18 wind direction to inform the selection of the fetch
#        input.

#     3. Produce centeroids for each raster cell and calculate fetch from
#        the cardinal points (NE, NW, SW, SE). I.e. distance from coast for
#        each direction.

#     4. With fetch for each direction calculated, we then use the direction
#        from the wind dataset to select the fetch for input.

#     5. Depth is provided by a UKHP bathymetry dataset.
#     6. Gravity is taken as a constant at 9.80665

#-------------------------------------------------------------------------------
#                         Establishing Environment
#-------------------------------------------------------------------------------

#   Read in libaries

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(waver)
library(data.table)
library(terra)

#   Set working directry

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave")

#   Read in the coastlines for fetch calculation 

cst <- st_read("ne_50m_coastline/clip_cst.gpkg")

cst <- cst$geom

#   Read in bathymetry for Bretschneiders method

bth <- raster("bathymetry/gebco_2022.tif")

#   UKCP18 wind data - for template and calculations (only selecting top layer)

template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind/east/01/uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")


#-------------------------------------------------------------------------------
#                 Definining Bretschneider's methods as functions
#-------------------------------------------------------------------------------

#   Without depth Bretschneider's method

#   ws = windspeed in meters per second
#   grav = gravity as 9.80665
#   fetch = fetch in meters
#   depth = depth depth in meters

sw <- function(ws, grav, fetch){
  
  (ws^2 / grav) * 0.283 * tanh(0.0125 * (( grav * fetch) / ws^2))^0.42 
  
}


#   With depth Bretschneider's method

swd <- function(ws, fetch){
  
  (ws^2 / 9.80665) * 0.283 * tanh(0.53 * (( 9.80665 * bth) / ws^2)^0.75) * 
    
    tanh(0.0125 * ((9.80665 * fetch) / ws^2)^0.42 / tanh(0.530 * ((9.80665 * bth) / ws^2)))^0.75
          
}

#   Example - note that bathymetery will require inverting
t <- swd(ws = 45, fetch = 6761569)
plot(t)

#   At this point I think it may be worth using both methods and then testing 
#   them against the validation dataset. However, to save time I will first
#   try with the depth version.

#-------------------------------------------------------------------------------
#                      Calculating fetch distances
#-------------------------------------------------------------------------------

#   I need a simplified polygon of the worlds coasts and points for each
#   raster cell.

plot(cst)

#   Convert to spatial polygons

cst <- as_Spatial(cst)

#   Create points from template raster

pts <- rasterToPoints(template, spatial = T) #Convert raster to pts
pts <- spTransform(pts, crs(cst))            #Transform to lat lon

#   Check

plot(cst)
plot(pts, add = T)

#   Split points into manageable sizes for fetch calculation (also allows me
#   to track progress)

pts <- st_as_sf(pts)

split.pts <- list()

split.pts[[1]] <- pts[1:1148,]
split.pts[[2]] <- pts[1149:2296,]
split.pts[[3]] <- pts[2297:3444,]
split.pts[[4]] <- pts[3445:4592,]
split.pts[[5]] <- pts[4593:5740,]
split.pts[[6]] <- pts[5741:6888,]
split.pts[[7]] <- pts[6889:8036,]
split.pts[[8]] <- pts[8037:9184,]

#   Convert back to spatial dataframe

for(i in 1:8){
  
  split.pts[[i]] <- as_Spatial(split.pts[[i]])
  
}


#   Calculate fetch for all points on the cardinal and intercardinal points
#   of the compass. (more now - doing 16 points)

fetch <- list()

started.at = proc.time()

for(i in 1:8){
  
  print(paste0("Caclualting fetch for split points ", i, ".."))
  
  fetch[[i]] <- fetch_len_multi(split.pts[[i]], 
                                bearings = c(0,23,45,68,90,113,135,158,180,
                                             203,225,248,270,293,315,338), 
                                shoreline = cst, 
                                dmax = 250000,   # Orginal dmax was 8999999
                                method =  "clip")
  
  
}

cat("Finished in",timetaken(started.at),"\n")

#   Sleep to avoid crash on restart.

gc(full = T)

#   Restart R to clear memory and retain variables

#.rs.restartR()

#   Bind fetch back onto pts

for(i in 1:8){
  
  split.pts[[i]] <- cbind(split.pts[[i]], fetch[[i]])
  
  #   Convert to sf
  
  split.pts[[i]] <- st_as_sf(split.pts[[i]])
  
}

#   Collapse into one dataframe

pts <- bind_rows(split.pts)

#   Write out for safe keeping

st_write(pts, "wind/fetch_250.gpkg", overwrite = T)

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

#   Remove uneeded rasters

rm(west, east)
gc(full = T)

#   Convert points back to raster

#   Convert crs

pts <- st_transform(pts, crs = crs(template))

#   First create a list of pts values (each corresponding to a raster)

pts_list <- list()

for(i in 2:17){
  
  pts_list[[i]] <- pts[,i]
  
}

pts_list[[1]] <- NULL

#   Now rasterise

fetch <- list()

for(i in 1:length(pts_list)){
  
  mid <- pts_list[[i]]
  
  mid[is.na(mid)] <- 0
  
  namer <- colnames(mid)[1]
  
  fetch[[i]] <- rasterize(mid, template, field = namer)
  
  fetch[[i]] <- as.integer(fetch[[i]])
  
  rm(mid)
  
}

fetch <- brick(fetch)

#   So the above has produced a stack of rasters in with the fetch distance for
#   each cell for each of the specified angles.

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

check <- day_fetch

day_fetch <- check

for(j in 1:2){
  
  for(i in 1:12){
    
    print(paste0("converting model ", i, " year ", fn2[j], " to brick.."))
    
    day_fetch[[j]][[i]] <- brick(day_fetch[[j]][[i]])
    
  }
}

#   Write to disk 

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18_fetch")

years <- c("1980-1990.nc", "1990-2000.nc")

for(j in 1:2){
  
  for(i in 1:12){
   
   print(paste0("Writing UKCP18_5000_fetch_", models[i], "_", years[j], " to disk.."))
    
   writeRaster(day_fetch[[j]][[i]], 
               
               paste0(models[i],"/UKCP18_5000_fetch_", models[i], "_", years[j]))
    
  }
}


#   Check how it plots

day_fetch <- brick(day_fetch)
plot(day_fetch)

check <- dirc[[1:19]]

writeRaster(day_fetch, "fetch_dirc_test.nc")
writeRaster(check, "dirc_fetch_test.nc", overwrite = T)
writeRaster(fetch, "fetch_fetch_test.nc")

#   Remove uneeded datasets

rm(east, nrth, fetch, pts, pts_list, split.pts, cst)

gc(full = T)

#-------------------------------------------------------------------------------
#                   Calculate Significant Wave Height 
#-------------------------------------------------------------------------------

#   Read in wind speed data

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind")

wind <- brick("sfcWind/01/sfcWind_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")


#   Before we can run the data through the signifcant wave calcs we need 
#   to convert bathmetry to the same grid etc

#   Reproject bathmetry

bth <- projectRaster(from = bth, to = template)
day_fetch <- projectRaster(from = day_fetch, to = template)



#   Check
plot(template)
plot(bth, add = T)

#   Resample
bth <- resample(bth, template)

#   Check
plot(bth)

# Remove negatives

bth <- abs(bth)

#   Now run quick calculation of signifcant wave height


wave <- list()

for(i in 1:3600){
  
  print(paste0("Calcuating significant wave height for day ", i))
  
  wave[[i]] <- overlay(wind[[i]], day_fetch[[i]], fun = swd)
  
}

wave <- brick(wave)

plot(wave)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave")

#writeRaster(wave, "test_sig_wave_1990.nc", overwrite = T)

#   Okay that has worked - but we have significant hard lines caused by the 
#   fetch inputs. I will look to smooth this some how - best way is likely by
#   limiting max fetch and providiing more angles for fetch to run from.


percentiles <- calc(wave, fun = function(x){
  
  quantile(x, probs = c(0.9,0.95,1),  na.rm = T)})

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave")

writeRaster(percentiles, "test_percentiles_1990_5000.nc")
 
#   Okay - so having checked I have two obvious issues 

#   1. Fetch from 8 angles is two confining and produces overly 
#      obvious artifacts.

#   2. Signficant wave height at its top percentiles is too high (99th)
#      I suspect the issue is due to overly long fetches. I will reduce
#      these iterativly in an attempt to improve the results. 
