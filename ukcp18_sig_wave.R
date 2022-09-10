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
#                         Calculate wind direction
#-------------------------------------------------------------------------------

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind")

#   Create list of models

models <- dir("east/")

#   Fetch name of files

efn <- dir("east/01/")[-1:-2]
wfn <- dir("01")[-1:-2]

#   Split names of files to allow looping

efn1 <- strsplit(efn, "_01_")[[1]][1] # First element of east
wfn1 <- strsplit(wfn, "_01_")[[1]][1] # First element of west

fn2 <- vector()

for(i in 1:8){

  fn2[[i]] <- strsplit(efn, "_01_")[[i]][2] # End element of both after model
  
}

#   Example

print(paste0(efn1,models[1],fn2[1]))

#   Calculate fetch

east <- list()
west <- list()

for(i in 1:8){
  
  east[[i]] <- list()
  west[[i]] <- list()
  
}

for(i in 1:8){
  
  for(j in 1:length(models)){
    
    print(paste0("Reading in files from model ", models[j], " for ", fn2[i]))
    
    east[[i]][[j]] <- brick(paste0("east/", models[j], "/", efn1, "_", models[j], "_", fn2[i]))
    
    west[[i]][[j]] <- brick(paste0("west/", models[j], "/", wfn1, "_", models[j], "_", fn2[i])) 
    
  }
}

#   Convert wind speed from easterly and northerly components of wind to 
#   direction in degrees

dirc <- list()

for(i in 1:8){
  
  dirc[[i]] <- list()
  
}

for(i in 1:length(models)){
  
  for(j in 1:8){
    
    
    print(paste0("Converting east and north vectors to degrees for model ", models[i],
                 " and file ", fn2[j]))
    
    dirc[[j]][[i]] <- 180 + 180 * atan2(west[[j]][[i]],east[[j]][[i]]) / pi
    dirc[[j]][[i]] <- as.integer(dirc[[j]][[i]])
    
  }
}

#   Remove unneeded rasters

rm(west, east)
gc(full = T)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18")

#   Write to disk 

for(i in 1:8){
  
  for(j in 1:12){
    
    
    print(paste0("Writing model ", models[j],
                 " and file ", fn2[i], " to disk.."))
    
    writeRaster(dirc[[i]][[j]], paste0("direction/", models[j], "/direction_",
                                       models[j], "_", fn2[i]))
    
  }
}

#   Read back in as brick

for(i in 1:8){
  
  for(j in 1:12){
    
    
    print(paste0("Reading model ", models[j],
                 " and file ", fn2[i], " from disk.."))
    
    dirc[[i]][[j]] <- brick(paste0("direction/", models[j], "/direction_",
                                     models[j], "_", fn2[i]))
    
  }
}



rm(efn, efn1,fn,fn1,wfn,wfn1)

#-------------------------------------------------------------------------------
#                           Calculate fetch
#-------------------------------------------------------------------------------

#   Read in fetch calculated for 500 kms

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave")

pts <- st_read("wind/fetch_500.gpkg")

#   Convert pointsto raster

pts <- st_transform(pts, crs = crs(template)) # Convert crs

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

#   Now calculate fetch over all models

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18")

for(j in 1:8){
  
  for(i in 1:length(models)){
    
    #   To avoid memory issues this forces r to write to the below folder
    
    dir.create("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster", showWarnings = F)
    rasterOptions(tmpdir=file.path("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster"))
    
    
    print(paste0("Working on fetch for model ", models[i],
                   " year ", fn2[j],  ".."))
      
    dirc[[j]][[i]] <- fetcher(dirc[[j]][[i]], fetch) # This overwriting saves RAM
    
    
    #   We then write to disk as part of the loop so we can unlink temp files 
    #   afterwards
    
    print(paste0("Writing fetch for model ", i, " year ", fn2[j], " to disk.."))
    
    writeRaster(dirc[[j]][[i]], paste0("fetch/", models[i], "/fetch_500_", models[i], "_", fn2[j]),
                overwrite = T)
    
    #   Delete temp files at end of loop to retain space on hard drive space
    
    
    
    unlink(file.path("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster"), recursive = TRUE,
           force = T)
      
  }
}

#   Remove unneeded data

rm(dirc)
gc(full = T)

#   Read fetch back in

day_fetch <- list()

for(i in 1:8){
  
  day_fetch[[i]] <- list()

}

fn <- 


for(i in 1:8){
  
  for(j in 1:length(models)){
    
    print(paste0("Reading fetch for model ", j, " year ", fn2[i], " from disk.."))
    
    day_fetch[[i]][[j]] <-  brick(paste0("fetch/", models[j], 
                                          "/fetch_500_", models[j], "_", fn2[i]))
    
    
  }
}


#-------------------------------------------------------------------------------
#                                 Read in mws
#-------------------------------------------------------------------------------

#   Read in wind speed data

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/change_factors")

#   Create list of models

models <- dir("wind/")

#   Fetch name of files

fn <- dir("wind/01") # Fetch file name
fn <- strsplit(fn, split = "_01_") # Split at model
fn1 <- fn[[1]][1] # First element of name for 

fn2 <- vector() # To store in
for(i in 1:8){
  
  fn2[i] <- fn[[i]][2] # Fetch second half of file name - years
  
}
#   Example

print(paste0(fn1, "_", models[1],"_", fn2[1]))

#   Now read both files in to lists

ws <- list()

for(i in 1:8){ ws[[i]] <- list() }

for(i in 1:8){
  
  for(j in 1:length(models)){
    
    print(paste0("Reading in files from model ", models[j], " for ", fn2[i]))
    
    ws[[i]][[j]] <- brick(paste0("wind/", models[j], "/", fn1, "_", models[j], "_", fn2[i]))
    
  }
}

#-------------------------------------------------------------------------------
#                     Calculate significant wave height
#-------------------------------------------------------------------------------

#   Now run quick (haha) calculation of significant wave height

#   Run calculation

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18/significant_wave/500")

for(i in 1:8){
  
  for(j in 1:length(models)){
    
    #   To avoid memory issues this forces r to write to the below folder
    
    dir.create("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster", showWarnings = F)
    rasterOptions(tmpdir=file.path("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster"))
    
    
    print(paste0("Working on signifcant wave for model ", models[j],
                 " year ", fn2[i],  ".."))
    
    ws[[i]][[j]] <- swd(ws[[i]][[j]], day_fetch[[i]][[j]]) # This overwriting saves RAM
    
    
    #   We then write to disk as part of the loop so we can unlink temp files 
    #   afterwards
    
    print(paste0("Writing significant wave for model ", j, " year ", fn2[i], " to disk.."))
    
    writeRaster(ws[[i]][[j]], paste0(models[j],"/UKCP18_500_wave_", models[j], "_", fn2[i]),
                
                overwrite = T)
    
    #   Delete temp files at end of loop to retain space on hard drive space
    
    unlink(file.path("C:/Users/hamis/Desktop/Uni/Dissertation/data/temp/raster"), recursive = TRUE,
           force = T)
    
  }
}

#   Remove datasets to clear worktop

rm(day_fetch)
gc(full = T)
rm(ws)
gc(full = T)

#   Read back in

for(j in 1:8){
  
  for(i in 1:length(models)){
    
    print(paste0("Reading fetch for model ", i, " year ", fn2[j], " from disk.."))
    
    day_fetch[[j]][[i]] <-  brick(paste0(models[i],"/UKCP18_500_wave_", models[i], "_", fn2[j]))
    
  }
}

#   Convert mask

for(j in 1:8){
  
  for(i in 1:12){
    
    print(paste0("Masking wave  ", i, " year ", fn2[j], " to brick.."))
    
    wave[[j]][[i]] <- mask(wave[[j]][[i]], grid, inverse = T) # Mask
    
  }
}


#   Write to disk (again but this time with mask to reduce size)


for(j in 1:8){
  
  for(i in 1:12){
    
    print(paste0("Writing significant wave for ", models[i], "_", fn2[j], " to disk.."))
    
    writeRaster(wave[[j]][[i]], 
                
                paste0(models[i],"/UKCP18_500_wave_", models[i], "_", fn2[j]),
                
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                             Calculate percentiles
#-------------------------------------------------------------------------------

#   Here we will calculate the percentiles for the hindcast, and compare these
#   against the ERA5 model of combined wave and swell.

#   After this comparison will add mean ERA5 swell to see the difference that
#   this makes.

#   Assuming model performance is reasonable (in which I assume that the ERA5
#   results will be in the middle or so of model spread) I will then run the 
#   full 100yrs for the full ensemble.

#   At this point it would also be good to identify my best performing model.

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/ukcp18/significant_wave/500")

#   Read all the data back in

#wave <- list()
#for(i in 1:2){wave[[i]] <- list()}


#for(j in 1:2){
  
  #for(i in 1:12){
    
   # print(paste0("Reading significant wave for ", models[i], "_", years[j]))
    
    #wave[[j]][[i]] <- brick(paste0(models[i],"/UKCP18_250_wave_", models[i], "_", years[j]))
    
  #}
#}

#   Now need to combine years 1980 to 2000 to compare against era5

combi_wv <- list()

for(i in 1:12){
  
  print(paste0("Combining decades for model ", models[i], ".."))
  
  mid <- stack(wave[[1]][[i]], wave[[2]][[i]])
  
  combi_wv[[i]] <- brick(mid)
  
}


#   Now lets calculate percentiles without mean swell added

perc <- list()

for(i in 1:length(models)){
    
  print(paste0("Calculating perecentiles for model ", models[i], ".."))
    
  perc[[i]] <- calc(combi_wv[[i]], fun = function(x){
      
    quantile(x, probs = c(0.5,0.75,0.9,0.95,1),  na.rm = T)})
    
}

#   Read in  era5 percentiles as a comparison dataset

era5 <- brick("C:/Users/hamis/Desktop/Uni/Dissertation/data/era5/processed/daily_mean/era5_daily_mean_combined_wave_swell_percentiles.nc")

era5 <- mask(era5, grid, inverse = T)

plot(era5)
plot(perc[[2]])

plot(era5,breaks = c(0:14), col = hcl.colors(14,palette = "reds", rev = T))
plot(perc[[2]], breaks = c(0:14), col = hcl.colors(14,palette = "reds", rev = T))

#   Calculate rmse

rmse <- list()


for(i in 1:12){rmse[[i]] <- list()}

for(j in 1:5){
  
  for(i in 1:length(models)){
    
    rmse[[i]][[j]] <- sqrt(mean(perc[[i]][[j]] - era5[[j]])^2) 

    
  }
}

for(i in 1:12){rmse[[i]] <- brick(rmse[[i]])}

plot(rmse[[2]], breaks = c(0:7), col = hcl.colors(7,palette = "reds", rev = T))


#-------------------------------------------------------------------------------
#                      Compare against addition of mean swell
#-------------------------------------------------------------------------------

#   Read in daily average swell

swl <- brick("C:/Users/hamis/Desktop/Uni/Dissertation/data/era5/processed/daily_mean/era5_daily_mean_processed_swell.nc")

#p_swe <- list()

#for(i in 1:5){
  
#  p_swe <- calc(swl, fun = function(x){
    
#    quantile(x, probs = c(0.5,0.75,0.9,0.95,1),  na.rm = T)})
  
#}

#l_sw <- calc(swl, fun = function(x){
  
#  quantile(x, probs = c(0.25),  na.rm = T)})

#plot(l_sw)
#plot(swl)

#   Take mean swell over the 20 year period

swl <- mean(swl)

#   Add mean swell to the datasets

for(i in 1:12){
  print(paste0("Adding swell to model ", i, ".."))
  combi_wv[[i]] <- combi_wv[[i]] + swl
}

swl_perc <- list()

for(i in 1:length(models)){
  
  print(paste0("Calculating perecentiles for model ", models[i], ".."))
  
  swl_perc[[i]] <- calc(combi_wv[[i]], fun = function(x){
    
    quantile(x, probs = c(0.5,0.75,0.9,0.95,1),  na.rm = T)})
  
}



plot(era5)
plot(swl_perc[[2]])

plot(era5, breaks = c(0:16), col = hcl.colors(17,palette = "reds", rev = T))
plot(swl_perc[[2]], breaks = c(0:16), col = hcl.colors(17,palette = "reds", rev = T))

#===============================================================================
#                         Bias Correction
#===============================================================================

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/")

era5 <- brick("era5/processed/daily_mean/era5_daily_mean_processed_combi_wave.nc")

tplate <- template

r <- raster(nrow = 112, ncol = 82, crs = crs(tplate))

check <- isimip(x = era5, y = combi_wv[[1]][[1]], newdata = r)


