#===============================================================================
#                           ERA5 Processing
#===============================================================================

#   In this script we will accomplish the following tasks:

#   1. Downscale the temporal resolution of the ERA5 datasets to daily 
#      resolution.

#   2. Reproject to OSGB

#   3. Resample to the resolution of UKCP18 (12km)

#   4. Run Bretschneider's method for calculating significant wave height
#      and compare it to the era5 native calculations of significant wave 
#      height.

#   5. Extract percentiles of relevent datasets (sig swell, wave, combi etc)

#-------------------------------------------------------------------------------
#                           Establish environment 
#-------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(data.table)
library(terra)
library(plotrix)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/era5")

#   Read in datasets

#   wind
windu <- brick("era5_10_u_wind_1980-2000.nc")
windv <- brick("era5_10_v_wind_1980-2000.nc")

#   Sea level pressure
slr <- brick("era5_sea_level_pressure_1980-2000.nc")

#   Sig waves
swell <- brick("era5_sig_swell_1980-2000.nc")
wave <- brick("era5_sig_wave_wind_1980-2000.nc")
combi <- brick("era5_combined_sig_wave_1980-2000.nc")

#   Template UKCP18
template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind/sfcWind/01/sfcWind_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")


#-------------------------------------------------------------------------------
#                           Select daily value 
#-------------------------------------------------------------------------------

#   I have 8 temporal slices for each day across all datasets - I want to mean
#   these to produce daily values

#   Do I want that? Or do I just want a single slice?

#   After some thought I have decided to just take a single slice - due to the 
#   nature of moving atospheric systems, and the effect of the mean in
#   cancelling extremes, I would just end up with a depressed average.

#   So I want to take every 8th slice. I have 60192 slices.

#60192 / 8

#7524 / 365

#   Right - so its 1 and then every 8 after that 

#indices <- seq(from = 1, by = 8, length.out = 7524)
#tail(indices)

#   Excellent, and now I index each dataset by that

#vars <- list(windu, windv, slr, swell, combi, wave)

#for(i in 1:6){
  
#  vars[[i]] <- vars[[i]][[indices]]
  
#}

#   Convert back to brick for improved processing time.

#for(i in 1:6){
  
#  var[[i]] <- brick(var[[i]])
  
#}

#-------------------------------------------------------------------------------
#                           Select mean daily value 
#-------------------------------------------------------------------------------

#   The above approach seemed to produce somewhat unrealistic extremes and 
#   patterns, so we will try taking the mean value instead

#   Here we split the vector - this provides indices allowing us to mean each
#   day

#wu <- list()
#wv <- list()
#sp <- list()
#swl <- list()
#comb <- list()
#wv <- list()


#x = 1:60192
#n = 7524
#indices <- split(x, sort(x%%n))

#for(i in 1:7524){
  
#  print(paste0("Calculating daily mean for day ", i, ".."))
  
#  wu[[i]] <- mean(windu[[indices[[i]]]])
#  wv[[i]] <- mean(windv[[indices[[i]]]])
  
#  sp[[i]] <- mean(slr[[indices[[i]]]])
  
#  swl[[i]] <- mean(swell[[indices[[i]]]])
#  comb[[i]] <- mean(combi[[indices[[i]]]]) 
#  wv[[i]] <- mean(wave[[indices[[i]]]])
  
#}

#vars <- list(wu, wv, sp, swl, comb, wv)


#-------------------------------------------------------------------------------
#                           Reproject and resample
#-------------------------------------------------------------------------------

#   Here we reporoject to british grid, and then resample to UKCP18's 12km 
#   grid.

names <- c("windu", "windv", "pressure", "swell", "combi swell & wave", "wave")

for(i in 1:6){
  
  print(paste0("Reprojecting ", names[i], " to OSGB.."))
  
  vars[[i]] <- projectRaster(from = vars[[i]], to = template, crs = template)
  
}

#   Check

plot(template)
plot(vars[[6]][[1]], add = T)

#   And now we resample to 12km grid.

for(i in 1:6){
  
  print(paste0("Resampling ", names[i], " to UKCP18 12km grid"))
  
  vars[[i]] <- resample(vars[[i]], template)
  
}

#   Check

plot(template)
plot(vars[[6]][[1]], add = T)

#   Good and now right out.

fnames <- c("era5_processed_windu.nc", 
            "era5_processed_windv.nc",
            "era5_processed_pressure.nc",
            "era5_processed_swell.nc",
            "era5_processed_combi_wave.nc",
            "era5_processed_wave.nc")

for(i in 1:6){
  
  print(paste0("Writing ", fnames[i], " to disk.."))
  
  writeRaster(vars[[i]], 
              paste0("processed/", fnames[i]),
              overwrite = T)
  
}

#   Creating a file of wind speed for era5

ws <- sqrt(vars[[1]]^2 + vars[[2]]^2)

plot(vars[[1]])
plot(vars[[2]])
plot(ws)

#   Creating a file of wind direction for era5 

dirc <- 180 + 180 * atan2(vars[[1]], vars[[2]]) / pi
dirc <- as.integer(dirc)

plot(dirc)


#   Now write out

writeRaster(ws, "processed/era5_wind_speed.nc")
writeRaster(dirc, "processed/era5_wind_direction.nc")


#-------------------------------------------------------------------------------
#                 Calculating percentiles for wave and wind
#-------------------------------------------------------------------------------

#   Now calucalting percentiles for significant waves for use in later model
#   validation.

wave <- list()

for(i in 4:6){
  
  print(paste0("Calculating percentiles for ", names[i], ".."))
  
  wave[[i]] <- calc(vars[[i]], fun = function(x){
    
    quantile(x, probs = c(0.5, 0.75, 0.9,0.95,1),  na.rm = T)})
  
}

#   Check
wave <- wave[4:6]
plot(wave[[3]])

#   Now calculate percentiles for atmospheric vars

atmos <- list(ws, vars[[3]])

for(i in 1:2){
  
  atmos[[i]] <- calc(atmos[[i]], fun = function(x){
    
    quantile(x, probs = c(0.5, 0.75, 0.9,0.95,1),  na.rm = T)})
  
}

#   Check
plot(atmos[[1]])


#   And now write to disk 

names <- c("era5_swell_percentiles.nc",
           "era5_combined_wave_swell_percentiles.nc",
           "era5_wave_percentiles.nc")

for(i in 1:3){
  
  print(paste0("writing ", names[i], " to disk.."))
  
  writeRaster(wave[[i]],
              paste0("processed/",names[i]))
  
}


names <- c("era5_mws_percentiles.nc",
           "era5_pressure_percentiles.nc")

for(i in 1:2){
  
  print(paste0("writing ", names[i], " to disk.."))
  
  writeRaster(atmos[[i]],
              paste0("processed/",names[i]))
  
}

#-------------------------------------------------------------------------------
#                 Calculating signficant wave height
#-------------------------------------------------------------------------------

#   Okay - now we will calculate significant wave height from the wind 
#   reanalysis, and see how this compares to the era5 model outputs

#   First lets define the function

#   With depth Bretschneider's method


dirc <- brick("processed/era5_wind_direction_1_temp_slice.nc")
ws <- brick("processed/era5_wind_speed_1_temp_slice.nc")


swd <- function(ws, fetch){
  
  (ws^2 / 9.80665) * 0.283 * tanh(0.53 * (( 9.80665 * bth) / ws^2)^0.75) * 
    
    tanh(0.0125 * ((9.80665 * fetch) / ws^2)^0.42 / tanh(0.530 * ((9.80665 * bth) / ws^2)))^0.75
  
}

#   Now lets read in fetch (prepared in the sig_wave_height script)

fetch_pts <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/wind/fetch_250.gpkg")

#   Now rasterise

#   Convert crs

fetch_pts <- st_transform(fetch_pts, crs = crs(template))

#   First create a list of pts values (each corresponding to a raster)

pts_list <- list()

for(i in 2:17){
  
  pts_list[[i]] <- fetch_pts[,i]
  
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

plot(fetch)

#   Now we define this function to calculate fetch for each point of the 
#   raster for each day - depending on wind direction

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
#   a calculation of fetch for each day

day_fetch <- list()

for(i in 1:7524){
  
  print(paste0("Working on fetch for day ", i))
  
  day_fetch[[i]] <- fetcher(dirc[[i]], fetch)
  
}

#   Check how it plots

day_fetch <- brick(day_fetch)
plot(day_fetch)

#   Now read in and process bathymetry to allow for prediction of sig wave

bth <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/bathymetry/gebco_2022.tif")

#   Reproject to OSGB

bth <- projectRaster(from = bth, to = template)

#   Resample to res of UKCP18

bth <- resample(bth, template)

#   Remove negatives

bth <- abs(bth)

#   Check
plot(bth)

#   And now we can calculate significant wave 

wave <- list()

for(i in 1:7524){
  
  print(paste0("Calcuating significant wave height for day ", i))
  
  wave[[i]] <- overlay(ws[[i]], day_fetch[[i]], fun = swd)
  
}

wave <- brick(wave)
plot(wave)


writeRaster(wave, "C:/Users/hamis/Desktop/Uni/Dissertation/data/era5/processed/significant_wave/sig_wave_250.nc")

#   Well firs take away from that is that I appear to be much more likely to 
#   have areas of completely flat calm than with UKCP18. I wonder if this is 
#   the result of taking a single hour in a day - the tails are much more
#   prominent - for both high and low.

#   Lets now calculate some statistics 

percentiles <- calc(wave, fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 1),  na.rm = T)})

writeRaster(percentiles, "processed/sig_wave_perc_hw_model_1_time_slice_2500.nc")

#   Now lets calculate the RMSE of the percentiles against one another

hw_perc <- percentiles

era5_perc <- brick("processed/era5_combined_wave_swell_percentiles_1_temp_slice.nc")

hw_perc5 <- brick("processed/sig_wave_perc_hw_model_5000_1_temp_slice.nc")

#   We want to remove areas covered by the land mask 

grid <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/grids/12km_land_mask.gpkg")

grid <- rasterize(grid, template) # Rasterise
grid <- grid / grid               # Divide by itself to give values of 1
plot(grid)                        # Check


#   Now remove land mask areas 

plot(era5_perc)
plot(hw_perc)
plot(hw_perc5)

era5_perc <- mask(era5_perc, grid, inverse = T)
hw_perc <- mask(hw_perc, grid, inverse = T)
hw_perc5 <- mask(hw_perc5, grid, inverse = T)



plot(era5_perc)
plot(hw_perc)
plot(hw_perc5)

#   Now calculate RMSE

rmse <- list()
rmse2 <- list()

#   This calculates RMSE for each layer - so gives an indication of where the 
#   model performs best and worst - in this case at the 99th percentile

for(i in 1:5){
  
  rmse[[i]] <- sqrt(mean((era5_perc[[i]] - hw_perc[[i]])^2))
  
  rmse2[[i]] <- sqrt(mean((era5_perc[[i]] - hw_perc5[[i]])^2))
  
}

rmse <- brick(rmse)
rmse2 <- brick(rmse2)

plot(rmse)
plot(rmse2)

anom <- list()

for(i in 1:5){
  
  anom[[i]] <-  rmse2[[i]] - rmse[[i]]
  
}

anom <- brick(anom)
plot(anom)

#   This calculates rmse across the 5 selected percentiles and gives one 
#   value of rmse

rmse <- sqrt(mean(hw_perc - era5_perc)^2)

plot(rmse)

#   Together these indicate that the model performs worst in the north see
#   and most poorly for the 99th percentile.

#   Also some notable issues for the Irish Sea

#   Now test this against full model run i.e. all days between 1980 and 2000

#   This is my model results

plot(wave)

wave <- mask(wave, grid, inverse = T)

plot(wave)

#   This is ERR5 results for swell and wave

era5 <- vars[[5]]

plot(era5)


era5 <- mask(era5, grid, inverse = T)

plot(era5)

#   Now find RMSE


rmse_full <- sqrt(mean(wave - era5)^2)

plot(rmse_full)


#   Residual sum of squares
#   Residual = Observed value – Predicted value
#   So rss = sum(Observed value – Predicted value)^2 
#   Essentially just means the sum of the difference squared

rss <- sum(era5 - wave)^2
plot(rss)

#   Total sum of squares
#   sum = observed + predicted
#   Essentially just the sum of the observed and the predicted squared
#   So tss = sum(e + 2)^2

tss <- sum(era5 + wave)^2

plot(tss)

#   R2 

r2 <- 1 - (rss / tss)

plot(r2)

#   Okay so r2 correlation really quite good

#   Seeing on the mean scatter plot also quite good correlation

plot(mean(era5), mean(wave))

#   Also reasonable on the percentiles, with a noticable drop off for 
#   the 99th percentile.

plot(era5_perc, hw_perc)


#   Lets check r2 for that


rss <- sum(era5_perc - hw_perc)^2
plot(rss)

#   Total sum of squares
#   sum = observed + predicted
#   Essentially just the sum of the observed and the predicted squared
#   So tss = sum(e + 2)^2

tss <- sum(era5_perc + hw_perc)^2

plot(tss)

#   R2 

r2 <- 1 - (rss / tss)

plot(r2)

#   Even then seing really quite strong correlation.


#   Okay - this is all a bit of a mess. Take a break now, and then aim to clean
#   it all up and write out outputs later.

#   Takeaway I think is its not a bad result, *but* it is very likely worth
#   reducing fetch to see if I can improve the results.

tss <- list()
rss <- list()
r2 <- list()

for(i in 1:5){
  
  rss[[i]] <- sum(era5_perc[[i]] - hw_perc[[i]])^2
  
  tss[[i]] <- sum(era5_perc[[i]] + hw_perc[[i]])^2
  
  r2[[i]] <- 1 - (rss[[i]] / tss[[i]])
  
}

r2 <- brick(r2)

plot(r2)

