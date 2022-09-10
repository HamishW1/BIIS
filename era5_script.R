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

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#   Read in datasets

#   wind
windu <- brick("era5/raw/era5_wind_u_v_1980-2000.nc", varname = "u10")
windv <- brick("era5/raw/era5_wind_u_v_1980-2000.nc", varname = "v10")

#   Sea level pressure
slr <- brick("era5/raw/era5_surface_pressure_1980-2000.nc")

#   Sig waves
swell <- brick("era5/raw/era5_wind_wave_swell_1980-2000.nc", varname = "shts")
wave <- brick("era5/raw/era5_wind_wave_swell_1980-2000.nc", varname = "shww")
combi <- brick("era5/raw/era5_combined_significant_wave.nc")

#   Template UKCP18
template <- raster("UKCP18/wind/sfcWind/01/sfcWind_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")


#-------------------------------------------------------------------------------
#                           Select mean daily value 
#-------------------------------------------------------------------------------

#   I have 8 temporal slices for each day across all datasets - I want to mean
#   these to produce daily values

#   I have 58440 slices.

58440 / 8 # 7305 days

7305 / 365.25 # 20 years (.25 accounts for leap years)


#   Here we split the vector - this provides indices allowing us to mean each
#   day

wu <- list()
wv <- list()
sp <- list()
swl <- list()
comb <- list()
wv <- list()


x = 1:58440
n = 7305
indices <- split(x, sort(x%%n))

for(i in 1:7305){
  
  print(paste0("Calculating daily mean for day ", i, ".."))
  
  wu[[i]] <- mean(windu[[indices[[i]]]])
  wv[[i]] <- mean(windv[[indices[[i]]]])
  
  sp[[i]] <- mean(slr[[indices[[i]]]])
  
  swl[[i]] <- mean(swell[[indices[[i]]]])
  comb[[i]] <- mean(combi[[indices[[i]]]]) 
  wv[[i]] <- mean(wave[[indices[[i]]]])
  
}

vars <- list(wu, wv, sp, swl, comb, wv)


#-------------------------------------------------------------------------------
#                           Reproject and resample
#-------------------------------------------------------------------------------

#   Here we reporoject to british grid, and then resample to UKCP18's 12km 
#   grid.

names <- c("windu", "windv", "pressure", "swell", "combi swell & wave", "wave")

for(i in 1:6){
  
  print(paste0("Reprojecting ", names[i], " to OSGB.."))
  
  vars[[i]] <- brick(vars[[i]])
  
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

#-------------------------------------------------------------------------------
#                           Fit to UKCP18
#-------------------------------------------------------------------------------

#   For change factor bias correction we need the ERA5 dataset to fit that of 
#   UKCP18 (in essence to have 360 day years.)

#   ERA5 follows a standard year, with a standard number in each month. They 
#   are as follows:

#   For leap years:
#--------------------

#   Leap years have occurred 5 times: (so that's 5 extra days)
#   years - 1980, 1984, 1988, 1992, 1996

#   Leap years occur every 4 years - so every 365 * 4 days - 1460 days.
#   First year is a leap year, so day 29 and then every day 1460 days after 
#   that.

save <- vars

leap <- seq(from = 29, by = 1460, length.out = 5) # length.out accounts for the 5 years

#   Now we index to remove these years and convert to normal years

for(i in 1:6){
  
  print(paste0("Removing leap year days from ", names[i]))
                                
  vars[[i]] <- vars[[i]][[-leap]]
  
}


#   For normal years:
#--------------------

#   January - 31 days   #|
#   February - 28 days  #|These balance out, so can be left
#   March - 31 days     #| 
#   April - 30 days     
#   May - 31 days       # Remove a day here (slice 151 recurring)
#   June - 30 days
#   July - 31 days      # Remove a day here (slice 211 recurring)
#   August - 31 days    # Remove a day here (slice 241 recurring) 
#   September - 30 days
#   October - 31 days   # Remove a day here (slice 301 recurring)
#   November - 30 days
#   December - 31 days  # Remove a day here (slice 361 recurring)

#   For the above to work we must run each by turn (so math works)

#   We remove each day every year - (which shorten each run)

may <- seq(from = 151, to = 7300, by = 365)     # Define the days which will be
jly <- seq(from = 211, to = 7280, by = 364)     # removed from the dataset
aug <- seq(from = 241, to = 7260, by = 363)
oct <- seq(from = 301, to = 7240, by = 362)
dec <- seq(from = 361, to = 7220, by = 361)

for(i in 1:6){
  
  print(paste0("Removing extra days from ", names[i]))
  
  vars[[i]] <- vars[[i]][[-may]]    # Now we sequentially remove the extra days
  vars[[i]] <- vars[[i]][[-jly]]    # to leave us with 3600.
  vars[[i]] <- vars[[i]][[-aug]]
  vars[[i]] <- vars[[i]][[-oct]]
  vars[[i]] <- vars[[i]][[-dec]]
  
  vars[[i]] <- brick(vars[[i]])     # Convert back to brick before writing.
  
}


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
              paste0("era5/processed/daily_mean/", fnames[i]),
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

writeRaster(ws, "era5/processed/daily_mean/era5_processed_wind_speed.nc")
writeRaster(dirc, "era5/processed/daily_mean/era5_processed_wind_direction.nc")


#-------------------------------------------------------------------------------
#                           Calculating fetch
#-------------------------------------------------------------------------------

#   Okay - now we will calculate fetch from wind direction.

#   Now lets read in fetch (prepared in the sig_wave_height script)

fetch_pts <- st_read("wave/wind/fetch_500.gpkg")

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

for(i in 1:7200){
  
  print(paste0("Working on fetch for day ", i))
  
  day_fetch[[i]] <- fetcher(dirc[[i]], fetch)
  
}

#   Check how it plots

day_fetch <- brick(day_fetch)
plot(day_fetch)

#   Write to disk

writeRaster(day_fetch, "era5/processed/daily_mean/era5_processed_fetch_500.nc")

#-------------------------------------------------------------------------------
#                       Calculating significant wave
#-------------------------------------------------------------------------------

#   With depth Bretschneider's method - this uses windspeed, fetch and depth
#   to calculate significant wave height

swd <- function(ws, fetch){
  
  (ws^2 / 9.80665) * 0.283 * tanh(0.53 * (( 9.80665 * bth) / ws^2)^0.75) * 
    
    tanh(0.0125 * ((9.80665 * fetch) / ws^2)^0.42 / tanh(0.530 * ((9.80665 * bth) / ws^2)))^0.75
  
}

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

for(i in 1:7200){
  
  print(paste0("Calcuating significant wave height for day ", i))
  
  wave[[i]] <- overlay(ws[[i]], day_fetch[[i]], fun = swd)
  
}

wave <- brick(wave)
plot(wave)

#-------------------------------------------------------------------------------
#                     Calculating percentiles - base
#-------------------------------------------------------------------------------

#   Lets now calculate some statistics 

percentiles <- calc(wave, fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 0.99),  na.rm = T)})

writeRaster(percentiles, "era5/processed/daily_mean/sig_wave_perc_hw_model.nc")

hw_perc <- percentiles


#   Read in era5's own model of combined swell and wave as a comparision.

era5_perc <- brick("era5/processed/daily_mean/era5_processed_combi_wave.nc")

#   Percentiles

percentiles <- calc(era5_perc, fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 0.99),  na.rm = T)})

era5_perc <- percentiles

#   We want to remove areas covered by the land mask 

grid <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/grids/12km_land_mask.gpkg")

grid <- rasterize(grid, template) # Rasterise
grid <- grid / grid               # Divide by itself to give values of 1
plot(grid)                        # Check


#   Now remove land mask areas 

era5_perc <- mask(era5_perc, grid, inverse = T)
hw_perc <- mask(hw_perc, grid, inverse = T)

plot(era5_perc)
plot(hw_perc)

#   Now calculate RMSE

rmse <- list()

#   This calculates RMSE for each layer - so gives an indication of where the 
#   model performs best and worst - in this case at the 99th percentile

for(i in 1:5){
  
  rmse[[i]] <- sqrt(mean((era5_perc[[i]] - hw_perc[[i]])^2))
  
}

rmse <- brick(rmse)
plot(rmse)

#-------------------------------------------------------------------------------
#                     Calculating percentiles - mean swell
#-------------------------------------------------------------------------------

#   In this version we add the mean swell to the model

mean_swl <- mean(vars[[4]]) # Calculate mean swell over the period
swave <- wave + mean_swl    # Add to wave

#   Run percentiles again

sw_perc <- calc(swave, fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 0.99),  na.rm = T)})

#   Mask

sw_perc <- mask(sw_perc, grid, inverse = T)

#   Calculate rmse 

srmse <- list()

for(i in 1:5){
  
  srmse[[i]] <- sqrt(mean((era5_perc[[i]] - sw_perc[[i]])^2))
  
}

srmse <- brick(srmse)

#   Have a look

plot(sw_perc)
plot(era5_perc)
plot(srmse)

#   And very good results. We write these to disk for later analysis etc

writeRaster(sw_perc, "era5/processed/percentiles/hw_swell_percentiles_model.nc")
writeRaster(era5_perc, "era5/processed/percentiles/era5_swell_wave_percentiles_model.nc")
writeRaster(srmse, "era5/processed/percentiles/rmse_era5_combi_hw_swell.nc")

#-------------------------------------------------------------------------------
#                         Extra statistics methods
#-------------------------------------------------------------------------------

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

