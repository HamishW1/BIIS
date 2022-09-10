#===============================================================================
#                      Adding swell to significant wave
#===============================================================================

#   In this script we will add swell to significant wave, and compare it against
#   the era5 reanalysis resuslts for the same period

#-------------------------------------------------------------------------------
#                         Establish environment
#-------------------------------------------------------------------------------

#   Packages

library(raster)
library(ncdf4)
library(tidyverse)
library(sf)
library(data.table)
library(doSNOW)
library(foreach)
library(parallel)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#   Read in era5 change factored swell

year <- c("2000-2010", "2010-2020", "2020-2030", "2030-2040",
          "2040-2050", "2050-2060", "2060-2070", "2070-2080")

swl <- list()

for(i in 1:8){
  
  swl[[i]] <- brick(paste0("era5/change_factors/swell_", year[i], ".nc"))
  
}

#   Read in UKCP18 change factored wave

models <-dir("wave/ukcp18/significant_Wave/500/")

wave <- list()

for(i in 1:length(year)){ wave[[i]] <- list() }

for(i in 1:length(year)){
  
  for(j in 1:length(models)){
   
    wave[[i]][[j]] <- brick(paste0("wave/ukcp18/significant_Wave/500/",
                                   models[j], "/UKCP18_500_wave_", models[j],
                                   "_", year[i], ".nc")) 
    
  }
}

#   Read in grid for masking

grid <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/grids/12km_land_mask.gpkg")

grid <- rasterize(grid, wave[[1]][[1]]) # Rasterise
grid <- grid / grid               # Divide by itself to give values of 1
plot(grid)   

#   Read in ERA5 combined swell and wave for comparison

era5 <- brick("era5/era5_raw/era5_combined_sig_wave_raw_2000-2020.nc")

#-------------------------------------------------------------------------------
#                         Wrangle ERA5
#-------------------------------------------------------------------------------


#   Calculate mean daily values for ERA5 

#   They are in time slices - so need to calculate the daily mean here 
x = 1:61368
n = 7671
indices <- split(x, sort(x%%n))

#   Run in parrallel for speed

co <- detectCores() - 2
cl <- makeCluster(co)
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = length(indices), style = 3)
progress <- function(n)setTxtProgressBar(pb, n) 
opts <- list(progress = progress)

mera5 <- list()

mera5 <- foreach(i = 1:length(indices),
                 .options.snow = opts, 
                 .verbose = F, .packages = c("raster")) %dopar% {
                   
                   setTxtProgressBar(pb, i) 
                   
                   return(mean(era5[[indices[[i]]]]))
              
                 }
#   Halt cluster

stopCluster(cl)

#   Convert from list to brick

era5 <- brick(mera5)

#   Reproject and resample

era5 <- projectRaster(from = era5, to = swl[[1]], crs = swl[[1]])
era5 <- resample(era5, swl[[1]])

#    Fit to UKCP18


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

save <- era5

leap <- seq(from = 29, by = 1460, length.out = 5) # length.out accounts for the 5 years

#   Now we index to remove these years and convert to normal years

era5 <- era5[[-leap]]
  
#   For normal years:
#--------------------

may <- seq(from = 151, to = 7300, by = 365)     # Define the days which will be
jly <- seq(from = 211, to = 7280, by = 364)     # removed from the dataset
aug <- seq(from = 241, to = 7260, by = 363)
oct <- seq(from = 301, to = 7240, by = 362)
dec <- seq(from = 361, to = 7220, by = 361)
  
era5 <- era5[[-may]]    # Now we sequentially remove the extra days
era5 <- era5[[-jly]]    # to leave us with 3600.
era5 <- era5[[-aug]]
era5 <- era5[[-oct]]
era5 <- era5[[-dec]]

#   Have one extra year (2000-2021)

era5 <- era5[[1:7200]]

era5 <- brick(era5)     # Convert back to brick before writing.
  
#   Write to disk

writeRaster(era5, "era5/processed/daily_mean/era5_daily_combi_sig_wave_2000-2020", overwrite = T)

#-------------------------------------------------------------------------------
#                             Test case
#-------------------------------------------------------------------------------

era5 <- brick("era5/processed/daily_mean/era5_daily_combi_sig_wave_2000-2020")

#   Here we will add swell to wind wave for a ukcp18 test case and compare
#   it against the era5 projections.

w <- list()
s <- list()

for(i in 1:2){ 
  
  w[[i]] <- wave[[2]][[i]]
  s[[i]] <- swl[[i]]
  
}

w <- brick(w)
s <- brick(s)

#   Mask

w <- mask(w, grid, inverse = T)
s <- mask(s, grid, inverse = T)
era5 <- mask(era5, grid, inverse = T)


plot(w)
plot(s)

sig <- w + s

plot(sig, xlab = "Model")
plot(era5, xlab = "Observations")

m_perc <- calc(sig, fun = function(x){
  
  quantile(x, probs = c(0.5,0.75,0.9,0.95,0.99),  na.rm = T)})

o_perc <- calc(era5, fun = function(x){
  
  quantile(x, probs = c(0.5,0.75,0.9,0.95,0.99),  na.rm = T)})

plot(m_perc, xlab = "model")
plot(o_perc, xlab = "observations")

sig1 <- mean(sig, na.rm = T) # Need the na remove for meaning etc - worth remembering 
era51 <- mean(era5)

plot(sig1, main = "model")
plot(era51, main = "obs")

rmse <- sqrt(sig1 - era51)^2 

plot(rmse)

#   Okay... So a few findings here:

#     1. Overall the model performs very well, simulating the same spatial patterns
#        and having a total rmse of under 0.7 for all areas, and generally below
#        0.3 for coastal areas.

#     2. For percentiles it performs less well, with the model projecting around a 
#        1m too much, with the 99th percentile showing around 2m's too much.

#     3. On a daily level the model produces quite a lot of ugly artifacts - these
#        are a result of the clashes between era5 and ukcp18 weather systems on a 
#        given day.

#   The solution to 2 and 3 are likely to be to change the ukcp18 change factors 
#   applied to era5 to seasonal means for each year, much as was calculated for
#   swell. This smoother approach will reduce the prevalence of artifacts, lower
#   the height significant wave, and hopefully not effect RMSE overall to much -
#   although potentially this could improve as well.

#   So lets do that - last change hopefully!




