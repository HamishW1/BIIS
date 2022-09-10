#===============================================================================
#                             Bias Correction
#===============================================================================

#   In this script we will attempt to bias correct the UKCP18 projections 
#   to the ERA5 observations using a change factor (CF) methodology

#   This follows the following therotical structure:

#   CF Projections = (Future Projections - Baseline Projections) + Observations

#   For hindcast simulations this means that the reanalysis and the projections
#   will match exactly (essentially (baseline - baseline) + obs)

#   For future it means the trend of simulations will be maintained while the 
#   values will be bias corrected to observations.

#-------------------------------------------------------------------------------
#                         Establishing Environment
#-------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(data.table)
library(terra)

#   This increases run speed

rasterOptions(chunksize = 9e+9,
              maxmemory = 9e+9,
              memfrac = 0.75)


setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/")

#   Read in data 

era_p <- brick("era5/processed/daily_mean/era5_processed_pressure.nc") # era5 pressure
era_w <- brick("era5/processed/daily_mean/era5_processed_wind_speed.nc") # era5 wind

ukcp_p <- list()
ukcp_w <- list()

for(i in 1:12){
  
  ukcp_p[[i]] <- list()
  ukcp_w[[i]] <- list()
  
}

fnp <- dir("UKCP18/pressure/01")
fnw <- dir("UKCP18/wind/sfcWind/01")
models <- dir("UKCP18/pressure")

pfn1 <- strsplit(fnp, "_01_")[[1]][1] # First element of east
wfn1 <- strsplit(fnw, "_01_")[[1]][1] # First element of west

fn2 <- list()

for(i in 1:10){
  
  fn2[[i]] <- strsplit(fnw, "_01_")[[i]][2] # End element of both after model
  
}

for(i in 1:length(models)){
  
  for(j in 1:length(fnp)){
    
    print(paste0("Reading in ", fn2[[j]], " for model ", i))
    
    ukcp_p[[j]][[i]] <- brick(paste0("UKCP18/pressure/", models[i], "/", pfn1, "_", models[i], "_", fn2[[j]])) # UKCP18  pressure
    
    ukcp_w[[j]][[i]] <- brick(paste0("UKCP18/wind/sfcWind/", models[i], "/", wfn1, "_", models[i], "_", fn2[[j]])) # UKCP18 mws
    
  }
}

#-------------------------------------------------------------------------------
#                       Combining to twenty year intervals
#-------------------------------------------------------------------------------

#   For change factors to be applied we need temporal slices of equal length

baseline_p <- list()
x2020_p <- list()
x2040_p <- list()
x2060_p <- list()
x2080_p <- list()

baseline_w <- list()
x2020_w <- list()
x2040_w <- list()
x2060_w <- list()
x2080_w <- list()

for(i in 1:length(models)){
  
  baseline_p[[i]] <- list()
  x2020_p[[i]] <- list()
  x2040_p[[i]] <- list()
  x2060_p[[i]] <- list()
  x2080_p[[i]] <- list()
  
  baseline_w[[i]] <- list()
  x2020_w[[i]] <- list()
  x2040_w[[i]] <- list()
  x2060_w[[i]] <- list()
  x2080_w[[i]] <- list()
  
}


year <- c("2000-2020", "2020-2040", "2040-2060", "2060-2080")

for(i in 1:length(models)){
  
  print(paste0("Stacking pressure baseline for model ", i, ".."))
  baseline_p[[i]] <- stack(ukcp_p[[1]][[i]], ukcp_p[[2]][[i]])  # 1980 to 2000   # DELETE THIS WHEN DONE
  
  print(paste0("Stacking wind baseline for model ", i, ".."))
  baseline_w[[i]] <- stack(ukcp_w[[1]][[i]], ukcp_w[[2]][[i]])  # 1980 to 2000
  
}

for(i in 1:length(models)){
  
  print(paste0("Stacking pressure baseline for model ", i, ".."))
  baseline_p[[i]] <- stack(ukcp_p[[1]][[i]], ukcp_p[[2]][[i]])  # 1980 to 2000
  
  print(paste0("Stacking pressure 2000-2020 for model ", i, ".."))
  x2020_p[[i]] <- stack(ukcp_p[[3]][[i]], ukcp_p[[4]][[i]])     # 2000 to 2020
  
  print(paste0("Stacking pressure 2020-2040 for model ", i, ".."))
  x2040_p[[i]] <- stack(ukcp_p[[5]][[i]], ukcp_p[[6]][[i]])     # 2020 to 2040   PRESSURE
  
  print(paste0("Stacking pressure 2040-2060 for model ", i, ".."))
  x2060_p[[i]] <- stack(ukcp_p[[7]][[i]], ukcp_p[[8]][[i]])     # 2040 to 2060
  
  print(paste0("Stacking pressure 2060-2080 for model ", i, ".."))
  x2080_p[[i]] <- stack(ukcp_p[[9]][[i]], ukcp_p[[10]][[i]])    # 2060 to 2080
  
  print(paste0("Stacking wind baseline for model ", i, ".."))
  baseline_w[[i]] <- stack(ukcp_w[[1]][[i]], ukcp_w[[2]][[i]])  # 1980 to 2000
  
  print(paste0("Stacking wind 2000-2020 for model ", i, ".."))
  x2020_w[[i]] <- stack(ukcp_w[[3]][[i]], ukcp_w[[4]][[i]])     # 2000 to 2020
  
  print(paste0("Stacking wind 2020-2040 for model ", i, ".."))
  x2040_w[[i]] <- stack(ukcp_w[[5]][[i]], ukcp_w[[6]][[i]])     # 2020 to 2040   WIND
  
  print(paste0("Stacking wind 2040-2060 for model ", i, ".."))
  x2060_w[[i]] <- stack(ukcp_w[[7]][[i]], ukcp_w[[8]][[i]])     # 2040 to 2060
  
  print(paste0("Stacking wind 2060-2080 for model ", i, ".."))
  x2080_w[[i]] <- stack(ukcp_w[[9]][[i]], ukcp_w[[10]][[i]])    # 2060 to 2080
  
}


#   List for loops

projected_p <- list(x2020_p, x2040_p, x2060_p, x2080_p)

projected_w <- list(x2020_w, x2040_w, x2060_w, x2080_w)

#   Write to disk and read back in as brick

for(i in 1:4){
  
  for(j in 1:12){ # Writing projections to disk
    
    print(paste0("writing pressure ", year[i], " model ", j, " to disk.."))
    
    try( # This skips if the file already exists in directory
    
    writeRaster(projected_p[[i]][[j]],
                paste0("UKCP18/processed/pressure/",
                       models[j], "/cf_pressure", "_", 
                       models[j], "_", year[i], ".nc"))
    
    )
    
    print(paste0("writing wind ", year[j], " model ", j, " to disk.."))
    
    
    
    try( # This skips if the file already exists in directory
    
    writeRaster(projected_w[[i]][[j]],
                paste0("UKCP18/processed/wind/",
                       models[j], "/cf_wind", "_", 
                       models[j], "_", year[i], ".nc"))
    )
    
    
  }
}

for(i in 1:12){ # Writing baseline to disk
  
  print(paste0("Writing wind baseline for model ", i, " to disk.."))
  
  try(
  
  writeRaster(baseline_w[[i]],
              paste0("UKCP18/processed/wind/",
                     models[i], "/cf_wind", "_", 
                     models[i], "_1980-2000_baseline.nc"))
  )
  
  print(paste0("Writing pressure baseline for model ", i, " to disk.."))
  
  try(
  
  writeRaster(baseline_p[[i]],
              paste0("UKCP18/processed/pressure/",
                     models[i], "/cf_pressure", "_", 
                     models[i], "_1980-2000_baseline.nc"))
  )
  
}


#   Read back in as brick (this is a huge amount quicker than converting in R)

for(i in 1:length(models)){ # For projections
  
  for(j in 1:length(year)){
    
    print(paste0("Reading in pressure year ", year[[j]], " for model ", i))
    
    projected_p[[j]][[i]] <- brick(paste0("UKCP18/processed/pressure/", models[i], "/cf_pressure", "_", 
                                          models[i], "_", year[j], ".nc")) # UKCP18  pressure
    
    print(paste0("Reading in wind year ", year[[j]], " for model ", i))
    
    projected_w[[j]][[i]] <- brick(paste0("UKCP18/processed/wind/", models[i], "/cf_wind", "_", 
                                          models[i], "_", year[j], ".nc")) # UKCP18 mws
  }
}


for(i in 1:length(models)){ # For baselines
    
    print(paste0("Reading in pressure baseline for model ", i))
    
    baseline_p[[i]] <- brick(paste0("UKCP18/processed/pressure/", models[i], "/cf_pressure", "_", 
                                          models[i], "_1980-2000_baseline.nc")) # UKCP18  pressure
    
    print(paste0("Reading in wind baseline for model ", i))
    
    baseline_w[[i]] <- brick(paste0("UKCP18/processed/wind/", models[i], "/cf_wind", "_", 
                                    models[i], "_1980-2000_baseline.nc")) # UKCP18 mws
    
}

#-------------------------------------------------------------------------------
#                           Applying change factors
#-------------------------------------------------------------------------------


#   Apply change factors

CFp <- list()
CFw <- list()

for(i in 1:length(projected_w)){ 
  
  CFp[[i]] <- list()
  CFw[[i]] <- list()
  
}

for(i in 1:length(projected_w)){
  
  for(j in 1:length(models)){
    
    print(paste0("Applying change factors to pressure for ", year[i], " model ", j, ".."))
    CFp[[i]][[j]] <- (projected_p[[i]][[j]] - baseline_p[[j]]) + era_p
    
    print(paste0("Applying change factors to wind for ", year[i], " model ", j, ".."))
    CFw[[i]][[j]] <- (projected_w[[i]][[j]] - baseline_w[[j]]) + era_w
    
  }
}


#   And that's it - write to disk as the new bias corrected dataset.

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("writing bias corrected pressure ", year[i], " model ", j, " to disk.."))
    
    writeRaster(CFp[[i]][[j]],
               paste0("UKCP18/processed/pressure/",
                      models[j], "/cf_pressure", "_", 
                      models[j], "_", year[i], "CF.nc"),
               overwrite = T)
    
    print(paste0("writing bias corrected wind ", year[i], " model ", j, " to disk.."))
    
    writeRaster(CFw[[i]][[j]],
                paste0("UKCP18/processed/wind/",
                       models[j], "/cf_wind", "_", 
                       models[j], "_", year[i], "CF.nc"),
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                Calculating Change Factors Seasonally for Wind
#-------------------------------------------------------------------------------

#   Here we calculate seasonal change factors, in order to preserve seasonal
#   signals. In addition, by have a smaller number of days to average we preserve
#   extremes better.

#   Creating index to subset seasons by

w1ind <- seq(from = 1, to = 7200, by = 360) 
w2ind <- seq(from = 90, to = 7200, by = 360)

sp1ind <- seq(from = 91, to = 7200, by = 360) 
sp2ind <- seq(from = 180, to = 7200, by = 360)

su1ind <- seq(from = 181, to = 7200, by = 360) 
su2ind <- seq(from = 270, to = 7200, by = 360)

a1ind <- seq(from = 271, to = 7200, by = 360) 
a2ind <- seq(from = 360, to = 7200, by = 360)

#   Creating lists to store results in

wtest <- list()
sptest <- list()
sutest <- list()
atest <- list()

for(i in 1:4){
    wtest[[i]] <- list()
    sptest[[i]] <- list()
    sutest[[i]] <- list()
    atest[[i]] <- list() 
}

for(i in 1:4){
  for(j in 1:12){
    wtest[[i]][[j]] <- list()
    sptest[[i]][[j]] <- list()
    sutest[[i]][[j]] <- list()
    atest[[i]][[j]] <- list()  
  }
}


#   Function for calculating change factors

CFfun <- function(proj, base, obs){
  
  x <- (mean(proj) - mean(base)) + obs
  
}

#   This calculates change factors for each season individually

for(i in 1:4){
  
  for(j in 1:12){
    
    for(z in 1:20){
      
      print(paste0("working on years ", year[i], " model ", j, " for slice ", z,".."))
      
      wtest[[i]][[j]][[z]] <- CFfun(proj = projected_w[[i]][[j]][[w1ind[z]:w2ind[z]]],
                                    
                                    base = baseline_w[[j]][[w1ind[z]:w2ind[z]]],
                                    
                                    obs = era_w[[w1ind[z]:w2ind[z]]])
      
        
      sptest[[i]][[j]][[z+1]] <- CFfun(proj = projected_w[[i]][[j]][[sp1ind[z]:sp2ind[z]]],
                                    
                                    base = baseline_w[[j]][[sp1ind[z]:sp2ind[z]]],
                                    
                                    obs = era_w[[sp1ind[z]:sp2ind[z]]])
      
      sutest[[i]][[j]][[z+2]] <- CFfun(proj = projected_w[[i]][[j]][[su1ind[z]:su2ind[z]]],
                                    
                                    base = baseline_w[[j]][[su1ind[z]:su2ind[z]]],
                                    
                                    obs = era_w[[su1ind[z]:su2ind[z]]])
      
      atest[[i]][[j]][[z+3]] <- CFfun(proj = projected_w[[i]][[j]][[a1ind[z]:a2ind[z]]],
                                    
                                    base = baseline_w[[j]][[a1ind[z]:a2ind[z]]],
                                    
                                    obs = era_w[[a1ind[z]:a2ind[z]]])
      
    }
  }
}



#   This recombines the split 20 years into a list of lists

#   Building lists to store variables in

full <- list()

for(i in 1:4){ full[[i]] <- list()}

for(i in 1:4){
  
  for(j in 1:12){
   
    full[[i]][[j]] <- list() 
    
  }
}

#   Now recombining in correct order as list of lists

for(i in 1:4){
  
  for(j in 1:12){
    
    for(z in 1:20){
      
      print(paste0("working on years ", year[i], " model ", j, " for slice ", z,".."))
      
      full[[i]][[j]][[z]] <- stack(c(wtest[[i]][[j]][[z]], 
                               sptest[[i]][[j]][[z+1]], 
                               sutest[[i]][[j]][[z+2]], 
                               atest[[i]][[j]][[z+3]]))
      
    }
  }
}


#   Now flatten to a single raster stack for each 20 years 

for(i in 1:4){
  
  for(j in 1:12){
  
   print(paste0("Converting list for model ", j, " year ", year[i], " to stack.."))
      
   full[[i]][[j]] <- stack(full[[i]][[j]]) 
    
  }
}

#   Write to disk and read back in as brick

for(i in 1:4){ # Writing
  
  for(j in 1:12){
    
    print(paste0("writing bias corrected wind ", year[i], " model ", j, " to disk.."))
    
    writeRaster(full[[i]][[j]],
                paste0("UKCP18/processed/wind/",
                       models[j], "/cf_wind", "_", 
                       models[j], "_", year[i], "CF.nc"),
                overwrite = T)
    
  }
}

for(i in 1:length(year)){ # Reading
  
  for(j in 1:length(models)){
    
    print(paste0("Reading in wind year ", year[[i]], " for model ", j))
    
    full[[i]][[j]] <- brick(paste0("UKCP18/processed/wind/",
                                   models[j], "/cf_wind", "_", 
                                   models[j], "_", year[i], "CF.nc")) # UKCP18 mws
  }
}


#   Now remove any negative numbers - negative wind speeds are not possible.

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("Setting negative values to zero for model ", j, " year ", year[i], ".."))
    
    full[[i]][[j]] <- clamp(full[[i]][[j]], lower = 0)
    
  }
}

#   Write to disk again

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("writing bias corrected wind ", year[i], " model ", j, " to disk.."))
    
    writeRaster(full[[i]][[j]],
                paste0("UKCP18/processed/wind/",
                       models[j], "/cf_wind", "_", 
                       models[j], "_", year[i], "CF.nc"),
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                Calculating Change Factors Seasonally for Pressure
#-------------------------------------------------------------------------------

#   Same as above, simply for pressure now - doing separately for RAM constraints

#   Empty lists to store results in

wtest <- list()
sptest <- list()
sutest <- list()
atest <- list()

for(i in 1:4){
  wtest[[i]] <- list()
  sptest[[i]] <- list()
  sutest[[i]] <- list()
  atest[[i]] <- list() 
}

for(i in 1:4){
  for(j in 1:12){
    wtest[[i]][[j]] <- list()
    sptest[[i]][[j]] <- list()
    sutest[[i]][[j]] <- list()
    atest[[i]][[j]] <- list()  
  }
}


#   This calculates change factors for each season individually

for(i in 1:4){
  
  for(j in 1:12){
    
    for(z in 1:20){
      
      print(paste0("working on years ", year[i], " model ", j, " for slice ", z,".."))
      
      wtest[[i]][[j]][[z]] <- CFfun(proj = projected_p[[i]][[j]][[w1ind[z]:w2ind[z]]],
                                    
                                    base = baseline_p[[j]][[w1ind[z]:w2ind[z]]],
                                    
                                    obs = era_p[[w1ind[z]:w2ind[z]]])
      
      
      sptest[[i]][[j]][[z+1]] <- CFfun(proj = projected_p[[i]][[j]][[sp1ind[z]:sp2ind[z]]],
                                       
                                       base = baseline_p[[j]][[sp1ind[z]:sp2ind[z]]],
                                       
                                       obs = era_p[[sp1ind[z]:sp2ind[z]]])
      
      sutest[[i]][[j]][[z+2]] <- CFfun(proj = projected_p[[i]][[j]][[su1ind[z]:su2ind[z]]],
                                       
                                       base = baseline_p[[j]][[su1ind[z]:su2ind[z]]],
                                       
                                       obs = era_p[[su1ind[z]:su2ind[z]]])
      
      atest[[i]][[j]][[z+3]] <- CFfun(proj = projected_p[[i]][[j]][[a1ind[z]:a2ind[z]]],
                                      
                                      base = baseline_p[[j]][[a1ind[z]:a2ind[z]]],
                                      
                                      obs = era_p[[a1ind[z]:a2ind[z]]])
      
    }
  }
}



#   This recombines the split 20 years into a list of lists

#   Empty lists to store variables in

full <- list()

for(i in 1:4){ full[[i]] <- list()}

for(i in 1:4){
  
  for(j in 1:12){
    
    full[[i]][[j]] <- list() 
    
  }
}

#   Now recombining in correct order as list of lists

for(i in 1:4){
  
  for(j in 1:12){
    
    for(z in 1:20){
      
      print(paste0("working on years ", year[i], " model ", j, " for slice ", z,".."))
      
      full[[i]][[j]][[z]] <- stack(c(wtest[[i]][[j]][[z]], 
                                     sptest[[i]][[j]][[z+1]], 
                                     sutest[[i]][[j]][[z+2]], 
                                     atest[[i]][[j]][[z+3]]))
      
    }
  }
}


#   Now flatten to a single raster stack for each 20 years 

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("Converting list for model ", j, " year ", year[i], " to stack.."))
    
    full[[i]][[j]] <- stack(full[[i]][[j]]) 
    
  }
}

#   Write to disk and read back in as brick

for(i in 1:4){ # Writing
  
  for(j in 1:12){
    
    print(paste0("writing bias corrected pressure ", year[i], " model ", j, " to disk.."))
    
    writeRaster(full[[i]][[j]],
                paste0("UKCP18/processed/pressure/",
                       models[j], "/cf_pressure", "_", 
                       models[j], "_", year[i], "CF.nc"),
                overwrite = T)
    
  }
}

for(i in 1:length(year)){ # Reading
  
  for(j in 1:length(models)){
    
    print(paste0("Reading in pressure year ", year[[i]], " for model ", j))
    
    full[[i]][[j]] <- brick(paste0("UKCP18/processed/pressure/",
                                   models[j], "/cf_pressure", "_", 
                                   models[j], "_", year[i], "CF.nc")) 
  }
}


#   Now remove any negative numbers - negative wind speeds are not possible.

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("Setting negative values to zero for model ", j, " year ", year[i], ".."))
    
    full[[i]][[j]] <- clamp(full[[i]][[j]], lower = 0)
    
  }
}

#   Write to disk again

for(i in 1:4){
  
  for(j in 1:12){
    
    print(paste0("writing bias corrected pressure ", year[i], " model ", j, " to disk.."))
    
    writeRaster(full[[i]][[j]],
                paste0("UKCP18/processed/pressure/",
                       models[j], "/cf_pressure", "_", 
                       models[j], "_", year[i], "CF.nc"),
                overwrite = T)
    
  }
}

#-------------------------------------------------------------------------------
#                Splitting back into decadal intervals
#-------------------------------------------------------------------------------

#   For other script compatibility - run later.

#   This has been run. Didn't save apparently though... Results in change factors
#   folder of UKCP18

#===============================================================================
#                                      END
#===============================================================================








