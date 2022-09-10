#===============================================================================
#                   Sea level and atmospheric pressure
#===============================================================================

#   In this script I will be converting the raw UKCP18 pressure dataset into
#   a metric for sea level rise.

#   To do this I will convert pressure values using the following theory:

#     1. Sea level is effected by atmospheric pressure.

#     2. The inverse barometer model states that a change in 1mb of pressure
#        produces 1cm of sea level rise.

#     3. The Met Office states that the average standard pressure at sea
#        level is defined at 1013Pa (or millibars).

#     4. Hence anything over this produces a fall in sea level, and anything
#        under a rise.

#   Methodology to achieve this will be as follows:

#     1. Pressure values will be converted to sea level rise in cm following
#        the simple equation:

#        sea level = standard pressure at sea level - projected pressure

#   As an example:

std_mb <- 1013
rl_mb = 1010

sl <- std_mb - rl_mb

print(paste0("Sea level change due to atmospheric pressure is: ", sl,"cm"))

#   This is a fairly simple calculation, so hopefully this portion of the 
#   model will not take too long.

#   Note that as in most of my models I will be availing of the full ensemble
#   of UKCP18 models.

#-------------------------------------------------------------------------------
#                         Establishing Environment
#-------------------------------------------------------------------------------

#   Sourcing packages
#--------------------

library(tidyverse)
library(raster)
library(ncdf4)
library(sf)


#   Reading in data
#-------------------

#   However, I am still using the full ensemble - so looping required for 
#   read in.

#   Establish working directory

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/seasonal/change_factors/pressure")

#   Fetch folder names

models <- dir()

#   Establish file name (again just 1980 and 2080)

fn <- dir("01")

fn1 <- strsplit(fn, split = "_01_")[[1]][1] # File names contain model so need split

fn2 <- vector()
for(i in 1:length(fn)){
  
  fn2[i] <- unlist(strsplit(fn, split = "_01_")[[i]][2])
  
}

#   Produce list to store files in (nested list)

rl_mb <- list()

for(i in 1:length(models)){ rl_mb[[i]] <- list() }

#   Year variable to track loop

year <- gsub(".nc", "", fn2)

#   Run loop to read in netcdf files

for(i in 1:length(models)){
  
  for(j in 1:length(year)){
    
    print(paste0("reading in pressure from model ", models[i], " years ", year[j]))
    
    rl_mb[[i]][[j]] <- brick(paste0(models[i], "/", fn1, "_", models[i], "_", fn2[j]))
    
  }
}

#   Check values as are expected

plot(rl_mb[[1]][[1]][[1:10]] / 10)
plot(rl_mb[[4]][[2]][[1:10]] / 10)

#   Okay - need to divide all values by 10

for(i in 1:length(models)){
  
  for(j in 1:length(year)){
    
    print(paste0("Correcting values for ", models[i], " years ", year[j]))
    
    rl_mb[[i]][[j]] <- rl_mb[[i]][[j]] / 10
    
  }
}

#   Read in grid for mask 
#------------------------

grid <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/grids/mask.gpkg")

grid <- rasterize(grid, rl_mb[[1]][[1]][[1]]) # Rasterise
grid <- grid / grid               # Divide by itself to give values of 1
plot(grid)                        # Check


#-------------------------------------------------------------------------------
#                     Checking band dates
#-------------------------------------------------------------------------------

#   DONT NEED TO DO THIS ANYMORE AS READING IN PROCESSED CHANGE FACTORS

#   Lets fetch dimensions - I do not think they are in date order, and so 
#   will not match lunar year

#   Looking here at 1980 for model 1 

nc <- nc_open(paste0(models[1], "/", fn1, models[1], fn2[2]))

#   Dates here is hours since 1970-01-01 00:00:00

dates <- ncvar_get(nc, "time")
nc_close(nc)
rm(nc)

#   Lets convert that to days

dates <- round(dates / 24)
min(dates)

#   And now months (UKCP18 are always 30 days)

dates <- dates / 30
min(dates)

#   And years

dates <- dates / 12
min(dates)
max(dates)

# Not a whole number - but days are taken at 12 noon which explains it 

summary(dates)

#   Glad I caught that. The bloody dates are not in order. Start somewhat 
#   arbitrarily at 05/10/1980

#   But its bloody different for the 2070s dataset - starts at 20/06/2069

test <- brick("05/psl_rcp85_land-rcm_uk_12km_05_day_20701201-20801130.nc")

plot(test)

#   Okay - so from a quick test they all start at completely arbitrary times.
#   What a pain.. - it is consistent across ensemble members though.

#   So I need to work out a mathematical loopable way of sorting this.

#   Firstly lets check the minimum hours and the max hours

x1980 <- nc_open("01/psl_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

dates90 <- ncvar_get(x1980, "time")

nc_close(x1980)

x2080 <- nc_open("01/psl_rcp85_land-rcm_uk_12km_01_day_20701201-20801130.nc")

dates28 <- ncvar_get(x1980, "time")

nc_close(x2080)

x1st <- min(dates90)
xlast <- max(dates28)

#   Now we want to know the number of days between these - so convert from 
#   hours to days

x1st <- x1st / 24
xlast <- xlast / 24

#   Now produce a sequence of 30 and 12 months days up to the values to
#   discover starting date

days <- 1:30

x1st / 30 # Need to repeat 1:30 131.0167 times

days <- rep(days, 132)

#   And months 

mnths <- rep(1,30)

for(i in 2:12){
  
  temp <- rep(i, 30)
  
  mnths <- c(mnths, temp)
  
  rm(temp)
}

x1st / 360 # need to repeat 1:360 10.918 times

mnths <- rep(mnths, 11)

#   Join months and days together 

start <- as.data.frame(cbind(days, mnths))

#   Now trim to length of first record of days since 1970-01-01 00:00:00

start <- start[1:ceiling(x1st),]

tail(start)

#   And that does not seem to add up.... According to that the record starts on
#   the first day of the 12th month...

#   Lets check the last day of the record

days <- 1:30

xlast / 30 # Need to repeat 1:30 1330.983 times

days <- rep(days, 1331)

#   And months 

mnths <- rep(1,30)

for(i in 2:12){
  
  temp <- rep(i, 30)
  
  mnths <- c(mnths, temp)
  
  rm(temp)
}

xlast / 360 # need to repeat 1:360 110.9153 times

mnths <- rep(mnths, 111)

#   Join months and days together 

xend <- as.data.frame(cbind(days, mnths))

#   Now trim to length of first record of days since 1970-01-01 00:00:00

xend <- xend[1:ceiling(xlast),]

tail(start)
tail(xend)

#   And finishes on the last day of the 11th month for 2079

plot(test[[3599:3600]]) # test is 2070s to 2080s

#   Okay - so what I am working out makes sense - it starts on the 1st day of 
#   the 12th month for each decade and then runs to the 30th day of the 11th
#   month ten years later.

#   This is odds with the titles on the plots, which suggest the start and 
#   finish at dates mid year - this makes no sense 

#   Added to this the file name dates add up with my math - so I am going to
#   assume I am right.

#   Regardless I am still going to have to make some adjustments to the dates -
#   currently they do not match the lunar year. I think that is best done with
#   the lunar dataset though - I will only have to do that once.

#-------------------------------------------------------------------------------
#                 Converting from pressure to sea level change (slc)
#-------------------------------------------------------------------------------

#   Here will will convert from pressure to sea level change, using the methods
#   outlined in the introduction.


#   Reminder of equation

# sea level change <- standard pressure - projected pressure

std_mb <- 1013

for(i in 1:length(models)){
  
  for(j in 1:length(year)){
    
    print(paste0("Coverting pressure to sea level change for model ", models[i],
                 " year ", year[j]))
    
    #   Convert to cm sea rise
    
    rl_mb[[i]][[j]] <-  std_mb - rl_mb[[i]][[j]] 
    
  }
}




#   Lets have a look at the max damage

for(i in 1:length(year)){
  
  for(j in 1:length(models)){
    
    percentiles <- calc(rl_mb[[i]][[j]], fun = function(x){
      
      quantile(x, probs = c(c(0.5,0.75,0.9, 0.95 ,1), na.rm = T))})
    
    plot(percentiles)
    
    
  }
}


#   And now write out

#   Define file paths

fn1 <- "psl_sea_level_change_"

#   Loop write out 

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18")

for(i in 1:length(models)){
  
  for(j in 1:length(year)){
    
    print(paste0("Writing model ", models[i],
                 " year ", year[j], " to disk.."))
    

    writeRaster(rl_mb[[i]][[j]], paste0("pressure_sea_level/", models[i],
                                      "/", fn1, "_", models[i], "_", fn2[j]),
                overwrite = T)
    
  }
}

#   Read back in and have a look at percentiles # Can do this later.

for(i in 1:length(year)){
  
  for(j in 1:length(models)){
    
    print(paste0("Writing model ", models[j],
                 " year ", year[i], " to disk.."))
    
    
    writeRaster(rl_mb[[i]][[j]], paste0("pressure_sea_level/", models[j],
                                        "/", fn1, "_", models[j], "_", fn2[i]))
    
  }
}



#   Excellent - as a recap we have:

#     1. Converted pressure to sea level change using the inverse barometer 
#        model

#     2. Saved the full converted ensemble to maintain a full lunar decade of 
#        results for each model

#     3. Extracted percentiles for each model in the ensemble.

#   Areas of potential further work:

#     1. Extract the max value across all of the models
#     2. Create a lunar year of max values (i.e each day of the year has the 
#        max value across all models selected)

#===============================================================================
#                               ###  END ###
#===============================================================================









