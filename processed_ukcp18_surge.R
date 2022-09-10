#===============================================================================
#                       Processing UKCP18 Surge
#===============================================================================

#   In this script we will reduce the UKCP18 surge dataset from a hourly 
#   record to a daily maximum record with 360 days a year. This will align 
#   with my model, and allow like for like comparisions.

#-------------------------------------------------------------------------------
#                       Establish Environment
#-------------------------------------------------------------------------------

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

library(raster)
library(ncdf4)
library(tidyverse)
library(data.table)
library(sf)


#   Read in surge data

fn <- dir("UKCP18/sea_level/ukcp18_surge/raw")

surge <- list()

for(i in 1:length(fn)){
  
  print(i)
  surge[[i]] <- brick(paste0("UKCP18/sea_level/ukcp18_surge/raw/", fn[i]))
  
}

#   UKCP18 dataset follows the same 360 day year as the rest of the dataset. As
#   they are year by year they will be consecutive - so from Dec to Dec.

#-------------------------------------------------------------------------------
#                             Process data
#-------------------------------------------------------------------------------

#   Create list of indices to mean by

x = 1:8640
n = 360
indices <- split(x, sort(x%%n))

#   Create list for processed data

pro <- list()
for(i in 1:length(fn)){ pro[[i]] <- list() }

for(i in 1:length(fn)){
  for(j in 1:length(indices)){ # Caclulate daily max surge (and therfore tide)
    
    print(paste0("Calculating daily max surge for ", fn[i], " day ", j))
    
    pro[[i]][[j]] <- mean(surge[[i]][[indices[[j]]]])
    
  }
}

#   Now create 20 yearly datasets of UKCP18 surge (first will be 10 years as is
#   2010 to 2020)

surge <- list()
dec <- list()

for(i in 1:length(fn)){
  
  print(i)
  surge[[i]] <- brick(pro[[i]]) # Convert surge or each year to brick
  
}

#   Create list to store data in 
fnl <- list()

for(i in 1:10){ fnl[[i]] <- surge[[i]] }
fnl <- brick(fnl)

fnl2 <- list() # Create 2nd list for 20 year inteveals 
x = 11:70
n = 3
indices <- split(x, sort(x%%n))

for(i in 1:length(indices)){
  
  print(i)
  
  fnl2[[i]] <- brick(surge[indices[[i]]])
  
}

fn <- "tide_surge_hadgem_"
year <- c("2020-2040", "2040-2060", "2060-2080")

writeRaster(fnl, "UKCP18/sea_level/ukcp18_surge/processed/tide_surge_hadgem_2010-2020.nc")

for(i in 1:length(year)){
  
  print(i)
  writeRaster(fnl2[[i]], paste0("UKCP18/sea_level/ukcp18_surge/processed/", fn,
                                year[i], ".nc"))
  
}

#===============================================================================
#                            ###   END   ###
#===============================================================================









