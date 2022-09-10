#===============================================================================
#                   NETCDF processing (probablistic)
#===============================================================================

#   In this script I will be experimenting with extracting netcdf data into
#   a 1km grid. However with this data it is not possible to use the simple 
#   raster method displayed in the exact extract and polygonise script.

#-------------------------------------------------------------------------------
#                       Establishing environment
#-------------------------------------------------------------------------------

#   Libaries required

library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(exactextractr)
library(Rcpp)
library(rgdal)
library(magrittr)

template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind/east/01/uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

#-------------------------------------------------------------------------------
#                       Fetching NC
#-------------------------------------------------------------------------------

#   setting nc paths 

ncpath <- "C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/sea_level/raw"
ncname <- "seaLevelAnom_marine-sim_rcp85_ann_2007-2100"
nc <- paste0(ncpath, "\\", ncname, ".nc")
varn <- "seaLevelAnom"

#   Opening nc 

sl <- nc_open(nc)

#   Viewing 

print(sl)

#-------------------------------------------------------------------------------
#                       Fetching NC dimension
#-------------------------------------------------------------------------------

#   Here we will fetch dimensions for lat long, time and the var. Likely
#   will have to use the same method to select percentiles as well.

#   Lat and long

lon <- ncvar_get(sl, "longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(sl, "latitude")
nlat <- dim(lat)
head((lat))

#   Now time (

nctime <- ncvar_get(sl, "time")
nt <- dim(nctime)
nt
tunit <- ncatt_get(sl, "time", "units")
tunit   #   Note there are 360 days in a year

#   Fetching percentile

perc <- ncvar_get(sl, "percentile")
nperc <- dim(perc)
head(perc)
nperc   #   Ah only 3 percentiles - that does explain the size of file.


#-------------------------------------------------------------------------------
#                     Fetching NC variable (as matrix)
#-------------------------------------------------------------------------------

var <- ncvar_get(sl, varn)
nvar <- dim(var)
head(var)
nvar   #    Okay this is showing the dimensions of the matrix - we have:
#               1. Percentiles
#               2. lon
#               3. lat
#               4. time
#           We can see all of this info from the dimensions of the vars 
#           we fetched above. 

#   Lets check the info surrounding the var

#   Long name

var.ln <- ncatt_get(sl, varn, "long_name")
var.ln

#   Units

var.u <- ncatt_get(sl, varn, "units")
var.u   #   In meters

#   no data value

nodata <- ncatt_get(sl, varn, "_FillValue")
nodata  #   Some weird and wonderful number

#   There are some other weird and wonderful datasets we could get
#   but I am not really interested in these.

#   Closing nc

nc_close(sl)


#-------------------------------------------------------------------------------
#                       Subsetting matrix 
#-------------------------------------------------------------------------------

#   I am going out on a bit of a tangent here, but I am going to try and solve
#   my prior issues with display in qgis etc by getting rid of the percentile 
#   dimension

# Dimensions of perc

perc   #    Okay so we want the 2nd value (50th percentile)

#   So we subset by that 

check <- var[2,,,]

#   Now converting matrix to brick

check <- brick(check,
               xmn = min(lon),   #                x min refers to longitude
               xmx = max(lon),
               ymn = min(lat),   #                y min refers to latitude
               ymx = max(lat),
               transpose = T,    #                transpose as the data is often upside down
               crs = st_crs(4326)$proj4string)#   crs as wgs - as it is long and lat


#   Convert to OSGB

check <- projectRaster(check, template)
check <- resample(check, template)


crs(check)


#   And again she works...

plot(check[[1]])

#   And now we see 

writeRaster(check, "C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/sea_level/processed/sl_anom_2007-2100.nc")

#   Lurvely 

#   As we have converted it back to a raster in this way we can now use the 
#   exact extract script to pull it into the 5km grid.

#   Realistically I think I will then need to find nearest neighbours for 
#   the coast.













