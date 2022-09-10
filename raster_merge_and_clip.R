#==============================================================================
#                     Raster merge and clip
#==============================================================================

#   first set directory in which all files reside

setwd("C:\\Users\\hamis\\Desktop\\r_scripts\\data\\DEM")

#   Libraries

library(raster)
library(sf)
library(gdal)
library(gdalUtils)

#   Create list of rasters (in dir) to merge

rast.list <- c('eu_dem_v11_E30N30.tif', "eu_dem_v11_E30N40.tif")

#   fetch extent from two rasters

raster1 <- raster(rast.list[[1]]) 
raster2 <- raster(rast.list[[2]])

#   Record these as vectors so we can splice the min/max of y together

#   Record runs xmin, xmax, ymin, ymax

e1 <- as.vector(extent(raster1))
e2 <- as.vector(extent(raster2))

#   Now combine to max full extent 

e <- extent(e1[1], e1[2], e1[3], e2[4])

#   Produce template raster

template <- raster(e)

#   set projection (matching those to be merged)

projection(template) <- crs(raster1)

#   Write to file

writeRaster(template, "dem.tif", format = "GTiff")

#   And now merge (using the template as output) This will take a little 
#   while. Be patient

mosaic_rasters(gdalfile = rast.list, dst_dataset = "dem.tif", of = "GTiff")

#   Now to clip read back in as well as clipping geometry

#   Clipping geomentry is made in qgis 

#   Before we move on lets clear the deck 

rm(raster1, raster2, e, template)

cliper <- read_sf("cliper.gpkg")
brit <- raster("dem.tif")

#   Insure crs for both are the same 

crs(brit) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
cliper <- st_transform(cliper, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#   First clip by bounding box

brit <- crop(brit, extent(cliper))

#   Insure shapefule is compatiable for mask

cliper <- st_zm(cliper)

#   Now clip by mask - this will take a while

brit <- raster::mask(brit, cliper)

#   Finally write to file 

writeRaster(brit, "clipped_dem.tif")

#   Then deleted all other files and renamed clipped_dem to dem

#===============================================================================
#                              ### END ###
#===============================================================================


