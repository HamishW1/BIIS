#===============================================================================
#                 Trimming and nearest neighbors for DEM
#===============================================================================

#   In this script we will trim the DEM by gb coastline (and remove the interior
#   to cut down size)

#   Then we will interpolate values from the nearby dem cells to provide values
#   for areas where we have no data.

#   This will fill the dem to the coast - and so allow interaction with the 
#   sea cells (which use 5km cells)

#   The 5km cells can then also be trimmed y the coastline, giving us a nice
#   consistent boundary.

#   There will likely be issues from the interpolation, but we can deal with 
#   that when it comes.

   
#-------------------------------------------------------------------------------
#                       Establishing environment
#-------------------------------------------------------------------------------

library(raster)
library(sf)
library(gdal)
library(gdalUtils)

setwd("C:\\Users\\hamis\\Desktop\\r_scripts\\data")

#   Read in DEM

dem <- raster("DEM\\dem_uk.tif")

#   Read in interior clip

in.clip <- st_read("DEM\\c.clip.gpkg")

#   Read in high watermark clip

hw <- st_read("nhwm.gpkg")

#   Read in extended OSGB grid

osgb <- st_read("5km_grid.gpkg")

#-------------------------------------------------------------------------------
#                       Clipping raster
#-------------------------------------------------------------------------------

#   Insure crs for both are the same 

in.clip <- st_transform(in.clip, st_crs(dem))

#   Insure shapefule is compatiable for mask

in.clip  <- st_zm(in.clip)

#   Now clip by mask - this will take a while

dem <- raster::mask(dem, in.clip)

plot(dem)

#   Finally write to file 

writeRaster(dem, "clipped_dem.tif")

#-------------------------------------------------------------------------------
#                          Buffer raster
#-------------------------------------------------------------------------------

#   Before we clip the DEM to the simplifed hwm I need to buffer the raster out 
#   to make sure that it all can actually be clipped to the coast.

#   The HWM has been processed  (in qgis) to be buffererd, simplified etc to 
#   25m - the resolution of the DEM 

#   Hopefully this will mean that the DEM should follow the contours of the 
#   coast acceptably.

#   Read in DEM 

dem <- raster("clipped_dem.tif")

#   buffer by 200m (this is overly large - but it is simply to insure that all
#   of the raster exceeds the hwm)

#   Do edge here insures that only the cells near NA are examined. This takes 
#   time, but as the raster is so big will save time long term. 

dem <- raster::buffer(dem, width = 200, doEdge = T)






