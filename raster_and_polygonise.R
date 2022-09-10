#===============================================================================
#                   Exact Exctract and polygonise raster
#===============================================================================

#   In this script I will be expermienting with extracting netcdf data into
#   a 1km grid.

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

#   Grid to extract to (or polygons)

grid <- st_read("C:\\Users\\hamis\\Downloads\\OSGB_Grids-master\\Shapefile\\OSGB_Grid_5km.shp")

#   Raster or netcdf to be extracted

pr <- brick("C:\\Users\\hamis\\Desktop\\r_scripts\\data\\psl_rcp85_land-cpm_uk_5km_01_mon_20601201-20801130.nc")

#   Removing irrelevent info from grid

head(grid)

grid <- grid[,1]

#   Setting raster crs to the same as grid

crs(pr) <- crs(grid)

#   Running extract (note there is no need to loop this, with a brick
#   it automatically extracts all values.)

e <- exact_extract(pr, grid, include_cols = "TILE_NAME", progress = T)

#   Collapsing exact extract output from list to dataframe

e <- bind_rows(e)

#   Subsetting to select only those majority within the grids

e <- subset(e, subset = e$coverage_fraction > 0.9)

#   make spatial

e <- left_join(grid, e, by = "TILE_NAME")

#   And write to disk 

st_write(e, "C:\\Users\\hamis\\Downloads\\check.gpkg")


#   Excellent works - but what if we want a polygon vector of the whole area
#   that the netcdf covers?

#   Polygonise raster

pol <- rasterToPolygons(pr[[1]])

#   Convert to sf

pol <- st_as_sf(pol)

head(pol)

#   Write to disk


st_write(pol, "C:\\Users\\hamis\\Desktop\\r_scripts\\data\\5km_grid.gpkg")

#   Reading back in to reproject

dem <- raster("C:\\Users\\hamis\\Desktop\\r_scripts\\data\\DEM\\dem.tif")

dem <- projectRaster(dem, st_crs(27700)$proj4string)

plot(dem)

writeRaster(dem, "C:\\Users\\hamis\\Desktop\\r_scripts\\data\\DEM\\dem_uk.tif")









