#===============================================================================
#                           Wave indications
#===============================================================================

#   In this script I will be extracting the significant wave height data from 
#   the copernicus dataset.

#   Datasets which are being downloaded are:

#     1. Historical covering 1976 to 2005
#     2. RCP8.5 mid century covering 2041 to 2070
#     3. RCP8.5 late century cocering 2071 to 2100

#   These datasets cover the 90th, 95th and 99th percentile - so in effect 
#   the worst case events of the periods covered. 

#   They also cover this as a average for the time period, and so do not show
#   a daily temporal average. This is at odds to my model, and so I intend to
#   use this data to validate my own significant wave height model.

#   Methods for extraction are as follows:

#     1. The data could not be extracted using raster, and so they were 
#        fetched using the nc_get methods.

#     2. The data did not come packaged in 3 dimensions, and so I have had
#        to manaully reconstruct the matrix before converting to raster.

#     3. The cordinates provided were not continous, with a discontinuty 
#        running through prime meridian, so the dataset has had to be spliced
#        and stiched back together

#     4. The x axis of longitude given in the file is incorrect, covering the
#        entire globe rather than europe, so this has had to be altered.

#     5. The y axis of lat is slightly off, so this has had to be adjusted to
#        correct dataset location.

#     6. The data was still not perfectly aligned, and regardless had to be 
#        resampled to the UKCP18 dataset projection to allow comparison. 

#     7. To achive point 6 the data was converted to points, reprojected to
#        espg 20700 and the distance weight interpolated to provide a continous
#        layer.

#-------------------------------------------------------------------------------
#                       Establishing environment
#-------------------------------------------------------------------------------

library(tidyverse)
library(raster)
library(ncdf4)
library(data.table)
library(sf)
library(gstat)

#   Setting working directory

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/dataset-sis-ocean-wave-indicators-raw")

#   Fetching file names

fn <- dir() # Note order of files here - historical, late century, mid century

#   Opening nc files

sw <- list()

for(i in 1:length(fn)){
  
  sw[[i]] <- nc_open(fn[i])
  
}


#   Checking info

print(sw[[i]])

#   Fetch var names

varn <- list()

for(i in 1:length(fn)){
  
  #   Looks confusing, but the data is just stored in a series of lists
  
  varn[[i]] <- as.character(sw[[i]][[15]][[1]][2])
   
}

#-------------------------------------------------------------------------------
#                       Fetching NC variables
#-------------------------------------------------------------------------------

#   Here we will fetch dimensions for lat long, and the variable - x and y will
#   be the same, so we only need to change fetching of the var name

#   Lat and long

lon <- ncvar_get(sw[[1]], "station_x_coordinate")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(sw[[1]], "station_y_coordinate")
nlat <- dim(lat)
head(lat)


#   Var fetched as list

wave <- list()

for(i in 1:length(fn)){
  
  wave[[i]] <- ncvar_get(sw[[i]], varn[[i]])
  
}

head(wave[[1]])

#   Okay - so the variable is not packaged with dimensions of lat and lon. 
#   So we need to package them together in a dataframe.

lat <- as.vector(lat)
lon <- as.vector(lon)

for(i in 1:length(fn)){
  
  wave[[i]] <- as.vector(wave[[i]])
  
}

#   Convert lon to standard wgs84 format 

lon <- lon - 180

#   Combine into dataframe

df <- list()

for(i in 1:length(fn)){
  
  df[[i]] <- data.frame(lat,lon,wave[[i]])
  
}

#-------------------------------------------------------------------------------
#                       Matrix rearrangement
#-------------------------------------------------------------------------------




for(i in 1:length(fn)){
  
  #   Swap to datatable to then convert to wide format (as preparation for 
  #   creating a matrix)
  
  df[[i]] <- data.table(df[[i]])
  
  #   Convert to wide
  
  df[[i]] <- data.table::dcast(df[[i]], formula = lat~lon, value.var = "wave..i..")
  
  #   Convert to dataframe to allow index manipulation
  
  df[[i]] <- as.data.frame(df[[i]])
  
}


#   This snippet of code that has been commented out allowed me to identify
#   the columns in the matrix spatially - so it allowed me to clip the 
#   dateset to just include iceland.

#   Further, the dataset originally had a discontinuty running though prime 
#   meridian, which meant that half the UK was at the far east of the map.

#   I was able to fix this by locating it spatially using the values produced 
#   in the commented out bit of code in qgis (essentially a line along the 
#   top of the map)

#for(i in 1:807){
  
#  df[[1]][556,i] = i
  
#}

#   Here stitching data to remove greenland and have it all run east to west

west <- list()

for(i in 1:length(fn)){
  
  west[[i]] <- df[[i]][,562:807]
  
  df[[i]] <- df[[i]][,2:561]
  
  df[[i]] <- df[[i]][,-438:-561]
  
  df[[i]] <- cbind(west[[i]], df[[i]])
  
}

#   Set new lon coords by changing name (this actually does nothing, but it 
#   makes the general idea of what I am doing a little clearer)

for(i in 1:length(fn)){
  
  setnames(df[[i]], new = as.character(seq(from = -24.5, to = 44.5, length.out = 683)))
  
}

#-------------------------------------------------------------------------------
#                         Convert to raster
#-------------------------------------------------------------------------------

#   List for storing full europe

eu <- list()

for(i in 1:length(fn)){
  
  #   Convert to matrix to allow conversion to raster
  
  df[[i]] <- as.matrix(df[[i]])
  
  #   Convert to raster
  
  eu[[i]] <- raster(df[[i]],
                    xmn = min(-24.7),   # x min refers to longitude - this has been jigged to improve fit
                    xmx = max(44),
                    ymn = min(21.5),   #  y min refers to latitude - has also been jigged
                    ymx = max(lat),
                    crs = st_crs(4258)$proj4string) # Projectin in wgs84 (eu version) as in lat and lon
  
  # Lets have a look
  
  plot(eu[[i]], col = "black") # Upside down. Lets flip
  
  eu[[i]] <- flip(eu[[i]])
  plot(eu[[i]], col = "black")  # And that's worked. Excellent
  
  
}

#   Okay - nice. Thats about as close as we are going to get I think.

#   At this point I want to clip to the UKCP18 dataset, convert to points, and 
#   then use inverse distance weighted interpolation to produce a dataset
#   spatially consistent with that of UKCP18 and my other datasets

#-------------------------------------------------------------------------------
#                     Clip and convert to OSGB proj
#-------------------------------------------------------------------------------

#   Read in template for reprojecting 

template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/tides/final/max_tide.tiff")

#   Read in polygon for clipping (polygon created in qgis to closely match 
#   UKCP18 extent)

clipper <- st_read("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/clipper.gpkg")

#   Check they align

plot(eu[[1]], col = "black")
plot(clipper, add = T)

#   Now crop

for(i in 1:length(fn)){
  
  eu[[i]] <- crop(eu[[i]], extent(clipper))
  
  plot(eu[[i]], col = "black")
  
  #   Excellent. Project to OSGB
  
  eu[[i]] <- projectRaster(from = eu[[i]], to = template)
  
}

#-------------------------------------------------------------------------------
#                 Convert to points and interpolate to UKCP18 grid
#-------------------------------------------------------------------------------

#   Create lists to store data in

pts <-list()
idw_wave <- list()

for(i in 1:length(fn)){
  
  pts[[i]] <- rasterToPoints(eu[[i]], spatial = T)
  plot(pts[[i]])
  
  #   Convert to gstat object
  
  pts[[i]] <- gstat(formula = layer~1, data = pts[[i]])
  
  #   Interpolate using the power value of 2 and the UKCP18 raster as a template
  
  idw_wave[[i]] <- interpolate(template, pts[[i]])
  
  plot(idw_wave[[i]])
  
}

#   Now combine each scenario into its own brick 

idw_wave <- stack(idw_wave)

historical <- brick(idw_wave[[1:3]]) #  These all run 90 - 95 - 99
RCP85mid <- brick(idw_wave[[7:9]])
RCP85late <- brick(idw_wave[[4:6]])


#   Excellent, and write out.

#     1. Historical covering 1976 to 2005
#     2. RCP8.5 mid century covering 2041 to 2070
#     3. RCP8.5 late century cocering 2071 to 2100

#   names to write out as

namses <- c("historical_1976-2005_90-95-99_perc.nc",
            "rcp85_2041-2070_90-95-99_perc.nc",
            "rcp85_2071-2100_90-95-99_perc.nc")

#   List to write bricks out

out <- list(historical, RCP85mid, RCP85late)

#   Directory to write too

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/wave/final")

#   Write out

for(i in 1:3){
  
  writeRaster(out[[i]], namses[i])
  
}

#   Nice

#===============================================================================
#                             ###   END   ###
#===============================================================================







