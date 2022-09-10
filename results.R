#===============================================================================
#                               Results
#===============================================================================

#   In this script I will generate results from change factors, swell, and 
#   significant wave generation. Ultimately this will include results from 
#   combined storm surge as well. 

#-------------------------------------------------------------------------------
#                           Establish environment
#-------------------------------------------------------------------------------

library(terra)
library(raster)
library(ncdf4)
library(tidyverse)
library(data.table)
library(sf)
library(gridExtra)
library(ggpubr)
#library(jtools)
#library(rasterVis)
library(exactextractr)
library(extRemes)
library(ggnewscale)

rasterOptions(progress = "text",timer=TRUE)
options(scipen = 999)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data")

#-------------------------------------------------------------------------------
#                                   Swell
#-------------------------------------------------------------------------------

#   Here I will examine the trend and distribution of swell which I have
#   generated via change factor.

#   For this I will produce:

#   1.  Anomaly maps from baseline to 2080.
#   2.  Trend from 2000-2080.
#   3.  Histogram illustrating shift in distribution. 
#   4.  Baseline seasonal trend.
#   5.  RMSE for 2000-2020

fn <- dir("era5/change_factors/swell")
swl <- list()

for(i in 1:length(fn)){ # Read in projections
  
  swl[[i]] <- brick(paste0("era5/change_factors/swell/", fn[i]))
  
}

#   Read in raster for mask

grid <- st_read("grids/mask.gpkg")
grid <- rasterize(grid, swl[[1]]) # Rasterise
grid <- grid / grid    

#   Read in baseline
base <- brick("era5/processed/daily_mean/era5_processed_swell_1980-2000.nc")

#   Read in model comparison 
comp <- brick("era5/processed/daily_mean/era5_daily_swl_2000-2020.nc")

#-----------------------
#   Calculate RMSE
#-----------------------

#   Check percentiles 

eperc <- calc(comp, fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 0.99),  na.rm = T)})

uperc <- calc(swl[[1]], fun = function(x){
  
  quantile(x, probs = c(0.5, 0.75, 0.9, 0.95, 0.99),  na.rm = T)})

#   And about as expected

plot(eperc)
plot(uperc)

rmse <- sqrt(mean((comp - swl[[1]])^2))
rmse <- mask(rmse, grid, inverse = T)

#   GGPlot

#   Convert to dataframe for plotting
mid <- as(rmse, "SpatialPixelsDataFrame")
mid <- as.data.frame(mid)
colnames(mid) <- c("value", "x", "y")

aplot <- ggplot(mid)+
  
  geom_raster(aes(x = x, y= y, fill = value))+
  scale_fill_viridis_c()+
  ggtitle("Root Mean Square Error Swell")+
  xlab("Eastings")+
  ylab("Northings")+
  labs(fill = "RMSE")+
  theme_classic()+
  coord_fixed(expand = F)

plot(aplot)

#   Save to disk
ggsave(plot = aplot, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_RMSE_2000-2020.png")


#-----------------------
#   Generate anomaly map
#-----------------------

#   This is mean(2060-2080) - mean(1980-2000) 

x2080 <- mean(swl[[4]])
xbase <- mean(base)

anom <- x2080 - xbase
anom <- mask(anom, grid, inverse = T)
plot(anom)

#   Lets make this a little prettier

#   GGPlot

#   Convert to dataframe for plotting
mid <- as(anom, "SpatialPixelsDataFrame")
mid <- as.data.frame(mid)
colnames(mid) <- c("value", "x", "y")

aplot <- ggplot(mid)+
  
  geom_raster(aes(x = x, y= y, fill = value))+
  scale_fill_viridis_c()+
  ggtitle("Primary Swell Anomaly")+
  xlab("Eastings")+
  ylab("Northings")+
  labs(fill = "Swell Height (m)")+
  theme_classic()+
  coord_fixed(expand = F)

plot(aplot)

#   Save to disk
ggsave(plot = aplot, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_anomaly.png")

#-----------------------
#   Trend from 2000-2080
#-----------------------

swl_df <- list()

for(i in 1:length(swl)){
  
  #   Convert to dataframe
  
  swl_df[[i]] <- as.data.frame(swl[[i]], xy = T) %>% reshape2::melt(id.vars = c("x", "y"))
  
}

#   This is 90 days for every grid on the raster 
season <- 1:826560 # This is winter

x = 1:66124800 # This is for each year
n = 20
indices <- split(x, sort(x%%n))

#   This is for each year 
year1 <- 2001:2020
year2 <- 2021:2040
year3 <- 2041:2060
year4 <- 2061:2080

for(i in 1:4){
  
  swl_df[[i]][,"variable"] <- as.character(swl_df[[i]][,"variable"])
  
}

for(j in 1:20){
    
   swl_df[[1]][indices[[j]], "variable"] <- year1[j]
   swl_df[[2]][indices[[j]], "variable"] <- year2[j]
   swl_df[[3]][indices[[j]], "variable"] <- year3[j]
   swl_df[[4]][indices[[j]], "variable"] <- year4[j]
    
}

#   Now make indices for winter, sring, summer, autumn 


season <- 1:826560 # This is the length of a season - there are 80 seasons in 20 years

x = 1:66124800 # This is for each season
n = 80
indices <- split(x, sort(x%%n))

#   Now split them into winter spring etc

w_ind <- list()
s_ind <- list()
su_ind <- list()
a_ind <- list()

for(i in 1:20){
 
  w_ind[[i]] <- indices[[seq(from = 1, to = 80, by = 4)[i]]]
  s_ind[[i]] <- indices[[seq(from = 2, to = 80, by = 4)[i]]]
  su_ind[[i]] <- indices[[seq(from = 3, to = 80, by = 4)[i]]]
  a_ind[[i]] <- indices[[seq(from = 4, to = 80, by = 4)[i]]]
  
}

#   And now rename to reflect year and season

for(i in 1:4){
  for(j in 1:20){
    
    print(paste0("Dataframe ", i, " index ", j))
    
    swl_df[[i]][w_ind[[j]], "variable"] <- paste0("Winter_",swl_df[[i]][w_ind[[j]], "variable"])
    swl_df[[i]][s_ind[[j]], "variable"] <- paste0("Spring_",swl_df[[i]][s_ind[[j]], "variable"])
    swl_df[[i]][su_ind[[j]], "variable"] <- paste0("Summer_",swl_df[[i]][su_ind[[j]], "variable"])
    swl_df[[i]][a_ind[[j]], "variable"] <- paste0("Autumn_",swl_df[[i]][a_ind[[j]], "variable"])
    
  } 
}

#   Now aggregate by season and year to reduce the number of data points

for(i in 1:4){
  
  print(i)
  swl_df[[i]] <- aggregate(swl_df[[i]][,"value"], 
                        by = list(as.character(swl_df[[i]][,"variable"])), 
                        FUN = mean, na.rm = T)
  
}

#   Now split into dataframes for each season

w <- list()
s <- list()
su <- list()
a <- list()

for(i in 1:4){
  
  w[[i]] <- swl_df[[i]][swl_df[[i]][,"Group.1"] %like% "Winter", ]
  s[[i]] <- swl_df[[i]][swl_df[[i]][,"Group.1"] %like% "Spring", ]
  su[[i]] <- swl_df[[i]][swl_df[[i]][,"Group.1"] %like% "Summer", ]
  a[[i]] <- swl_df[[i]][swl_df[[i]][,"Group.1"] %like% "Autumn", ]
  
}

#   Collapse lists into dataframes

w <- bind_rows(w)
s <- bind_rows(s)
su <- bind_rows(su)
a <- bind_rows(a)

#   Strip seasonal reference 

w$Group.1 <- gsub("Winter_", "", w$Group.1)
s$Group.1 <- gsub("Spring_", "", s$Group.1)
su$Group.1 <- gsub("Summer_", "", su$Group.1)
a$Group.1 <- gsub("Autumn_", "", a$Group.1)

seasons <- list(w,s,su,a)

for(i in 1:4){
  seasons[[i]][,"Group.1"] <- as.integer(seasons[[i]][,"Group.1"])
}

#   Create plots

sfun <- function(data, name){ # Function for creating plots with corellation etc
  
  mid <- ggplot(data = data, aes(data[,"Group.1"], data[,"x"]))+
         geom_point()+
         geom_smooth(method = lm, se = F)+
         ggtitle(paste0(name ," Trend for Primary Swell"))+
         xlab(NULL)+
         ylab(NULL)+
         theme_bw()+
         theme(plot.title=element_text(size=10),
               axis.title.x=element_text(size=8),
               axis.title.y=element_text(size=8))
  
  mid <- mid + stat_cor(method = "pearson", alternative = "g", 
                        aes(label = paste(..rr.label..)))
  
  return(mid)
  
}

#   Run function to generate plots

names <- c("Winter", "Spring", "Summer", "Autumn")

plts <- list()
for(i in 1:4){
  
  plts[[i]] <- sfun(seasons[[i]], names[i])
  
}

#   Combine into one plot.

trends <- grid.arrange(plts[[1]], plts[[2]], plts[[3]], plts[[4]],
                       bottom = text_grob("Year",
                                          size=10), 
                       left = text_grob("Mean Significant Swell (m)", size=10, rot = 90))

#   Save to disk
ggsave(plot = trends, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_future_trends.png")

#-------------------------------------------
#   Histograms indicating distribution shift 
#-------------------------------------------

#   Create data frames with 2000 and 2080 to illustrate shift in distribution.

year <- c("2000-2040", "2040-2080")

for(i in 1:4){
  
  setnames(seasons[[i]], old = "Group.1", new = "time_period", skip_absent=TRUE)
  
  seasons[[i]][1:40,"time_period"] <- year[1]
  seasons[[i]][41:80,"time_period"] <- year[2]
  
}

#   Create new plotting function

#   Create plots

sfun <- function(data, name){ # Function for creating plots with corellation etc
  
  mid <- ggplot(data = data, aes(x = x, colour = time_period, fill = time_period))+
    geom_histogram(position="identity", alpha=0.6, bins = 15)+
    scale_color_brewer(palette="Set1")+
    scale_fill_brewer(palette="Set1")+
    geom_density(alpha=0.2)+
    ggtitle(paste0(name ," Distibution for Primary Swell"))+
    xlab(NULL)+
    ylab(NULL)+
    labs(fill = "Time",
         colour = "Time")+
    theme_bw()+
    theme(plot.title=element_text(size=10),
          legend.position=c(0.86,0.81),
          legend.title = element_text(size=6),
          legend.text = element_text(size=5),
          legend.key.size = unit(0.25, 'cm'))
  
  return(mid)
  
}

plts <- list()
for(i in 1:4){
  
  plts[[i]] <- sfun(seasons[[i]], names[i])
  
}

trends <- grid.arrange(plts[[1]], plts[[2]], plts[[3]], plts[[4]],
                       bottom = text_grob("Mean Significant Swell (m)",
                                         size=10), 
                       left = text_grob("Frequency", size=10, rot = 90))

#   Save to disk
ggsave(plot = trends, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_distribution.png")


#---------------------------------------
#   Historic trend (used for projection)
#---------------------------------------

#   Read in preprocessed data

swl <- list()
fn <- dir("era5/swell/processed/")
for(i in 1:4){
  swl[[i]] <- read.csv(paste0("era5/swell/processed/", fn[i]))
}

#   Create plots

sfun <- function(data, name){ # Function for creating plots with corellation etc
  
  mid <- ggplot(data = data, aes(data[,"Year"], data[,"Swell"]))+
    geom_point()+
    geom_smooth(method = lm, se = F)+
    ggtitle(paste0(name ," Trend for Primary Swell"))+
    xlab(NULL)+
    ylab(NULL)+
    theme_bw()+
    theme(plot.title=element_text(size=10),
          axis.title.x=element_text(size=8),
          axis.title.y=element_text(size=8))
  
  mid <- mid + stat_cor(method = "pearson", alternative = "g", 
                        aes(label = paste(..rr.label..)))
  
  return(mid)
  
}

names <- c("Winter", "Spring", "Summer", "Autumn")

#   Order list to fit 

swl <- list(swl[[4]], swl[[2]], swl[[3]], swl[[1]])

plts <- list()
for(i in 1:4){
  
  plts[[i]] <- sfun(swl[[i]], names[i])
  
}

#   Combine into one plot.

trends <- grid.arrange(plts[[1]], plts[[2]], plts[[3]], plts[[4]],
                       bottom = text_grob("Year",
                                          size=10), 
                       left = text_grob("Mean Significant Swell (m)", size=10, rot = 90))

ggsave(plot = trends, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_historic_trend.png")

#   Statistics on linear regression

am <- list()

for(i in 1:4){
  
  am[[i]] <- lm(formula = Swell ~ Year, data = swl[[i]])
  
}

#   This exports to a table - note quite a lot of manual work is required after
#   this. 

export_summs(am, scale = F, to.file = "xlsx", file.name = "test.xlsx",
             model.names = c("Winter", "Spring", "Summer", "Autumn"),
            digits = 3, model.fit = T, model.info = T, pvals = T,
            statistics = "all")

t <- summ(am[[1]])

#--------
#   RMSE 
#--------

#   Here will calculate the RMSE of primary swell compared to the era5 model

#   (Data currently downloading)


#-------------------------------------------------------------------------------
#                             Bias Correction
#-------------------------------------------------------------------------------

#   For bias correction I simply need to demonstrate how the distribution of 
#   UKCP18 has been modified to more closely fit that of ERA5.

#   To do this I will use a histogram containing era5 values, raw ukcp18 values
#   and then the bias corrected ukcp18 values for both pressure and wind.

#   First read in the datasets. We will use UKCP18 model one for simplicities 
#   sake here, and compare all datasets for the 2000-2020 period.

#   era5 wind
ew <- brick("era5/processed/daily_mean/era5_processed_wind_speed_2000-2020.nc")

ew <- brick("era5/processed/daily_mean/era5_processed_wind_speed_1980-2000.nc")

#   era5 pressure

ep <- brick("era5/processed/daily_mean/era5_psl_daily_1980-2000.nc")

#   UKCP18 bc wind
bw <- brick("UKCP18/change_factors/wind/08/bias_corrected_sfcWind_08_2000-2020.nc")

#   UKCP18 bc pressure 
bp <- brick("UKCP18/change_factors/pressure/01/bias_corrected_pressure_01_2000-2020.nc")

#   UKCP18 raw wind
w <- brick("UKCP18/wind/sfcWind/08/sfcWind_08_2000-2020.nc")

#   UKCP18 raw pressure
p <- brick("UKCP18/pressure/01/psl_01_2000-2020.nc")

check <- p + (mean(ep) - mean(p))
check <- mask(check, grid, inverse = T)
check <- as.data.frame(check, xy = T) %>% reshape2::melt(id.vars = c("x", "y"))
check$variable <- "NUDGE"
check <- check[complete.cases(check),]

ep <- brick("era5/processed/daily_mean/era5_daily_psl_2000-2020.nc")
ep <- ep / 100

#   Combine into list

vars <- list(ew,bw,w)

#   Mask

for(i in 1:length(vars)){ vars[[i]] <- mask(vars[[i]], grid, inverse = T)}

#   Convert to dataframe
perc_df <- list()
for(i in 1:length(vars)){
  
  #   Convert to dataframe
  
  print(i)
  perc_df[[i]] <- as.data.frame(vars[[i]], xy = T) %>% reshape2::melt(id.vars = c("x", "y"))
  
  perc_df[[i]] <- perc_df[[i]][complete.cases(perc_df[[i]]),]
  
}

#   Group variables so can differentiate
#vars <- list(ew,ep,bw,bp,w,p) # Order of lists

namer <- c("ERA5","Bias Correction","UKCP18")
for(i in 1:length(perc_df)){ perc_df[[i]][,"variable"] <- namer[i]}

#   Now combine wind and pressure 

wind <- list(perc_df[[1]], perc_df[[3]], perc_df[[5]])
wind <- bind_rows(perc_df)

psl <- list(perc_df[[2]], perc_df[[4]], perc_df[[6]])
psl <- bind_rows(perc_df)
psl <- bind_rows(psl, check)

wind <- bind_rows(wind, check)

#   Now plot histograms 

sfun <- function(data, name){ # Function for creating plots
  
  mid <- ggplot(data = data, aes(x = value, colour = variable, fill = variable))+
    #geom_histogram(position="identity", alpha=0.6, bins = 15)+
    scale_color_brewer(palette="Set1")+
    scale_fill_brewer(palette="Set1")+
    geom_density(alpha=0.0)+
    ggtitle(paste0(name ," Distibution of Pressure"))+
    xlab("Mean Pressure at Sea Level (mb)")+
    ylab("Frequency")+
    labs(fill = "Dataset",
         colour = "Dataset")+
    theme_bw()+
    theme(plot.title=element_text(size=10),
          legend.position=c(0.86,0.81),
          legend.title = element_text(size=8),
          legend.text = element_text(size=7),
          legend.key.size = unit(0.4, 'cm'))
  
  return(mid)
  
}

#here
t <- sfun(wind, "2000-2020")
t


ggsave(plot = t, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/bias_correction",
       filename = "bias_correction_methods_comparision_psl.png")

#-------------------------------------------------------------------------------
#                           Pressure distribution
#-------------------------------------------------------------------------------

#   Here we check that the distribution of pressure does not change over time

#   Read in pressure datasets (we will use model 1 as a check)

psl <- list()
fn <- vector()

for(i in 1:4){ fn[i] <- strsplit(dir("UKCP18/change_factors/pressure/01"), split = "_01_")[[i]][2] }

for(i in 1:4){ psl[[i]] <- brick(paste0("UKCP18/change_factors/pressure/01/",
                                        "bias_corrected_pressure_01_", fn[i]))}


for(i in 1:length(psl)){
  
  print(i)
  
  #   Mask to reduce size by NA removal
  psl[[i]] <- mask(psl[[i]], grid, inverse = T)
  
  #   Convert to dataframe 
  psl[[i]] <- as.data.frame(psl[[i]], xy = T) %>% reshape2::melt(id.vars = c("x", "y"))
  
  #   Only select complete cases
  psl[[i]] <- psl[[i]][complete.cases(psl[[i]]),]
  
}

namer <- c("2000-2020", "2020-2040", "2040-2060", "2060-2080")

for(i in 1:length(psl)){ psl[[i]][,"variable"] <- namer[i]}

psl <- bind_rows(psl)

t <- sfun(psl, "2000-2080")
t

ggsave(plot = t, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/bias_correction",
       filename = "psl_distribution_change_2000-2080_01.png")



#-------------------------------------------------------------------------------
#                         Significant wave
#-------------------------------------------------------------------------------

#   For significant wave we want to describe:

#   1. The trend over the period
#   2. RMSE against era5
#   3. ANOM to future 
#   4. Percentiles of era5 against percentiles of modeled.

#   For this model 1 will be compared against era5. Points 1-3 will be combined
#   in a single plot. Percentiles will be combined into a seperate plot.

#   Calculate RMSE
#-----------------

#   RMSE was calculated separately in the combined wave script. Lets read in
#   all and take intermodal mean

models <- dir("UKCP18/pressure")
rmse <- list()

for(i in 1:12){ 
  
  rmse[[i]] <- brick(paste0("C:/Users/hamis/Desktop/Uni/Dissertation/results/significant_wave/raw_rmse/",
                             "RMSE_",models[i],"combi_sig_wave_2000-2020.nc"))
}

#   Calculate intermodal grand mean RMSE
rmse <- mean(rmse)

#   Now lets convert to a pretty format.

#   Convert to dataframe for plotting
mid <- as(rmse, "SpatialPixelsDataFrame")
mid <- as.data.frame(mid)
colnames(mid) <- c("value", "x", "y")

aplot <- ggplot(mid)+
  
  geom_raster(aes(x = x, y= y, fill = value))+
  scale_fill_viridis_c()+
  ggtitle("Root Mean Square Error Significant Wave")+
  xlab("Eastings")+
  ylab("Northings")+
  labs(fill = "RMSE")+
  theme_classic()+
  coord_fixed(expand = F)

plot(aplot)

#   Now calculate anomaly between 2000-2020 and 2060-2080 

w2000 <- list()
w2060 <- list()

for(i in 1:12){
  
  w2000[[i]] <- brick(paste0("wave/ukcp18/significant_wave/combined/", models[i], "/",
                             "combined_sig_wave_", models[i], "_2000-2020.nc" ))
  
  w2060[[i]] <- brick(paste0("wave/ukcp18/significant_wave/combined/", models[i], "/",
                             "combined_sig_wave_", models[i], "_2060-2080.nc" ))
}

#   Take intermodal grand mean

for(i in 1:12){
  
  print(i)
  w2000[[i]] <- mean(w2000[[i]])
  w2060[[i]] <- mean(w2060[[i]])
  
}

w2000 <- brick(w2000)
w2060 <- brick(w2060)

w2000 <- mean(w2000)
w2060 <- mean(w2060)

                                                                                                                                                                                               
anom <- w2060 - w2000
anom <- mask(anom, grid, inverse = T)
plot(anom)

#   Write anom to disk (takes ages to calculate this, may as well keep a hold..)

writeRaster(anom, "C:/Users/hamis/Desktop/Uni/Dissertation/results/significant_wave/raw_anom_2000-2060.tif")

#   Now plot 

#   Convert to dataframe for plotting
mid <- as(anom, "SpatialPixelsDataFrame")
mid <- as.data.frame(mid)
colnames(mid) <- c("value", "x", "y")


bplot <- ggplot(mid)+
  
  geom_raster(aes(x = x, y= y, fill = value))+
  scale_fill_viridis_c()+
  ggtitle("Significant Wave Anomaly")+
  xlab("Eastings")+
  ylab("Northings")+
  labs(fill = "Anomaly (m)")+
  theme_classic()+
  coord_fixed(expand = F)

plot(bplot)

#   Save to disk
ggsave(plot = aplot, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell",
       filename = "primary_swell_RMSE_2000-2020.png")


#-------------------------------------------------------------------------------
#               Return level trends for extreme still/moving water
#-------------------------------------------------------------------------------

#   This section produces plots indicating the trends on changes in surge level
#   for a given return period. Note this, like the rest of this script, is a 
#   working document. I chop and change the existing scripts. May not run 
#   in totality

setwd("C:/Users/hamis/Desktop/Uni/Dissertation")

gauge <- st_read("data/tides/final/gauge.gpkg")
level_order <- gauge[,c("site", "order")]
level_order$geom <- NULL
gauge$order <- NULL

models <- dir("data/extreme_water/newlyn/no_slr")
fn <- c("2000-2020.nc", "2020-2040.nc", "2040-2060.nc", "2060-2080.nc")
hw <- list()
hw2 <- list()
for(i in 1:12){ 
  hw[[i]] <- list() 
  hw2[[i]] <- list() }

for(i in 1:12){
  for(j in 1:4){
    
    hw[[i]][[j]] <- brick(paste0("data/extreme_water/extreme_moving_water/no_slr/", models[i],
                                 "/emw_no_slr_", models[i], "_", fn[j]))         # without slr
    
  }
}

fn <- c("2010-2020.nc", "2020-2040.nc", "2040-2060.nc", "2060-2080.nc")
for(i in 1:12){
  for(j in 1:4){

    hw2[[i]][[j]] <- brick(paste0("data/extreme_water/extreme_moving_water/with_slr/", models[i],
                                  "/emw_with_slr_", models[i], "_", fn[j]))        # with slr
  }
}

#   I now need to create a 2000-2020 dataset for emw. To do so I will assume
#   that pre 2010 had no sea level rise component relative to baseline. This
#   is clearly false, but it allows use of 20 year periods 

x2010 <- list()
for(i in 1:12){ x2010[[i]] <- list() }

for(i in 1:12){
  print(i)
  x2010[[i]] <- hw[[i]][[1]][[1:3600]]
}

for(i in 1:12){
  print(i)
  hw2[[i]][[1]] <- stack(x2010[[i]], hw2[[i]][[1]])
  hw2[[i]][[1]] <- brick(hw2[[i]][[1]])
}

#   Read in UKCP18 datasets

path <- c("data/UKCP18/sea_level/ukcp18_surge/MPI",
          "data/UKCP18/sea_level/ukcp18_surge/processed")

namer <- c("mpi", "hadgem")

fn2 <- c("tide_surge_MPI_", "tide_surge_hadgem_")

uk <- list()
for(i in 1:2) { uk[[i]] <- list() }

for(i in 1:2){ for(j in 1:4){ 
  uk[[i]][[j]] <- brick(paste0(path[i],  "/", fn2[i], fn[j]))
  uk[[i]][[j]] <- projectRaster(uk[[i]][[j]], hw[[1]][[1]])
  
  }}


#   Lists for storage for hw model
hwx <- list()                           # No slr 
for(i in 1:12){ hwx[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ hwx[[i]][[j]] <- list()  } }

hwx1 <- list()                          # With slr
for(i in 1:12){ hwx1[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ hwx1[[i]][[j]] <- list()  } }

#   uckp18

uk1 <- list()
for(i in 1:2){ uk1[[i]] <- list() }
for(i in 1:2){ for(j in 1:4){uk1[[i]][[j]] <- list()  } }

#   Calculate 5 day maximum for hw model

#   Now calculate maximum over 5 days for 20 year period

x = 1:7200 # This is for each year
n = 1440
indices <- split(x, sort(x%%n))

for(i in 1:12){
  for(j in 1:4){
    for(z in 1:length(indices)){
      
      print(paste0("model ", i, " time ", j, " year ", z))
      hwx[[i]][[j]][[z]] <- max(hw[[i]][[j]][[indices[[z]]]])
      hwx1[[i]][[j]][[z]] <- max(hw2[[i]][[j]][[indices[[z]]]])
    }
  }
}

#   ukcp18

for(i in 1:2){
  for(j in 1:4){
    for(z in 1:length(indices)){
      print(paste0("model ", i, " time ", j, " year ", z))
      uk1[[i]][[j]][[z]] <- max(uk[[i]][[j]][[indices[[z]]]])
    }
  }
}


#   Combine into bricks hw

for(i in 1:12){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    hwx[[i]][[j]] <- brick(hwx[[i]][[j]]) 
    hwx1[[i]][[j]] <- brick(hwx1[[i]][[j]]) 
  }
}

#   ukcp18
for(i in 1:2){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    uk1[[i]][[j]] <- brick(uk1[[i]][[j]]) 
  }
}

#   Now extract into dataframes

#   First lets mask so biliner extraction is consistent across all.

#mask <- hwx1[[1]][[4]][[1]]
#plot(mask)

for(i in 1:12){
  for(j in 1:4){
    print(paste0("Masking model ", i, " period ", j))
    hwx[[i]][[j]] <- mask(hwx[[i]][[j]], uk1[[1]][[1]][[1]])
    hwx1[[i]][[j]] <- mask(hwx1[[i]][[j]], uk1[[1]][[1]][[1]])
    
  }
}


#   hw model

hw <- list()
hw2 <- list()
for(i in 1:12){ 
  hw[[i]] <- list()
  hw2[[i]] <- list()
  }

for(i in 1:12){ for(j in 1:4){ 
  hw[[i]][[j]] <- list() 
  hw2[[i]][[j]] <- list()
  } }

for(i in 1:12){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    hw[[i]][[j]] <- raster::extract(hwx[[i]][[j]],
                                    gauge, df = T, sp = T, method = "bilinear")
    
    hw[[i]][[j]] <- st_as_sf(hw[[i]][[j]])
    hw[[i]][[j]][,"geometry"] <- NULL
    hw[[i]][[j]] <- as.data.frame(hw[[i]][[j]])
    
    hw2[[i]][[j]] <- raster::extract(hwx1[[i]][[j]],
                                    gauge, df = T, sp = T, method = "bilinear")
    
    hw2[[i]][[j]] <- st_as_sf(hw2[[i]][[j]])
    hw2[[i]][[j]][,"geometry"] <- NULL
    hw2[[i]][[j]] <- as.data.frame(hw2[[i]][[j]])
  }
}

#   ukcp18

uk <- list()
for(i in 1:2){ uk[[i]] <- list() }
for(i in 1:2){ for(j in 1:4){ uk[[i]][[j]] <- list() } }

for(i in 1:2){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    uk[[i]][[j]] <- raster::extract(uk1[[i]][[j]],
                                    gauge, df = T, sp = T, method = "simple")
    
    uk[[i]][[j]] <- st_as_sf(uk[[i]][[j]])
    uk[[i]][[j]][,"geometry"] <- NULL
    uk[[i]][[j]] <- as.data.frame(uk[[i]][[j]])
  }
}



#   For hw model
t <- list()
for(i in 1:12) {t[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ t[[i]][[j]] <- list() }}
for(i in 1:12){ for(j in 1:4) for(z in 1:length(hw[[1]][[1]]$site)){{ 
  t[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 7, nrow = 0)) 
}}}

t1 <- list()
for(i in 1:12) {t1[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ t1[[i]][[j]] <- list() }}
for(i in 1:12){ for(j in 1:4) for(z in 1:length(hw[[1]][[1]]$site)){{ 
  t1[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 7, nrow = 0)) 
}}}

#   For uk model
uk2 <- list()
for(i in 1:2) {uk2[[i]] <- list() }
for(i in 1:2){ for(j in 1:4){ uk2[[i]][[j]] <- list() }}
for(i in 1:2){ for(j in 1:4) for(z in 1:length(uk[[1]][[1]]$site)){{ 
  uk2[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 7, nrow = 0)) 
}}}

x = 2:1441 # This is for each year (take 5 max)
n = 20
indices <- split(x, sort(x%%n))
year <- 1:20

#   For hw model

for(i in 1:12){
  for(j in 1:4){
    for(z in 1:length(hw[[1]][[1]]$site)){
      for(y in 1:length(indices)){ 
        
        print(paste0("model ", i, " period ", j, " site ", z, " indice ", y))
        mid <- as.data.frame(matrix(ncol = 72, nrow = 0))
        mid[1,] <- hw[[i]][[j]][z, indices[[y]]] # Select gauge 
        mid <- mid[1,c(order(-mid)[1:5])]    # Select top 5 events
        mid[,"site"] <- hw[[i]][[j]][z,"site"] # Name sites
        mid[,"year"] <- year[y]               # Name year  
        setnames(mid, new = c("x1", "x2", "x3", "x4", "x5", "site", "year"))
        t[[i]][[j]][[z]][y,] <- mid # Combine back to list
        
        mid <- as.data.frame(matrix(ncol = 72, nrow = 0))
        mid[1,] <- hw2[[i]][[j]][z, indices[[y]]] # Select gauge 
        mid <- mid[1,c(order(-mid)[1:5])]    # Select top 5 events
        mid[,"site"] <- hw2[[i]][[j]][z,"site"] # Name sites
        mid[,"year"] <- year[y]               # Name year  
        setnames(mid, new = c("x1", "x2", "x3", "x4", "x5", "site", "year"))
        t1[[i]][[j]][[z]][y,] <- mid # Combine back to list
      
      }
    }
  }
}

#   For ukcp18

for(i in 1:2){
  for(j in 1:4){
    for(z in 1:length(uk[[1]][[1]]$site)){
      for(y in 1:length(indices)){ 
        
        print(paste0("model ", i, " period ", j, " site ", z, " indice ", y))
        mid <- as.data.frame(matrix(ncol = 72, nrow = 0))
        mid[1,] <- uk[[i]][[j]][z, indices[[y]]] # Select gauge 
        mid <- mid[1,c(order(-mid)[1:5])]    # Select top 5 events
        mid[,"site"] <- uk[[i]][[j]][z,"site"] # Name sites
        mid[,"year"] <- year[y]               # Name year  
        setnames(mid, new = c("x1", "x2", "x3", "x4", "x5", "site", "year"))
        uk2[[i]][[j]][[z]][y,] <- mid # Combine back to list
        
      }
    }
  }
}

#   For hw model
store <- list()
for(i in 1:12) {store[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ store[[i]][[j]] <- list() }}
for(i in 1:12){ for(j in 1:4) for(z in 1:length(hw[[1]][[1]]$site)){{ 
  store[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 6, nrow = 0)) 
}}}

#   For ukcp18 or hw model slr
store1 <- list()
for(i in 1:12) {store1[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ store1[[i]][[j]] <- list() }}
for(i in 1:12){ for(j in 1:4) for(z in 1:length(hw[[1]][[1]]$site)){{ 
  store1[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 6, nrow = 0)) 
}}}

year <- c("2000-2020", "2020-2040", "2040-2060", "2060-2080")

#   For hw model
for(i in 1:12){
  for(j in 1:4){
    for(z in 1:length(hw[[1]][[1]]$site)){
      
      print(paste0("model ", i, " period ", j, " site ", z))
      m <- t[[i]][[j]][[z]]                    # Select dataframe
      mid <- as.data.frame(matrix(nrow = 100)) # Create midway dataframe
      mid$value <- unlist(m[,1:5])             # Flatten dataframe
      mid$site <- m$V6                         # Add site
      mid$year <- 1:100                        # add pseudo year
      mid$time <- year[j]                      # add period
      mid$rank <- rank(-mid[,"value"])         # add rank
      mid$return <- (100+1) / mid$rank         # Calculate return period
      mid <- mid[c(order(mid$return)[1:5]), ]  # Select cluster around return period 5 (pseudo one) = 1 is 1:5
      mid <- mid[,-1]                          # remove empty row
      store[[i]][[j]][[z]] <- mid              # Store in list 
      
      m <- t1[[i]][[j]][[z]]                    # Select dataframe
      mid <- as.data.frame(matrix(nrow = 100)) # Create midway dataframe
      mid$value <- unlist(m[,1:5])             # Flatten dataframe
      mid$site <- m$V6                         # Add site
      mid$year <- 1:100                        # add pseudo year
      mid$time <- year[j]                      # add period
      mid$rank <- rank(-mid[,"value"])         # add rank
      mid$return <- (100+1) / mid$rank         # Calculate return period
      mid <- mid[c(order(mid$return)[1:5]), ]  # Select cluster around return period 5 (pseudo one) = 1 is 1:5
      mid <- mid[,-1]                          # remove empty row
      store1[[i]][[j]][[z]] <- mid              # Store in list 
      
      
    }
  }
}

#   For ukcp18
for(i in 1:2){
  for(j in 1:4){
    for(z in 1:length(hw[[1]][[1]]$site)){
      
      print(paste0("model ", i, " period ", j, " site ", z))
      m <- uk2[[i]][[j]][[z]]                    # Select dataframe
      mid <- as.data.frame(matrix(nrow = 100)) # Create midway dataframe
      mid$value <- unlist(m[,1:5])             # Flatten dataframe
      mid$site <- m$V6                         # Add site
      mid$year <- 1:100                        # add pseudo year
      mid$time <- year[j]                      # add period
      mid$rank <- rank(-mid[,"value"])         # add rank
      mid$return <- (100+1) / mid$rank         # Calculate return period
      mid <- mid[c(order(mid$return)[96:100]), ]  # Select cluster around return period 5 (pseudo one) # 20 is 96:100
      mid <- mid[,-1]                          # remove empty row
      store1[[i]][[j]][[z]] <- mid              # Store in list 
      
    }
  }
}


#   Now I need to flatten this lists so I have all values across years in a 
#   single df

#   For hw model
for(i in 1:12){ for(j in 1:4){ store[[i]][[j]] <- bind_rows(store[[i]][[j]]) }}
for(i in 1:12){ store[[i]] <- bind_rows(store[[i]]) }
sites <- unique(store[[1]][,"site"])

#   for ukcp18 or sea level rse
for(i in 1:12){ for(j in 1:4){ store1[[i]][[j]] <- bind_rows(store1[[i]][[j]]) }}
for(i in 1:12){ store1[[i]] <- bind_rows(store1[[i]]) }

#   for hw model
rl <- list()
for(i in 1:12){ 
  rl[[i]] <- data.frame(1,1)
  setnames(rl[[i]], new = c("site", "trend"))
}

#   for ukcp18 or sea level rise
ukrl <- list()
for(i in 1:12){ 
  ukrl[[i]] <- data.frame(1,1)
  setnames(ukrl[[i]], new = c("site", "trend"))
}

#   Function for calculating slope

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}


#   for hw model
for(i in 1:12){
  for(j in 1:length(sites)){
   
    m <- store[[i]]
    m <- subset(m, subset = m$site == sites[[j]])
    
    m <- m[str_order(m$time, decreasing = F),]
    m$year <- 1:length(m$time)
    
    rl[[i]][j,"trend"] <- slope(m$year, m$value) * 1000 / 4
    rl[[i]][j,"site"] <- sites[j]
    
    rl[[i]][,"model"] <- models[i]
    
  }
}

#   for uckp18 or sea level rise
for(i in 1:12){
  for(j in 1:length(sites)){
    
    m <- store1[[i]]
    m <- subset(m, subset = m$site == sites[[j]])
    
    m <- m[str_order(m$time, decreasing = F),]
    m$year <- 1:length(m$time)
    
    ukrl[[i]][j,"trend"] <- slope(m$year, m$value) * 1000 / 4
    ukrl[[i]][j,"site"] <- sites[j]
    
    ukrl[[i]][,"model"] <- models[i]
    
  }
}



#   For hw model
return1 <- bind_rows(rl)
return1 <- reshape(return1, idvar = "site", timevar = "model", direction = "wide")
return1$r1mean <- rowMeans(return1[,2:13], na.rm = T)
return1$x95th <- apply(return1[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.95),na.rm = T)
return1$x5th <- apply(return1[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.05),na.rm = T)
return1$x50th <- apply(return1[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.50),na.rm = T)
return1 <- left_join(return1, level_order, by = "site")
return1 <- return1[order(return1$order),]
return1$site <- ifelse(return1 $site == "Liverpool, Gladstone Dock", "Liverpool", return1$site)
l_order <- return1$site
return1 <- return1[!return1$site == "St. Mary's",]

#return1 <- return1[!return1$site == "Leith",]

#   Sea level
return2 <- bind_rows(ukrl)
return2 <- reshape(return2, idvar = "site", timevar = "model", direction = "wide")
#return2[24,11] <- NA # Manual removal of odd value
#return2[18,2:13] <- abs(return2[18,2:13]) # Make leith postive
return2$r1mean <- rowMeans(return2[,2:13], na.rm = T)
return2$x95th <- apply(return2[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.95), na.rm = T)
return2$x5th <- apply(return2[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.05), na.rm = T)
return2$x50th <- apply(return2[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.50), na.rm = T)
return2 <- left_join(return2, level_order, by = "site")
return2 <- return2[order(return2$order),]
return2$site <- ifelse(return2$site == "Liverpool, Gladstone Dock", "Liverpool", return2$site)
l_order <- return2$site
return2 <- return2[!return2$site == "St. Mary's",]

# <- return2[!return2$site == "Leith",]

#   Something is going screwy with Leith on 20 year return


#   for ukcp18
uk_return1 <- bind_rows(ukrl)
uk_return1$model <- ifelse(uk_return1$model == "01", "MPI", "HADGEM")
uk_return1 <- left_join(uk_return1, level_order, by = "site")
uk_return1 <- uk_return1[order(uk_return1$order),]
uk_return1$site <- ifelse(uk_return1 $site == "Liverpool, Gladstone Dock", "Liverpool", uk_return1$site)
l_order <- uk_return1$site
l_order <- unique(l_order)

BIIS <- return1[,c(1,14,18)]
BIIS$model <- "BISS"
colnames(BIIS) <- c("site", "surge", "order", "engine")

setnames(uk_return1, old = colnames(uk_return1), new = c("site", "surge", "engine", "order")) 
uk_return1 <- bind_rows(uk_return1, BIIS)
uk_return1 <- uk_return1[!uk_return1$site == "St. Mary's",]
return1 <- return1[!return1$site == "St. Mary's",]
#return1 <- bind_rows(return1, return1)

#   For hw model
c <- ggplot(data = return1, aes(x = factor(site, level = l_order), y = r1mean, group=1))+
      geom_line(color="green")+
      geom_ribbon(aes(ymin=x5th, ymax=x95th, fill = "green"))+
      scale_fill_manual(values=alpha("green", alpha = 0.25))+
      geom_hline(yintercept=0, linetype = "dashed")+
      theme_bw()+
      xlab("")+
      ylab("")+
      ggtitle("BIIS 20 Year Return Level Ensemble")+
      theme(legend.position="none")+
      theme(text = element_text(size = 15))+
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 10))

#   For sea level 

# HERE

a2 <- ggplot()+
  geom_line(data = return1, aes(x = factor(site, level = l_order), y = r1mean, group=1,color="blue"))+
  geom_ribbon(data = return1, aes(ymin=x5th, ymax=x95th, x = factor(site, level = l_order)),
              fill = "blue", inherit.aes = FALSE, group = 1, alpha = 0.25)+
  
  geom_line(data = return2, aes(x = factor(site, level = l_order), y = r1mean, group=1,color="red"))+
  geom_ribbon(data = return2, aes(ymin=x5th, ymax=x95th, x = factor(site, level = l_order)),
              fill = "red", inherit.aes = FALSE, group = 1, alpha = 0.25)+
  
  geom_hline(yintercept=0, linetype = "dashed")+
  theme_bw()+
  xlab("")+
  ylab("")+
  ggtitle("Extreme Moving Water 1 Year Return Level")+
  labs(color="Forcing")+
  scale_color_manual(labels = c("No SLR", "With SLR"), values = c("blue", "red"))+
  theme(text = element_text(size = 15))+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 10))+
  scale_y_continuous(limits = c(-5, 18), breaks = seq(-5, 18, 5))

#   20 year return level saved as a1

#   For ukcp18 comparison

d <- ggplot()+     
      geom_line(data = uk_return1, 
                aes(x = factor(site, level = l_order), 
                    y = surge, group=engine, color = engine),size=1)+
      scale_color_brewer(palette="Set1")+
      scale_fill_brewer(palette="Set1")+
      geom_hline(yintercept=0, linetype = "dashed")+
      labs(color="Model") +
      theme_bw()+
      xlab("")+
      ylab("")+
      ggtitle("20 Year Return Level Comparison")+
      theme(text = element_text(size = 15))+
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 10))+
      scale_y_continuous(limits = c(-3.2, 4), breaks = seq(-3.2, 4, 1))


#   So a and b have been saved as 1 year return levels 

trends <- grid.arrange(a2, a1,
                       bottom = text_grob("Gauge",
                                          size=20), 
                       left = text_grob("Surge Trend (mm/yr)", size=20, rot = 90),
                       top = text_grob("Extreme Moving Water", size=20),
                       ncol = 1)

#   Save to disk
ggsave(plot = trends, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results/extreme_water",
       filename = "extreme_moving_water_trends2.png",
       width = 30, height = 30, units = "cm")



#-------------------------------------------------------------------------------
#                       General extreme value theory
#-------------------------------------------------------------------------------

#   In this section I am going to dabble in extreme value theory to estimate 
#   how 100 year return levels change throughout time for extreme moving 
#   water.

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/")

gauge <- st_read("data/tides/final/gauge.gpkg")
level_order <- gauge[,c("site", "order")]
level_order$geom <- NULL
gauge$order <- NULL

models <- dir("data/extreme_water/newlyn/with_slr")
fn <- c("2000-2020.nc", "2020-2040.nc", "2040-2060.nc", "2060-2080.nc")
hw2 <- list()
for(i in 1:12){ hw2[[i]] <- list() }


fn <- c("2010-2020.nc", "2020-2040.nc", "2040-2060.nc", "2060-2080.nc")
for(i in 1:12){
  for(j in 1:4){
    print(i)
    hw2[[i]][[j]] <- brick(paste0("data/extreme_water/extreme_moving_water/with_slr/", models[i],
                                  "/emw_with_slr_", models[i], "_", fn[j]))        # with slr
  }
}

#   I now need to create a 2000-2020 dataset for emw. To do so I will assume
#   that pre 2010 had no sea level rise component relative to baseline. This
#   is clearly false, but it allows use of 20 year periods 

emw10 <- list()

for(i in 1:12){ emw10[[i]] <- list() }

for(i in 1:12){
  print(i)
  emw10[[i]] <- brick(paste0("data/extreme_water/extreme_moving_water/no_slr/",
                           models[i], "/", "emw_no_slr_", models[i], "_",
                           "2000-2020.nc"))
  
  x2010[[i]] <- emw100[[1:3200]]
  
}

for(i in 1:12){
  print(i)
  hw2[[i]][[1]] <- stack(hw2[[i]][[1]], x2010[[i]])
  hw2[[i]][[1]] <- brick(hw2[[i]][[1]])
}

hwx1 <- list()                          # With slr
for(i in 1:12){ hwx1[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ hwx1[[i]][[j]] <- list()  } }


#   Calculate 5 day maximum for hw model

#   Now calculate maximum over 5 days for 20 year period

x = 1:7200 # This is for each year
n = 1440
indices <- split(x, sort(x%%n))

for(i in 1:12){
  for(j in 1:4){
    for(z in 1:length(indices)){
      
      print(paste0("model ", i, " time ", j, " year ", z))
      hwx1[[i]][[j]][[z]] <- max(hw2[[i]][[j]][[indices[[z]]]])
    }
  }
}

#   Combine into bricks hw

for(i in 1:12){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    hwx1[[i]][[j]] <- brick(hwx1[[i]][[j]]) 
  }
}

#   MASK

#   Now extract into dataframes
#   hw model

hw2 <- list()
for(i in 1:12){ hw2[[i]] <- list()}

for(i in 1:12){ for(j in 1:4){  hw2[[i]][[j]] <- list()} }

for(i in 1:12){
  for(j in 1:4){
    print(paste0("model ", i, " period ", j))
    
    hw2[[i]][[j]] <- raster::extract(hwx1[[i]][[j]],
                                     gauge, df = T, sp = T, method = "bilinear")
    
    hw2[[i]][[j]] <- st_as_sf(hw2[[i]][[j]])
    hw2[[i]][[j]][,"geometry"] <- NULL
    hw2[[i]][[j]] <- as.data.frame(hw2[[i]][[j]])
  }
}

t <- list()
for(i in 1:12) {t[[i]] <- list() }
for(i in 1:12){ for(j in 1:4){ t[[i]][[j]] <- list() }}
for(i in 1:12){ for(j in 1:4) for(z in 1:length(hw2[[1]][[1]]$site)){{ 
  t[[i]][[j]][[z]] <- as.data.frame(matrix(ncol = 7, nrow = 0)) 
}}}

#   And now extract the maximum 5 for each year period.

x = 2:1441 # This is for each year (take 5 max)
n = 20
indices <- split(x, sort(x%%n))
year <- 1:20

for(i in 1:12){
  for(j in 1:4){
    for(z in 1:length(hw2[[1]][[1]]$site)){
      for(y in 1:length(indices)){ 
        
        print(paste0("model ", i, " period ", j, " site ", z, " indice ", y))
        mid <- as.data.frame(matrix(ncol = 72, nrow = 0))
        mid[1,] <- hw2[[i]][[j]][z, indices[[y]]] # Select gauge 
        mid <- mid[1,c(order(-mid)[1])]    # Select top event
        mid <- data.frame(mid, hw2[[i]][[j]][z,"site"])
        #mid[,"site"] <- hw2[[i]][[j]][z,"site"] # Name sites
        mid[,"year"] <- year[y]               # Name year 
        setnames(mid, new = c("value","site", "year"))
        #setnames(mid, new = c("x1", "x2", "x3", "x4", "x5", "site", "year"))
        t[[i]][[j]][[z]][y,] <- mid # Combine back to list
        
      }
    }
  }
}


#   Now I need to combine the data into into sites for each 40 year period 
#   and each model


hw1 <- list()
for(i in 1:12){ hw1[[i]] <- list() }
for(i in 1:12){ for(j in 1:2){ hw1[[i]][[j]] <- list() }}

#   First combine datasets into 40 year blocks

for(i in 1:12){
    print(i)
    hw1[[i]][[1]] <- bind_rows(t[[i]][[1]], t[[i]][[2]])
    hw1[[i]][[2]] <- bind_rows(t[[i]][[3]], t[[i]][[4]])
}

#   Now subset data into sites

store <- list()
for(i in 1:12){ store[[i]] <- list() }
for(i in 1:12){ for(j in 1:2){ store[[i]][[j]] <- list() }}

year <- c("2000-2040", "2040-2080")
sites <- unique(gauge$site)

for(i in 1:12){
  for(j in 1:2){
    for(z in 1:length(sites)){
      
      print(paste0("model ", i, " period ", j, " site ", z))
      mid <- hw1[[i]][[j]]
      mid <- mid[mid$V2 == sites[z],]
      #mid <- mid[mid$V6 == sites[z],]
      
      mid <- melt(mid, id.vars = "V2", measure.vars <- colnames(mid[c(1)]),
                  value.name = "value")
      #mid <- melt(mid, id.vars = "V6", measure.vars <- colnames(mid[c(1:5)]),
      #            value.name = "value")
      
      mid$variable <- NULL
      mid$year <- year[j]
      setnames(mid, old = "V2", new = "site")
      #mid$V7 <- NULL
      #setnames(mid, old = "V6", new = "site")
       
      
      store[[i]][[j]][[z]] <- mid
       
    }
  }
}

#   Now we we fit an extreme value distribution for each location to the two
#   time periods.

gev <- list()
for(i in 1:12) { gev[[i]] <- list() }
for(i in 1:12){ for(j in 1:2){ gev[[i]][[j]] <- list() } }

for(i in 1:12){
  for(j in 1:2){
    for(z in 1:length(sites)){
      
      print(paste0("model ", i , " period ", j, " site ", z))
      
      mid <- fevd(store[[i]][[j]][[z]][,"value"],type="GEV")
      
      m <- return.level(mid, return.period = 100)
      m <- as.numeric(m)
      m <- data.frame(sites[z], m)
      m$period <- year[j]
      colnames(m) <- c("site", "100yr_rl", "period")
      
      gev[[i]][[j]][[z]] <- m
      
    }
  }
}

#   Reduce to one dataframe per model

r100 <- list()
for(i in 1:12){
  for(j in 1:2){
    
    print(i)
    m <- bind_rows(gev[[i]])
    r100[[i]] <- m
    
  }
}

#   Join as one dataframe

r100 <- r100 %>% reduce(left_join, by = c("site", "period"))
r100$year <- r100$period
r100$period <- NULL

#   Calculate quatiles across models

#   Remove artifact numbers 

r100[,2:13] <- r100[,2:13] %>% mutate(replace(r100[,2:13], r100[,2:13] > 10, NA))

#   Caclucale percentiles

r100$x95th <- apply(r100[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.95), na.rm = T)
r100$x5th <- apply(r100[,2:13], MARGIN = c(1), FUN = quantile, probs = c(0.05), na.rm = T)
r100$mean <- rowMeans(r100[,2:13], na.rm = T)

r100 <- r100[,c(1,14:17)]

#   Write to disk

write_csv(r100, "results/extreme_water/rl_100yr.csv")

#   Okay - thats the table of change finished. I think as a demonstartion of 
#   model fit though I will need a few graphs indicating the results from the 
#   GEV analysis.


newlyn <- fevd(store[[1]][[1]][[27]][,"value"],type="GEV")
leigh <- fevd(store[[2]][[2]][[18]][,"value"],type="GEV")
sheerness <- fevd(store[[4]][[2]][[36]][,"value"],type="GEV")

plot(newlyn)

n1 <- plot(sheerness, main = "Sheerness Model 4 2040-2080", type = c("density"))
n2 <- plot(sheerness, main = "", type = "qq")
n3 <- plot(sheerness, main = "", type = "rl")

#   Sadly these plots need to be coped and pasted out to paint. Clearly not ideal.

#===============================================================================
#                         RMSE and percentiles
#===============================================================================

#   In this section I will generate RMSE plots, and percentile anomaly plots
#   for swell, wind wave and extreme water.

#   The logic behind these plots is that they will each cover the 2000-2020 
#   period. This allows homogeneity in temporal scale, and will further allow
#   isolation of the stronger and weaker aspects of the model.

#   To capture the full range of intermodal variability three plots for each 
#   will be produced - 5th percentile, RSME and 95th percentile. 

#   The percentiles will be taken from the full ensemble, and the intermodal
#   mean will be taken and used for RMSE. Percentiles and intermodal mean 
#   will likewise be used for the UKCP18 extreme water datasets.

#   Primary Swell
#----------------

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/")
models <- dir("data/extreme_water/newlyn/with_slr")

#   Read in data

#   HW SWELL
swl <- brick("data/era5/change_factors/swell/swell_2000-2020.nc")
#   ERA5 SWELL
comp <- brick("data/era5/processed/daily_mean/era5_daily_swl_2000-2020.nc")

#   Calculate RMSE
sw_rmse <- sqrt(mean(comp - swl)^2) 
  

#   Calcuate percentiles
eperc <- calc(comp, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})
uperc <- calc(swl, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})

#   Create anomalies from percentiles 

swl_5th <- eperc[[1]] - uperc[[1]]
swl_95th <- eperc[[2]] - uperc[[2]]

#   Wind Wave
#------------

#   read in data

wave <- list()
for(i in 1:12){wave <- list()}

#   HW model
for(i in 1:12){
  wave[[i]] <- brick(paste0("data/wave/ukcp18/significant_wave/raw/",
                            models[i], "/", "UKCP18_500_wave_", models[i],
                            "_2000-2020.nc"))
}

#   era5
wera <- brick("data/era5/processed/daily_mean/era5_daily_mean_sig_wind_wave_2000-2020.nc")

#   Now calculate percentiles from the full dataset

wens <- stack(wave[[1]],wave[[2]],wave[[3]],wave[[4]],wave[[5]],wave[[6]],
              wave[[7]],wave[[8]],wave[[9]],wave[[10]],wave[[11]],wave[[12]])

weperc <- calc(wera, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})
wuperc <- calc(wens, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})

wave_5th <- weperc[[1]] - wuperc[[1]]
wave_95th <- weperc[[2]] - wuperc[[2]]


#   Now calculate intermodal mean

wavg <- overlay(wave[[1]],wave[[2]],wave[[3]],wave[[4]],wave[[5]],wave[[6]],
                wave[[7]],wave[[8]],wave[[9]],wave[[10]],wave[[11]],wave[[12]],
                fun = mean)

#   rmse
wrmse <- sqrt(mean(wera - wavg)^2)
plot(wrmse)


#   Extreme Still Water
#----------------------
UKtide <-  brick("data/tides/final/UKCP18_newlyn_val.nc")
mhw <- mean(UKtide)

#   Read in data 
hw_esw <- list()
for(i in 1:12){ hw_esw[[i]] <- list()}

for(i in 1:12){print(i)
  hw_esw[[i]] <- brick(paste0("data/extreme_water/newlyn/no_slr/", models[i],
                              "/esw_newlyn_", models[i],"_2000-2020.nc"))
  
  hw_esw[[i]] <- hw_esw[[i]] - mhw
}

uk_esw1 <- brick("data/UKCP18/sea_level/ukcp18_surge/processed/tide_surge_hadgem_2000-2020.nc")
uk_esw2 <- brick("data/UKCP18/sea_level/ukcp18_surge/MPI/tide_surge_MPI_2000-2020.nc")

uk_esw <- stack(uk_esw1, uk_esw2)

BISS_esw <- stack(hw_esw[[1]], hw_esw[[2]], hw_esw[[3]], hw_esw[[4]], hw_esw[[5]], hw_esw[[6]],
                  hw_esw[[7]], hw_esw[[8]], hw_esw[[9]], hw_esw[[10]], hw_esw[[11]], hw_esw[[12]])


ew_eperc <- calc(BISS_esw, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})
ew_uperc <- calc(uk_esw, fun = function(x){quantile(x, probs = c(0.5,0.95),  na.rm = T)})


ew_avg <- overlay(hw_esw[[1]],hw_esw[[2]],hw_esw[[3]],hw_esw[[4]],hw_esw[[5]],hw_esw[[6]],
                  hw_esw[[7]],hw_esw[[8]],hw_esw[[9]],hw_esw[[10]],hw_esw[[11]],hw_esw[[12]],
                  fun = mean)

uew_avg <- overlay(uk_esw1, uk_esw2, fun = mean)
uew_avg <- projectRaster(uew_avg, ew_avg)

ew_rmse <- sqrt(mean(uew_avg - ew_avg)^2)
 
ew_uperc <- projectRaster(ew_uperc, ew_eperc)

esw_5th <- ew_eperc[[1]] - ew_uperc[[1]]
esw_95th <- ew_eperc[[2]] - ew_uperc[[2]]
#   Now clamp any data with stupid high values (looking at you shetland..)
esw_95th <- clamp(esw_95th, lower = -Inf, upper = 2, useValues = F)
esw_5th <- clamp(esw_5th, lower = -Inf, upper = 1.2, useValues = F)
ew_rmse <- clamp(ew_rmse , lower = -Inf, upper = 2, useValues = F)

#   Finished datasets

# swell
#------
plot(swl_5th)
plot(swl_95th)
plot(sw_rmse)

# wave
#-----
plot(wave_5th)
plot(wave_95th)
plot(wrmse)

# esw
#-----
plot(esw_5th)
plot(esw_95th)
plot(ew_rmse)

#   Note anom is BIIS - validation - so positive indicate positive bias, negative
#   equates to negative bias.

#   Now to plot results.

#   Convert to dataframe for plotting

pix_fun <- function(input){
  
  mid <- as(input, "SpatialPixelsDataFrame")
  mid <- as.data.frame(mid)
  colnames(mid) <- c("value", "x", "y")
  
  return(mid)
  
}

ms5 <- pix_fun(swl_5th)
ms95 <- pix_fun(swl_95th)
msrm <- pix_fun(sw_rmse)

w5 <- pix_fun(wave_5th)
w95 <- pix_fun(wave_95th)
wrm <- pix_fun(wrmse)

e5 <- pix_fun(esw_5th)
e95 <- pix_fun(esw_95th)
erm <- pix_fun(ew_rmse)

fun_plot <- function(input, title, legend){
  
  ggplot(input)+
    
    geom_raster(aes(x = x, y= y, fill = value))+
    scale_fill_viridis_b(n.breaks = 6, nice.breaks = T)+
    ggtitle(title)+
    xlab("")+
    ylab("")+
    labs(fill = legend)+
    theme_classic()+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 15))+
    coord_fixed(expand = F)
} 

splot1 <- fun_plot(msrm, "Swell RMSE", "")
splot2 <- fun_plot(ms5, "Swell 5th Percentile Anomaly", "")
splot3 <- fun_plot(ms95, "Swell 95th Percentile Anomaly", "")

wplot1 <- fun_plot(wrm, "Wave RMSE", "")
wplot2 <- fun_plot(w5, "Wave 5th Percentile Anomaly", "")
wplot3 <- fun_plot(w95, "Wave 95th Percentile Anomaly", "")

eplot1 <- fun_plot(erm, "Still Water RMSE", "")
eplot2 <- fun_plot(e5, "Still Water 5th Percentile Anomaly", "")
eplot3 <- fun_plot(e95, "Still Water 95th Percentile Anomaly", "")


#   swell rmse = splot1, 5th = splot2, 95th = splot3
#   wave rmse = wplot1, 5th = wplot2, 95th=wplot3
#   esw rmse = eplot1, 5th = eplot2, 95th=eplot3


trends <- grid.arrange(splot1, wplot1, eplot1,
                       splot2, wplot2, eplot2,
                       splot3, wplot3, eplot3,
                       ncol = 3, padding = 0.01)

#   Save to disk
ggsave(plot = trends, 
       path = "C:/Users/hamis/Desktop/Uni/Dissertation/results",
       filename = "anom_rmse_swell_wave_esw.png",
       width = 40, height = 40, units = "cm")
