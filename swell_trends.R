#===============================================================================
#                           Swell trends 
#===============================================================================

#   In this script we wil analysis the seasonal trends in swell across the 
#   full reanalysis period (from 1959 to 2022) to determine if mean swell
#   has increased with time.

#   If we find mean swell has increased with time, then we will fit a 
#   regression model to it, allowing us to predict swell in the future,
#   and allow the model to implicitly account for changes in swell.

#-------------------------------------------------------------------------------
#                         Establishing Environment
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
library(gridExtra)
library(ggpubr)

#   Establish parallel environment

c <- detectCores() - 2
cl <- makeCluster(c)
registerDoSNOW(cl)


#   Set working directory

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/era5/swell")

#   Template UKCP18
template <- raster("C:/Users/hamis/Desktop/Uni/Dissertation/data/UKCP18/wind/east/01/uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

#   Read in data

fn <- dir()[-5:-6] # File names for read in 
swl <- list() # List to save data in

for(i in 1:length(fn)){
  
  swl[[i]] <- brick(fn[i])
  
}
 
#-------------------------------------------------------------------------------
#                           Examining trend
#-------------------------------------------------------------------------------

#   Here we will convert to dataframe to enable us to examine the trend of 
#   the data across seasons 

for(i in 1:length(swl)){
  
  #   Convert to dataframe
  
  swl[[i]] <- as.data.frame(swl[[i]], xy = T) %>% reshape2::melt(id.vars = c("x", "y"))
  
}


#   Create progress bar for foreach

pb <- txtProgressBar(min = 1, max = length(swl[[i]][,1]), style = 3)
progress <- function(n)setTxtProgressBar(pb, n) 
opts <- list(progress = progress)

#   Better ways to go about this - this is slow

year <- foreach(i = 1:length(swl[[i]][,1]),
                .options.snow = opts, 
                .verbose = F) %dopar% {
                  
                  setTxtProgressBar(pb, i) 
                  
                  strsplit(as.character(swl[[1]][,"variable"]),
                           split = ".",
                           fixed = T)[[i]][1]
  
}

stopCluster(cl)

year <- unlist(year)

for(i in 1:4){
  
  print(i)
  
  swl[[i]][,"variable"] <- year
  
  swl[[i]] <- aggregate(swl[[i]][,"value"], by = list(as.character(swl[[i]][,"variable"])), FUN = mean, na.rm = T)
  
  #swl[[i]][,"year"] <- year
  
}

for(i in 1:4){
  
  swl[[i]][,"Group.1"] <- gsub(pattern = "X", replacement = "", swl[[i]][,"Group.1"])
  
  swl[[i]][,"Group.1"] <- as.numeric(swl[[i]][,"Group.1"])
  
}

#-------------------------------------------------------------------------------
#                       Calculating R2, significance etc
#-------------------------------------------------------------------------------

#   Here we will calculate the significance of our findings - how significant 
#   is the correlation between time and increase swell? What is the R2? 

#   From this we can then project swell into the future, and test how closly our 
#   test matches future conditions. 

#   Plot up with trend lines

autumn <- ggplot(data = swl[[4]], aes(swl[[4]][,"Group.1"], swl[[4]][,"x"]))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Autumn Trend")+
  xlab("Year")+
  ylab("Signficant Swell Height (m)")


autumn <- autumn + stat_cor(method = "pearson", alternative = "g", 
                            aes(label = paste(..rr.label.., ..r.label.., ..p.label.., sep = "~','~")))


spring <- ggplot(data = swl[[1]], aes(swl[[1]][,"Group.1"], swl[[1]][,"x"]))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Spring Trend")+
  xlab("Year")+
  ylab("Signficant Swell Height (m)")

spring <- spring + stat_cor(method = "pearson", alternative = "g", 
                            aes(label = paste(..rr.label.., ..r.label.., ..p.label.., sep = "~','~")))


summer <- ggplot(data = swl[[2]], aes(swl[[2]][,"Group.1"], swl[[2]][,"x"]))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Summer Trend")+
  xlab("Year")+
  ylab("Signficant Swell Height (m)")

summer <- summer + stat_cor(method = "pearson", alternative = "g", 
                            aes(label = paste(..rr.label.., ..r.label.., ..p.label.., sep = "~','~")))


winter <- ggplot(data = swl[[3]], aes(swl[[3]][,"Group.1"], swl[[3]][,"x"]))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Winter Trend")+
  xlab("Year")+
  ylab("Signficant Swell Height (m)")

winter <- winter + stat_cor(method = "pearson", alternative = "g", 
                            aes(label = paste(..rr.label.., ..r.label.., ..p.label.., sep = "~','~")))

#   Combine into one grid

grid.arrange(winter, spring, summer, autumn)

#   Okay - that's fairly compelling, we have an increase in swell height across
#   the dataset for all seasons 

#   And then to predict future values for the autumn model:

am <- lm(formula = x ~ Group.1, data = swl[[1]])
summary(lm(formula = x ~ Group.1, data = swl[[1]]))

#   Create new data of year to run prediction off

p <- data.frame(2080)
setnames(p, new = "Group.1")

#   And run prediction

predict(am, newdata = p)

#   And that gives a predicted swell height of 1.97m - which is within previous
#   variance. Generally good result.

#   Lets write out the data frames 

names <- c("spring_2ndswell_1959-2021_df.csv",
           "summer_2ndswell_1959-2021_df.csv",
           "winter_2ndswell_1959-2021_df.csv",
           "autumn_2ndswell_1959-2021_df.csv")


for(i in 1:4){
  
  setnames(swl[[i]], new = c("Year", "Swell"))
  
  write.csv(swl[[i]], paste0("processed/", names[i]))
  
}

#-------------------------------------------------------------------------------
#                         Translating to raster
#-------------------------------------------------------------------------------

#   Now having a general feel for the correlation tests, and seeing that they
#   are all significant we can create linear regression models for each
#   grid square of a raster, and use that to predict future swell conditions

#   Read raster data back in

for(i in 1:length(fn)){
  
  swl[[i]] <- brick(fn[i])
  
}

#   Values are in months rather than seasonal means, so we want to calculate
#   that

season1 <- seq(from = 1, to = 189, by = 3) # Index to select months to mean by
season2 <- seq(from = 3, to = 189, by = 3)

season <- list()
for(i in 1:4){season[[i]] <- list()} # Lists for storage

for(i in 1:4){ # Running to aquire seasonal mean for each month
  
  for(j in 1:length(season1)){
    
    season[[i]][[j]] <- mean(swl[[i]][[season1[j]:season2[j]]])
    
  }
}

swl <- season # Remove unneeded vars
rm(season1, season2)

for(i in 1:4){ # Convert back to brick
  
  season[[i]] <- brick(season[[i]])
  
  swl[[i]] <- brick(swl[[i]])
  
  swl[[i]] <- swl[[i]][[1:41]] # Subset to leave 2000 - 2020 to test against
  
}

time <- 1:nlayers(swl[[1]]) # Number of time layers in the observations - for use in the regression model

#   Calculation for getting the slope and the intercept of the lm for each
#   grid cell on the raster

#   This fetches slope and intercept
bfun <- function(x) { if (is.na(x[1])){ c(NA, NA) } else {lm(x ~ time)$coefficients}}


#   For the next two functions to work the rasters must be in stack format.

pun <- function(x) {    # This equation fetches probability
  if (all(is.na(x))) {
    return(c(NA, NA))
  } 
  m <- lm(x ~ time)
  summary(m)$coefficients[,4]
}

un <- function(x) {     # This fetches standard error and r2
  if (all(is.na(x))) {
    return(c(NA, NA))
  } 
  m = lm(x~time)
  s  <- summary(m)
  r2 <- s$r.squared
  resid.s.e <- s$sigma
  cbind(r2, resid.s.e)
}

#   Calculate standard error, probability etc and write out.

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/results/primary_swell/regression")

names <- c("Winter", "Spring", "Summer", "Autumn")

test <- list()
for(i in 1:4){
  test[[i]] <- stack(swl[[i]])
}


t <- list()
for(i in 1:4){
  
  t[[i]] <- calc(test[[i]], pun) # probability
  writeRaster(t[[i]], paste0("p-value_", names[i], ".nc"))
  
  t[[i]] <- calc(test[[i]], un) # r2 residual error
  writeRaster(t[[i]], paste0("r2_std-error_", names[i], ".nc"))
  
}

t <- calc(test[[1]], un)

#   This runs the function and obtains the coefficient and the slope for the
#   raster brick (as 1 and 2)

coff <- list()

for(i in 1:4){
  
  coff[[i]] <- calc(swl[[i]], bfun)
  
}

#   This is an example of how it works - 121 is the 121th year from the baseline - or year 2080.
#   x[[1]] is the intercept and x[[2]] is the slope

#   So to calculate from year 2000 to 2080 (running from there to leave 2000 - 2020)
#   as a model fit test) we run from 168 to 248

project <- list()
for(i in 1:4)(project[[i]] <- list())

time <- 41:121 # Independent variable

for(i in 1:4){ # Projecting future swell from the linear relationship
  
  for(j in 1:length(time)){
    
    print(paste0("Projecting linear regression for year ", j))
    
    project[[i]][[j]] <- coff[[i]][[1]] + coff[[i]][[2]] * time[j]
    
  }
}

for(i in 1:4){ # Convert to brick
  
  print(i)
  project[[i]] <- brick(project[[i]])
  
}

#   Compare to actual results for the 2020s

plot(season[[1]][[53:63]])
plot(project[[1]][[1:10]])

obs <- season[[1]][[53:63]]
model <- project[[1]][[1:10]]

mobs <- mean(season[[1]])
mmodel <- mean(project[[1]])

mrmse <- sqrt((mmodel - mobs)^2)
plot(mrmse)

#   And that does a pretty good job - sightly too high but not bad at all.

#   Have a look at RMSE

rmse <- list()

for(i in 1:10){
 
  rmse[[i]] <- sqrt(mean(model[[i]] - obs[[i]])^2) 
  
}

rmse <- brick(rmse)
plot(rmse)

#   Alright, and again that's really a pretty good result - error is kept below
#   1m fairly consistently. To maintain the extremes within the dataset I now 
#   want to apply change factors to the future projections in regards to the 
#   1980 to 2000 swell (daily.) By doing this I will maintain the distribution 
#   seen (and the tails most importantly) of the observed swell dataset.

#-------------------------------------------------------------------------------
#                   Daily mean for obs swell 1980 - 2000
#-------------------------------------------------------------------------------

#   First read in the daily observations

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/era5/")

dswl <- brick("era5_raw/era5_wind_wave_swell_1980-2000.nc", varname = "shts")

#   They are in time slices - so need to calculate the daily mean here 
x = 1:58440
n = 7305
indices <- split(x, sort(x%%n))

#   Run in parrallel for speed

co <- detectCores() - 2
cl <- makeCluster(co)
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = length(indices), style = 3)
progress <- function(n)setTxtProgressBar(pb, n) 
opts <- list(progress = progress)

mdswl <- list()

mdswl <- foreach(i = 1:length(indices),
                  .options.snow = opts, 
                  .verbose = F, .packages = c("raster")) %dopar% {
                  
                  setTxtProgressBar(pb, i) 
                  
                  return(mean(dswl[[indices[[i]]]]))
                  
                  
                  
                  }

stopCluster(cl)

#   Convert from list to brick

mdswl <- brick(mdswl)

#   Now fit to UKCP18 

#-------------------------------------------------------------------------------
#                           Fit to UKCP18
#-------------------------------------------------------------------------------

#   For change factor bias correction we need the ERA5 dataset to fit that of 
#   UKCP18 (in essence to have 360 day years.)

leap <- seq(from = 29, by = 1460, length.out = 5) # length.out accounts for the 5 years

#   Now we index to remove these years and convert to normal years
  
mdswl <- mdswl[[-leap]]

may <- seq(from = 151, to = 7300, by = 365)     # Define the days which will be
jly <- seq(from = 211, to = 7280, by = 364)     # removed from the dataset
aug <- seq(from = 241, to = 7260, by = 363)
oct <- seq(from = 301, to = 7240, by = 362)
dec <- seq(from = 361, to = 7220, by = 361)
  
mdswl <- mdswl[[-may]]    # Now we sequentially remove the extra days
mdswl <- mdswl[[-jly]]    # to leave us with 3600.
mdswl <- mdswl[[-aug]]
mdswl <- mdswl[[-oct]]
mdswl <- mdswl[[-dec]]

mdswl <- brick(mdswl)     # Convert back to brick 
mdswl <- mdswl[[1:7200]]

#   This is now the obs to which change factors will be applied.

#   Now I need to combine the projections into one dataset

prswl <- list()

for(i in 1:80){ # This combines them into years
  
  prswl[[i]] <- list(project[[4]][[i+1]],
                     project[[2]][[i+1]],  # Seasons in order of winter to autumn
                     project[[3]][[i+1]],
                     project[[1]][[i+1]])
  
}

#   We now have a list of seasons in order for 80 years.

prswl <- unlist(prswl, recursive = F)

#   We can now use these to apply change factors

#-------------------------------------------------------------------------------
#                             Change Factors
#-------------------------------------------------------------------------------

#x2010 <- list()
x2020 <- list()
#x2030 <- list()
x2040 <- list()
#x2050 <- list()
x2060 <- list()
#x2070 <- list()
x2080 <- list()


#   I need to produce indexes to apply the seasons too

x = 1:7200
n = 80
obs_ind <- split(x, sort(x%%n))

w_ind <- list()
s_ind <- list()
su_ind <- list()
a_ind <- list()

#   Create indexs for seasons

for(i in seq(from = 1, to = 80, by = 4)){
  
  w_ind[[i]] <- obs_ind[[i]]
  s_ind[[i]] <- obs_ind[[i+1]]
  su_ind[[i]] <- obs_ind[[i+2]]
  a_ind[[i]] <- obs_ind[[i+3]]
  
}

#   Now remove null list elements

w_ind <- w_ind[-which(sapply(w_ind, is.null))]
s_ind <- s_ind[-which(sapply(s_ind, is.null))]
su_ind <- su_ind[-which(sapply(su_ind, is.null))]
a_ind <- a_ind[-which(sapply(a_ind, is.null))]

#   Create projection index

pw_ind <- seq(from = 1, to = 80, by = 4)
ps_ind <- seq(from = 2, to = 80, by = 4)
psu_ind <- seq(from = 3, to = 80, by = 4)
pa_ind <- seq(from = 4, to = 80, by = 4)


#   Apply change factors to days by season of each year

cfw20 <- list()
cfs20 <- list()     # 2000-2020
cfsu20 <- list()
cfa20 <- list()

cfw40 <- list()
cfs40 <- list()     # 2020-2040
cfsu40 <- list()
cfa40 <- list()

cfw60 <- list()
cfs60 <- list()     # 2040-2060
cfsu60 <- list()
cfa60 <- list()

cfw80 <- list()
cfs80 <- list()     # 2060-2080
cfsu80 <- list()
cfa80 <- list()


for(i in 1:20){
  
  print(paste0("Calculating for year ", i, " in period.."))
   
  cfw20[[i]] <- mdswl[[w_ind[[i]]]] + (prswl[[pw_ind[i]]] - prswl[[1]]) # The ind object at prwsl indicates
  cfs20[[i]] <- mdswl[[s_ind[[i]]]] + (prswl[[ps_ind[i]]] - prswl[[2]]) # the month which is being compared
  cfsu20[[i]] <- mdswl[[su_ind[[i]]]] + (prswl[[psu_ind[i]]] - prswl[[3]])# to the baseline
  cfa20[[i]] <- mdswl[[a_ind[[i]]]] + (prswl[[pa_ind[i]]] - prswl[[4]]) 
  
  print(paste0("2000-2020's year ", i, " complete.."))
  
  cfw40[[i]] <- mdswl[[w_ind[[i]]]] + (prswl[[pw_ind[i]+80]] - prswl[[1]]) # i+80 here moves onto the next
  cfs40[[i]] <- mdswl[[s_ind[[i]]]] + (prswl[[ps_ind[i]+80]] - prswl[[2]]) # 20 years
  cfsu40[[i]] <- mdswl[[su_ind[[i]]]] + (prswl[[psu_ind[i]+80]] - prswl[[3]])
  cfa40[[i]] <- mdswl[[a_ind[[i]]]] + (prswl[[pa_ind[i]+80]] - prswl[[4]]) 
  
  print(paste0("2020-2040's year ", i, " complete.."))
  
  cfw60[[i]] <- mdswl[[w_ind[[i]]]] + (prswl[[pw_ind[i]+160]] - prswl[[1]]) # Same logic of i+160 follows
  cfs60[[i]] <- mdswl[[s_ind[[i]]]] + (prswl[[ps_ind[i]+160]] - prswl[[2]]) 
  cfsu60[[i]] <- mdswl[[su_ind[[i]]]] + (prswl[[psu_ind[i]+160]] - prswl[[3]])
  cfa60[[i]] <- mdswl[[a_ind[[i]]]] + (prswl[[pa_ind[i]+160]] - prswl[[4]]) 
  
  print(paste0("2040's-2060's year ", i, " complete.."))
  
  cfw80[[i]] <- mdswl[[w_ind[[i]]]] + (prswl[[pw_ind[i]+240]] - prswl[[1]]) # The numbers at prswl indicate
  cfs80[[i]] <- mdswl[[s_ind[[i]]]] + (prswl[[ps_ind[i]+240]] - prswl[[2]]) # the first season - the baseline
  cfsu80[[i]] <- mdswl[[su_ind[[i]]]] + (prswl[[psu_ind[i]+240]] - prswl[[3]])
  cfa80[[i]] <- mdswl[[a_ind[[i]]]] + (prswl[[pa_ind[i]+240]] - prswl[[4]]) 
  
  print(paste0("2060's-2080's year ", i, " complete.."))
  
}

#-------------------------------------------------------------------------------
#                             Sort into decades
#-------------------------------------------------------------------------------

#   Don't really need to do this bit - easier to leave it in twenty year chunks

#   Create lists to store disparate bits in

for(i in 1:20){
  
#  x2010[[i]] <- list()
  x2020[[i]] <- list()
#  x2030[[i]] <- list()
  x2040[[i]] <- list()
#  x2050[[i]] <- list()
  x2060[[i]] <- list()
#  x2070[[i]] <- list()
  x2080[[i]] <- list()
  
}

#   Run for each decade 

#for(i in 1:10){
  
#  x2010[[i]] <- list(cfw20[[i]], cfs20[[i]], cfsu20[[i]], cfa20[[i]])
#  x2020[[i]] <- list(cfw20[[i+10]], cfs20[[i+10]], cfsu20[[i+10]], cfa20[[i+10]])
  
#  x2030[[i]] <- list(cfw40[[i]], cfs40[[i]], cfsu40[[i]], cfa40[[i]])
#  x2040[[i]] <- list(cfw40[[i+10]], cfs40[[i+10]], cfsu40[[i+10]], cfa40[[i+10]])
  
#  x2050[[i]] <- list(cfw60[[i]], cfs60[[i]], cfsu60[[i]], cfa60[[i]])
#  x2060[[i]] <- list(cfw60[[i+10]], cfs60[[i+10]], cfsu60[[i+10]], cfa60[[i+10]])
  
#  x2070[[i]] <- list(cfw80[[i]], cfs80[[i]], cfsu80[[i]], cfa80[[i]])
#  x2080[[i]] <- list(cfw80[[i+10]], cfs80[[i+10]], cfsu80[[i+10]], cfa80[[i+10]])
  
#}

#   As twenty years

for(i in 1:20){
  
  x2020[[i]] <- list(cfw20[[i]], cfs20[[i]], cfsu20[[i]], cfa20[[i]])
  
  x2040[[i]] <- list(cfw40[[i]], cfs40[[i]], cfsu40[[i]], cfa40[[i]])
  
  x2060[[i]] <- list(cfw60[[i]], cfs60[[i]], cfsu60[[i]], cfa60[[i]])
  
  x2080[[i]] <- list(cfw80[[i]], cfs80[[i]], cfsu80[[i]], cfa80[[i]])
  
  
}


#   Unlist

#x2010 <- unlist(x2010, recursive = T)
x2020 <- unlist(x2020, recursive = T)
#x2030 <- unlist(x2030, recursive = T)
x2040 <- unlist(x2040, recursive = T)
#x2050 <- unlist(x2050, recursive = T)
x2060 <- unlist(x2060, recursive = T)
#x2070 <- unlist(x2070, recursive = T)
x2080 <- unlist(x2080, recursive = T)

#   Convert to brick

#x2010 <- brick(x2010)
x2020 <- brick(x2020)
#x2030 <- brick(x2030)
x2040 <- brick(x2040)
#x2050 <- brick(x2050)
x2060 <- brick(x2060)
#x2070 <- brick(x2070)
x2080 <- brick(x2080)

#-------------------------------------------------------------------------------
#                          Reproject and Resample
#-------------------------------------------------------------------------------

#   Add all years to a list 

cf <- list(#x2010,
           x2020,
           #x2030,
           x2040,
           #x2050,
           x2060,
           #x2070,
           x2080)

#   Reproject and resample

for(i in 1:length(cf)){
  
  print(paste0("Reprojecting for decade ", i , ".."))
  
  cf[[i]] <- projectRaster(from = cf[[i]], to = template, crs = template)
  
  print(paste0("Resampling for decade ", i , ".."))
  
  cf[[i]] <- resample(cf[[i]], template)
  
}

#   Okay - and the trend generally looks alright there.

plot(cf[[4]][[3586:3600]])
plot(mdswl[[7186:7200]])

#   Now write to disk

year <- c("2000-2020", "2020-2040", "2040-2060", "2060-2080")
          #"2040-2050", "2050-2060", "2060-2070", "2070-2080")

for(i in 1:length(cf)){
  
  print(paste0("Writing decade ", year[i], " to disk.."))
  
  writeRaster(cf[[i]], paste0("change_factors/swell/swell_", year[i], ".nc"))
  
}


#===============================================================================
#                               ### END ###
#===============================================================================
