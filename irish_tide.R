#===============================================================================
#                               Irish Tides
#===============================================================================

#   In this script I will mimic the processes of the main tide loop, and in 
#   doing so create a tidal chainage for ireland. I will read these into the
#   main tide script, and produce a updated IDW layer.

#-------------------------------------------------------------------------------
#                           Establish environment
#-------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rgdal)
library(data.table)
library(raster)
library(ncdf4)
library(gstat)

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/tides/irish_tide/raw")

#   Read in data

t <- list()

names <- dir(pattern = ".csv")

cnames <- c("time","station_id","latitude","longitude","Water_Level_LAT","QC_Flag")

for(i in 1:length(names)){
  
  t[[i]] <-  fread(names[i], header = T, skip = "0", col.names = cnames)
  
}

for(i in 1:12){
  
  print(tail(t[[i]][,c("time", "station_id")]))
  
}


#-------------------------------------------------------------------------------
#                       Selecting max tide per day
#-------------------------------------------------------------------------------


#   Check values - okay there does not appear to be any no data values.

table(round(t[[1]][,"Water_Level_LAT"], 0))

#   Now I want the max for for each day - so to do so I will need to aggregate 
#   all the hours of the day together

for(i in 1:length(t)){
  
  print(paste0("stripping hours from date for ", names[i]))
  
  c <- t[[i]]
  
  #   Here we subset date from hours and day to simply day
  
  c$day <- sub(" .*", "", c$time)
  
  print(paste0("Selecting daily maximum tide for ", names[i]))
  
  #   Here we select only the max value for each day
  
  c1 <- aggregate(c$Water_Level_LAT, by = list(c$day), FUN = max)
  
  #   Add valid columns back on to the data. As these columns are the same 
  #   for every observation this is a simple process. Just copy the first
  #   row of each column to every observation of the new max daily tide
  #   dataset.
  
  c1[,3] <- c[1,2]
  c1[,4] <- c[1,3]
  c1[,5] <- c[1,4]
  
  #   Rename columns to something sensible.
  
  namer <- c("date", "tide", "site", "lat", "lon")
  
  setnames(c1, new = namer)
  
  #   Overwrite orginal data in the list.
  
  t[[i]] <- c1
  
}

#   Remove halfway datasets

rm(c, c1)

#-------------------------------------------------------------------------------
#                       Adding Lunar Day and Month
#-------------------------------------------------------------------------------

#   As the dataset length for each of these tide gauges is different it means
#   that I will have to come up with a different method for calculating 
#   lunar day and lunar month.

#   I am tempted to it by date - i.e. if date = 01/01/12 then lunar month == val

#   But I would have to do that for every day of the month which is not really
#   feasible.

#   okay - lets do it for the longest dataset and then run left joins off time
#   with the rest of them.

#   2007 is the longest

lun <- t[[3]]

#   Lets create a year field

lun$year <- gsub(pattern = "-.*", "", lun$date)

table(lun$year)

#   Okay - so the datasets for ireland are very incomplete.

#   Dummy dataset then

#   Lunar months - these alternate

l <- c(1:29, 1:30)

#   Repeat to fill out a year

l <- rep(l, 6)

#   Repeat to full out full time series (over do so we can clip to length)

l <- rep(l,16)

#   First full moon is on the 3rd of 2007 so add 29 and 30 to the first two
#   rows

l <- c(29,30,l)

#   Now build date

# First fetch days of each month (from full year)

temp <- subset(lun, lun$year == "2018")

head(temp)

#   So this is how we replace to build a year

temp$x2007 <- sub(pattern = "018*", "007", temp$date)
temp$x2008 <- sub(pattern = "018*", "008", temp$date)
temp$x2009 <- sub(pattern = "018*", "009", temp$date)
temp$x2010 <- sub(pattern = "018*", "010", temp$date)
temp$x2011 <- sub(pattern = "018*", "011", temp$date)
temp$x2012 <- sub(pattern = "018*", "012", temp$date)
temp$x2013 <- sub(pattern = "018*", "013", temp$date)
temp$x2014 <- sub(pattern = "018*", "014", temp$date)
temp$x2015 <- sub(pattern = "018*", "015", temp$date)
temp$x2016 <- sub(pattern = "018*", "016", temp$date)
temp$x2017 <- sub(pattern = "018*", "017", temp$date)
temp$x2018 <- sub(pattern = "018*", "018", temp$date)
temp$x2019 <- sub(pattern = "018*", "019", temp$date)
temp$x2020 <- sub(pattern = "018*", "020", temp$date)

#   And that gives us the full record, so we bind them together 

full <- c(temp$x2007,temp$x2008,temp$x2009,temp$x2010,temp$x2011,
          temp$x2012,temp$x2013,temp$x2014,temp$x2015,temp$x2016,
          temp$x2017,temp$x2018,temp$x2019,temp$x2020)

#   Trim to length of full date system

l <- l[1:length(full)]

#   Now join 

l <- data.frame(l, full)

#   Setnames

setnames(l, old = c("l", "full"), new = c("lunar_day", "date"))

#   Now we can join by date (as a left join)

lun <- left_join(l, lun, by = "date")
head(lun)

#   And now we want to create lunar months


l <- c(12,12)
l1 <- rep(1, 29)
l2 <- rep(2, 30)
l3 <- rep(3, 29)
l4 <- rep(4, 30)
l5 <- rep(5, 29)
l6 <- rep(6, 30)
l7 <- rep(7, 29)
l8 <- rep(8, 30)
l9 <- rep(9, 29)
l10 <- rep(10, 30)
l11 <- rep(11, 29)
l12 <- rep(12, 30)

#   Join into one vector

lm <- c(l,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)

lm

#   rm redundant 

rm(l, l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)

# Again repeat to fit data

lm <- rep(lm, 15)

lm <- lm[1:length(lun$lunar_day)]

#   Join to data

lun <- cbind(lun, lm)

#   Create record to join to all in list

lun <- data.frame(lun$date, lun$lunar_day, lun$lm)
setnames(lun, new = c("date", "lunar_day", "lunar_month"))
head(lun)
tail(lun)

#   Okay - with that looped I should be able to find the mean for each lunar
#   day by using the na.rm argument. 

for(i in 1:length(t)){
  
  t[[i]] <- left_join(lun, t[[i]], by = "date")
  
}

head(t[[1]])

test <- list()

for(i in 1:length(names)){
  
  test[[i]] <-  fread(names[i], header = T, skip = "0", col.names = cnames)
  
}

#-------------------------------------------------------------------------------
#                       Finding mean lunar day / month
#-------------------------------------------------------------------------------

t <- lapply(t, function(t){
  
  t <- aggregate(t$tide, by = list(t$lunar_day, t$lunar_month), FUN = mean, na.rm = T)
  
  t
  
})

head(t[[1]])

#   Lets rename so I know what is what 

t <- lapply(t, function(t){
  
  setnames(t, new = c("lunar_day", "lunar_month", "tide"))
  
})

head(t[[1]])

#   And now I need to append site and lat lon back onto the data

for(i in 1:length(t)){
  
  t[[i]][,"site"] <- test[[i]][1,2]
  
  t[[i]][,"lat"] <- test[[i]][1,3]
  
  t[[i]][,"lon"] <- test[[i]][1,4]
  
  print(head(t[[i]]))
  
}

#   Checking for NA's (i.e. areas where data gaps mean lunar day / month combos
#   are not covered)

for(i in 1:length(t)){
  
  print(table(is.na(t[[i]][,"tide"])))
  
}

#   And we don't have any - excellent.

#-------------------------------------------------------------------------------
#                       Aligning to UKCP18 calander
#-------------------------------------------------------------------------------

#   UKCP18 has months with 30 days - so I need to create a 30th day for all
#   months with only 29 days

table(t[[3]][,"lunar_month"])
table(t[[3]][,"lunar_day"])

#   Checking this it is always the 30h day that is missing (as would be expected)

#   The lunar months which are missing a day are the 1st, 3rd, 5th, 7th, 9th 
#   and 11th

#   We will create this extra day by taking the mean of the 29th and the 1st
#   of the bridging months

#   Creating list of lists to store results in 

x30ths <- list()

for(i in 1:length(t)){
  
  x30ths[[i]] <- list()
  
}

#   Creating months to loop though for value selection

mnt1 <- c(12, 2, 4, 6, 8, 10)
mnt2 <- c(1, 3, 5, 7, 9, 11)


for(j in 1:length(t)){
  
  for(i in 1:length(mnt1)){
    
    x30ths[[j]][[i]] <- (subset(t[[j]], t[[j]][,"lunar_month"] == mnt1[i] & 
                                  t[[j]][,"lunar_day"] == 30)[,3] +
                      
                      subset(t[[j]], t[[j]][,"lunar_month"] == mnt2[i] & 
                               t[[j]][,"lunar_day"] == 1)[,3]) / 2
  }
}

head(x30ths[[1]][[1]])

#   Okay - now I need to append relevant information to each of these vars so
#   I can join back to t


for(j in 1:length(t)){
  
  for(i in 1:length(mnt1)){
    
    x30ths[[j]][[i]] <- as.data.frame(x30ths[[j]][[i]])
    
    x30ths[[j]][[i]][,"site"] = t[[j]][1,"site"]
    x30ths[[j]][[i]][,"lat"] = t[[j]][1,"lat"]
    x30ths[[j]][[i]][,"lon"] = t[[j]][1,"lon"]
    x30ths[[j]][[i]][,"lunar_month"] = mnt2[i]
    x30ths[[j]][[i]][,"lunar_day"] = 30
    
    setnames(x30ths[[j]][[i]], new = c("tide", "site", "lat", "lon", "lunar_month", "lunar_day"))
    
    print(head(x30ths[[j]][[i]]))
     
  }
}

#   Now collapse the interior list

for(i in 1:length(t)){
  
  x30ths[[i]] <- bind_rows(x30ths[[i]])
  
}

#   And bind these onto t

for(i in 1:length(t)){
  
  t[[i]] <- bind_rows(t[[i]], x30ths[[i]])
  
}

#   Excellent - now it would be good to order the dataframes by month and day



for(i in 1:length(t)){
  
  mid <- t[[i]]
  
  mid <- mid[with(mid, order(lunar_month, lunar_day)),]
  
  t[[i]] <- mid
  
}

#   Lets check we have 360 day year anf full lunar months

tail(t[[1]])

for(i in 1:length(t)){
  
  print(table(t[[i]][,"lunar_day"]))
  
}

#   Excellent

#-------------------------------------------------------------------------------
#                             Making spatial
#-------------------------------------------------------------------------------


t <- bind_rows(t)

t <- st_as_sf(t, coords = c("lon", "lat"), crs = 4326)
  
#   And join ni gauges to the data

setwd("C:/Users/hamis/Desktop/Uni/Dissertation/data/tides/irish_tide")

ni <- st_read("ni_gauges.gpkg")

head(ni)
head(t)

#   Need to rename ni geom

setnames(ni, old = "geom", new = "geometry")
st_geometry(ni) <- "geometry"

#   And now join together

t <- bind_rows(t,ni)

#-------------------------------------------------------------------------------
#                  Splitting by lunar day and lunar month
#-------------------------------------------------------------------------------

#   For eventual interpolation (and for upcoming interval interpolation) I 
#   need a seperate temporal slice for each day of the year

day <- c(1:30)
month <- c(1:12)

#   First split by month

for(j in 1:12){
  
  test[[j]] <- subset(t, subset = t$lunar_month == month[j])
  
}

head(test[[1]][,2])

#   And now split months by days
#   So we need to do this for each month 

#   First we build nested loops, with a list containing lists for each month. 
#   Each months list then contains a dataframe for each day of the month

t1 <- list()

for(i in 1:12){
  
  t1[[i]] <- list()
  
}

#   Now we subset by lunar days 

for(j in 1:12){
  
  for(i in 1:30){
    
    c <- test[[j]]
    
    t1[[j]][[i]] <- subset(c, subset = c$lunar_day == day[i])
    
  }
}

#-------------------------------------------------------------------------------
#                             Inteval interpolaton
#-------------------------------------------------------------------------------

#   Now I run interval interpolation between each of the gauges along the 
#   chainage I have generated using the Qchainage tool in qgis. 

#   Read in chainage 

c <- st_read("irish_chainage.gpkg")

plot(c)


#   Splitting chainage into parts
#--------------------------------

cl <- list()

#   Get unique to call them

suber <- unique(c$area)
suber


#   Split

for(i in 1:length(suber)){
  
  cl[[i]] <- subset(c, subset = c$area == suber[i])
  
}

#   Calculating intervals
#--------------------------------

#   This shows the intervals which we want to run

unique(c$area)

#   And this is the same order as we wish to run intervals from.

#   Extract names from the gauge record

sites <- unique(t$site)
sites
suber

#   Now I need to order these correctly - not the order indexed below is 
#   determined by looking at the printed values in the terminal

sites1 <- c(sites[13], sites[10], sites[5], sites[12],
           
           sites[6], sites[2],  sites[4],
           
           sites[7],  sites[3],  sites[11],
           
           sites[8], sites[1],  sites[9],
           
           sites[14])


#   Okay - now I need to shift them so it loops between the two smoothly.

sites2 <- sites1[-1]

sites1 <- sites1[-14]

sites1
sites2

#   This produces a data frame of the length of each chainage between sites

a <- list()

for(i in 1:length(cl)){
  
  a[[i]] <- as.data.frame(1:dim(cl[[i]])[1])
  
  a[[i]][,2] <- suber[i]
  
  a[[i]][,3] <- cl[[i]][,2]
  
}

#   This function produces intervals from adjacent sites on a linear interval 
#   along the chainage points for each day of the year. 


lun_fun <- function(num1){
  
  #   First from which gauge at which temporal slice
  
  seq(from = as.vector(as.numeric(t[t$lunar_day == i #                    Showing day of gauge record
                                    & t$lunar_month == j #                Showing month of gauge record
                                    & t$site == sites1[num1],3], 1)[1]),# Showing site of gauge record
      
      #   Showing to which gauge at which temporal slice
      
      to = as.vector(as.numeric(t[t$lunar_day == i #                    Showing day of gauge record
                                  & t$lunar_month == j #                Showing month of gauge record
                                  & t$site == sites2[num1],3], 1)[1]), # Showing site of gauge record
      
      #   Defining the length of sequence to calculate (and so the interval jumps)
      
      length.out = dim(a[[num1]])[1])
  
}

#   Now we run the function to produce intervals for each segment for each 
#   temporal slice.

for(z in 1:13){        #  Segement (i.e. between gauges)
  
  for(j in 1:12){      #  Month
    
    for(i in 1:30){    #  day
      
      b <- lun_fun(z)
      
      print(paste0("Processing ", sites1[z], " to ", sites2[z],  " month ", j, " day ", i))
      
      a[[z]] <- cbind(a[[z]], b)
      
      setnames(a[[z]], old = "b", new = paste0("m", j, "_d", i)) 
      
    }
  }
}

#   Now we bind the list back into one dataset containing all the tide gauge
#   locations

chain <- bind_rows(a)
colnames(chain)

#   Remove uneeded details

chain <- chain[-1]

#   Rename area

setnames(chain, old = "V2", new = "area")

#   Now run a left join to the chainage points to once again make spatial.

chain <- left_join(chain, c, by = "chainage")

plot(chain$geom)

#   Remove unneeded reference numbers

chain$area.y <- NULL
setnames(chain, old = "area.x", new = "area")

chain <- st_as_sf(chain)

#   Now I need to convert between Malin Datum and Newlyn Datum

#   Right out and check in qgis

st_write(chain, "irish_intervals.gpkg", append = F)

#   Excellent - thats all good. I will now read this into the main script 
#   attach to the UK's chainage and run IDW.



