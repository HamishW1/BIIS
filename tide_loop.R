#===============================================================================
#                               Tides
#===============================================================================

#   In this script we will be processing the baseline tidal data. This attempts
#   to do the following:

#   Aims
#-------

#   1.  Catagorise each recorded day of the baseline data as a corresponding
#       lunar day

#   2.  Catagorise each of these days as belonging to a lunar month

#   3.  Take the mean of each of the lunar days in each lunar month for each 
#       year of baseline data

#   4.  Use this as a "lunar phase" dataset, to simulate a model with daily
#       resolution of tides as well as atmospheric pressure.

#   Methods
#----------

#   To achieve this we will do the following:

#   1.  Find the max value for each day in the dataset (it is hourly)

#   2.  Assign each day a lunar day (corresponding with the actual past lunar
#       day) This will alternate between 29 and 30 days to keep roughly in time
#       with the solar months

#   3.  Assign each day a lunar month from 1 - 12 (so this can match up roughly
#       with the solar months)

#   4.  Aggregate the data by lunar day, lunar month and year.

#   6.  Take the mean across each (so mean of day 1 of month 1 of year 1 against
#       day 1 of month 1 of year 2/n etc)

#   8.  To match with UKCP18 I then need to add 30th day to the lunar months
#       with 29 days. I achieve this by taking the mean of the 29th of one month
#       and the 1st of the next.

#   7.  Should give final dataset as described



#-------------------------------------------------------------------------------
#                       Establishing environment
#-------------------------------------------------------------------------------

#   Libraries required

library(tidyverse)
library(sf)
library(rgdal)
library(data.table)
library(raster)
library(ncdf4)
library(gstat)

#   Set wd

setwd("C:\\Users\\hamis\\Desktop\\Uni\\Dissertation\\data\\tides\\RN-5144_1644600075795\\")

#   Read in all test data

#   Listing files from the directory

t.files <- list.files(pattern = ".csv")

#   Creating a list to save read in files into

t <- list()

#   Creating colnames 

cnames <- c("Site Name",	"Country",	"Contributor",	"Latitude",	"Longitude",
            "Coordinate System",	"Datum Information",	"Quality Control",
            "Date",	"Parameter",	 "Data value",	"QC flag")

#   Reading files in and saving in list

#   Using fread as it is significantly quicker

for(i in 1:length(t.files)){
  
  print(paste0("Reading in ", t.files[i]))
  
  t[[i]] <- fread(t.files[i], header = T, skip = "0", col.names = cnames)
  
}

#-------------------------------------------------------------------------------
#                       Selecting max tide per day
#-------------------------------------------------------------------------------

#   Here we will select only the max tide per day - screening out a lot of 
#   required data.

#   First lets look at the data

head(t[[1]])

#   Checking to see those which have been flagged as unreliable.

#*****************************************************************************
#   Need to find out what M, N and T stand for - if it is as simple as T being 
#   unreliable then I think I should nuke them early on.
#*****************************************************************************

table(t[[2]][,12])

#   First thing first - -99 is the no data value. For now we will not set that 
#*  to NA - as we want to keep all the data.

#   The dataset contains hourly data rather than daily. I want to isolate the 
#   max for each day. We can see that by looking at the below column

head(t[[2]][,9])

#   Currently we have date and hour together. I want just the day and no hour
#   to run an agg and pull max from.

for(i in 1:length(t.files)){
  
  #   First we strip the dataset down to only the relevant columns
  
  print(paste0("stripping uneeded columns from ", t.files[i]))
  
  t[[i]] <- t[[i]][,c(1, 4, 5, 6, 9, 11)]
  
  print(paste0("stripping hours from date for ", t.files[i]))
  
  c <- t[[i]]
  
  #   Here we subset date from hours and day to simply day
  
  c$day <- sub(" .*", "", c$Date)
  
  print(paste0("Selecting daily maximum tide for ", t.files[i]))
  
  #   Here we select only the max value for each day
  
  c1 <- aggregate(c$`Data value`, by = list(c$day), FUN = max)
  
  #   Add valid columns back on to the data. As these columns are the same 
  #   for every observation this is a simple process. Just copy the first
  #   row of each column to every observation of the new max daily tide
  #   dataset.
  
  c1[,3] <- c[1,1]
  c1[,4] <- c[1,2]
  c1[,5] <- c[1,3]
  
  #   Rename columns to something sensible.
  
  namer <- c("date", "tide", "site", "lat", "lon")
  
  setnames(c1, new = namer)
  
  #   Overwrite orginal data in the list.
  
  t[[i]] <- c1
  
}

#   Remove halfway datasets

rm(c, c1)

#-------------------------------------------------------------------------------
#                         Sliming dataset
#-------------------------------------------------------------------------------

#   Now I need to reduce the dataset to just the time I want to use for a 
#   baseline - this will be from 1980

#   First collapse to dataframe

df <- bind_rows(t)

#   Now create a year column

df$year <- gsub(pattern = "/.*", "", df$date)

head(df)

#   Make numeric 

df$year <- as.numeric(df$year)

#   Subset to only after 1980

df <- subset(df, subset = df$year >= 1980)

head(df)

#   Okay - So I now only have data post 1980 - but I do not have replica
#   datasets, which I will need for my lunar month methodolgy to work.

#   So I now need to split the dataframe by site, and then left join Aberdeen to
#   all.

#   So first split by site

sites <- unique(df$site)

test <- list()

for(i in 1:length(sites)){
  
  test[[i]] <- subset(df, subset = df$site == sites[i])
  
}

head(test[[32]])

#   Good. Now we left join to Aberdeen 

#   NA's creep in here - as not all datasets are the same length 

df <- test %>% reduce(left_join, by = "date")

head(df)

#   Create dataset to allow conversion from chart datum to ordnance datum

datum <- read.csv("C:/Users/hamis/Desktop/Uni/Dissertation/data/tides/chart_datum_to_newlyn.csv")

sites <- data.frame(sites)

datum <- right_join(datum, sites, by = c("site" = "sites"))
datum$dif <- as.numeric(datum$dif)

sites <- datum$site

#-------------------------------------------------------------------------------
#                       Adding Lunar Day and Month
#-------------------------------------------------------------------------------


#   First day of the lunar month was the 2nd of Jan in 1980 - handily this is close 
#   to the first. So we will just use the first in this case. 

#   Months are 1:29 (for first) and 1:30 (for 2nd) repeating as described in 
#   methods above.

#   So lets make a repeater for that 

table(df$year.x)

#   So there is 38 years

38*12  # And 456 months


#   Okay - I am not quite going to have a perfect number of days. I think
#   I can get around that though

#   Lets frst create the days for the two lunar months - 30 and 29 days

l <- c(1:29, 1:30)

#   How many repeats do we need?

#   Cant actually remember where I got these numbers from - I think the logic
#   though is that I am working put jow many lunar days I need to fill the 
#   time series I have.

14020 / 59 #   237.6271  - so 238 basically and we cut off the extra

l <- rep(l, 238)


#   New moon is on the 2nd so add 30 as first row

l <- c(30, l)

head(l)

l <- l[1:length(df$date)]

head(l)


#   And now we join to t as the lunar day

df$lunar_day <- l
head(df)

#   And now we need the lunar month need to create a datset of months that 
#   will match first

l <- 12
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

#   How many time do I need to repeat?

14020 / 354#   40 and prune

lm <- rep(lm, 40)

lm <- lm[1:length(df$date)]

df$lunar_month <- lm

head(df)

#   good - I now want to effectively split them all into their own datasets
#   again.

#-------------------------------------------------------------------------------
#                     Splitting into separate sites
#-------------------------------------------------------------------------------

#   Now need to return to the separate sites.

#   First save lunar day, month and date

d <- df$date
ld <- df$lunar_day
lm <- df$lunar_month

#   Now remove these from df

df$date <- NULL
df$lunar_day <- NULL
df$lunar_month <- NULL

#   Now split dataframe into chunks 

#   First create index

ind1 <- seq(from = 1, to = 230, by = 5)
ind1 <- ind1[1:46]

ind2 <- seq(from = 5, to = 230, by = 5)

ind1
ind2

#   Now split by index 

t <- list()

for(i in 1:length(ind1)){
  
  t[[i]] <- df[,ind1[i]:ind2[i]]
  
}

head(t[[1]])

#   Good. Now they all have wobbly names so lets fix that 

t <- lapply(t, function(t){
  
  setnames(t, new = c("tide", "site", "lat", "lon", "year"))
  
})

head(t[[10]])

#   And finally reappend lunar days and months 

t <- lapply(t, function(t){
  
  t$lunar_day <- ld
  
  t$lunar_month <- lm
  
  t$date <- d
  
  t
  
})

head(t[[10]])

#   Okay - so we still have our NA's here

#   Excellent

#-------------------------------------------------------------------------------
#                     Finding mean lunar day / month
#-------------------------------------------------------------------------------

#   Excellent - now we want to caluculate the mean for each corresponding day
#   and month of each year

#   First though lets check the values are what we would expect

View(t[[3]])

#   Worth saying that not all sites will have the same number of records.

#   And it is following a bit of tinkering it is as would be expected.

#   So now, and in order to produce one single lunar year for the site
#   we take the mean of each lunar day in regards to each lunar month

#   First set all -99 no data values as na

t <- lapply(t, function(t){
  
  t$tide <- ifelse(t$tide == -99, NA, t$tide)
  
  t
  
})

head(t[[1]])

#   Now aggregate by month and day

t <- lapply(t, function(t){
  
  t <- aggregate(t$tide, by = list(t$lunar_day, t$lunar_month), FUN = mean, na.rm = T)
  
  t
  
})

head(t[[46]])


#   Lets rename so I know what is what 

t <- lapply(t, function(t){
  
  setnames(t, new = c("lunar_day", "lunar_month", "tide"))
  
})

head(t[[1]])

#   And now I need to append site and lat lon back onto the data

for(i in 1:46){
  
  t[[i]][,"site"] <- test[[i]][1,3]
  
  t[[i]][,"lat"] <- test[[i]][1,4]
  
  t[[i]][,"lon"] <- test[[i]][1,5]
  
}

for(i in 1:length(t)){
  
  print(table(is.na(t[[i]][,3])))
  
}

head(t[[1]])

#-------------------------------------------------------------------------------
#                       Fixing NA's
#-------------------------------------------------------------------------------

#   In areas where there are less records it has not always been possible to 
#   generate a tide value for every day of the lunar month, simply because
#   the data is missing  - this typically occurs on the 30th day for Portbury
#   Moray Firth.

#   To resolve this I have isolated the NA rows, determined what the closest 
#   temporal tide values are and found the mean between these. Then I have 
#   assinged these as the3 30th day, overwriting the NA values.


check <- bind_rows(t)

table(is.na(check$tide))

#   We have aquired some NA's here - so we need to replace these with the 
#   nearest time period - ormore accuratly the mean between the two nearest

#   First check what is missing 

errs <- subset(check, subset = is.na(check$tide))

#   Okay - Portbury and Moray Firth are missing the 30th day for an assortment 
#   of months.

#   Lets isolate and fix portbury first 

#   Create lists to store the replacement values in

fixs29 <- list()
fixs1 <- list()

#   Months used to select the values (based off the incorrect values in checks)

months <- c(2,4,6,8,10)

#   Now we select the 29th and 1st days of back to back months to find the mean
#   and assign this as the 30th day

for(i in 1:length(months)){
  
  fixs29[[i]] <- subset(check, subset = check$site == "Portbury" 
                      
                      & check$lunar_day == 29 & check$lunar_month == months[i])
  
  fixs1[[i]] <- subset(check, subset = check$site == "Portbury" 
                       
                       & check$lunar_day == 1 & check$lunar_month == months[i] + 1)
  
}

#   Bind lists together 

fixs29 <- bind_rows(fixs29)
fixs1 <- bind_rows(fixs1)

#   Dataframe to store results in

p.errs <- subset(errs, subset = errs$site == "Portbury")

#   Set new day to 30

p.errs$lunar_day <- 30

p.errs$tide <- (fixs29$tide + fixs1$tide) / 2 


#   Now repeat process for Moray Firth NA's 

fixs29 <- list()
fixs1 <- list()

#   Note that months have changed

months <- c(1,3,5,7,9,11)

for(i in 1:length(months)){
  
  fixs29[[i]] <- subset(check, subset = check$site == "Moray Firth" 
                        
                        & check$lunar_day == 29 & check$lunar_month == months[i]) # here
  
  fixs1[[i]] <- subset(check, subset = check$site == "Portbury" 
                       
                       & check$lunar_day == 1 & check$lunar_month == months[i] + 1) # here
  
}

#   Bind lists together 

fixs29 <- bind_rows(fixs29)
fixs1 <- bind_rows(fixs1)

#   Dataframe to store results in

m.errs <- subset(errs, subset = errs$site == "Moray Firth")

#   Set new day to 29

m.errs$lunar_day <- 30

m.errs$tide <- (fixs29$tide + fixs1$tide) / 2 

#   Bind the corrections together 

fixs <- bind_rows(m.errs, p.errs)

#   Now we remove the NAs from the origianal dataset

check <- subset(check, subset = is.na(check$tide) == F)

check <- bind_rows(check, fixs)

table(is.na(check$tide))
table(check$site)

#   Excellent - all fixed.


#-------------------------------------------------------------------------------
#                           Making spatial
#-------------------------------------------------------------------------------

#   Now we just convert to spatial - its under lat long so it will be under 
#   the WGS84 coordinate system


check <- st_as_sf(check, coords = c("lon", "lat"), crs = 4326)
plot(check$geometry)

t <- check

#   Remove unneeded datasets

rm(check, fixs, fixs1, fixs29, p.errs, m.errs)

#st_write(t, "C:\\Users\\hamis\\Desktop\\uni\\dissertation\\data\\tides\\tide_points.gpkg")

#-------------------------------------------------------------------------------
#                  Splitting by lunar day and lunar month
#-------------------------------------------------------------------------------

#   Before we can interpolate I need to split by lunar day and lunar month -
#   in effect so I can produce a separate raster layer for each day and 
#   month. (Which can then tie up with the daily resolution of the UKCP18 
#   data.)

day <- c(1:30)
month <- c(1:12)

#   First split by month

test <- list()

for(j in 1:12){
  
  test[[j]] <- subset(t, subset = t$lunar_month == month[j])
  
}

tail(test[[1]][,2])


#   And now split months by days
#   So we need to do this for each month 

#   First we build nested loops, with a list containing lists for each month. 
#   Each months list then contains a dataframe for each day of the month

t1 <- list()

for(i in 1:12){
  
  t1[[i]] <- list()
  
}

View(t1)

for(j in 1:12){
  
  for(i in 1:30){
    
    c <- test[[j]]
    
    t1[[j]][[i]] <- subset(c, subset = c$lunar_day == day[i])
    
  }
}

View(t1)

#   And now I want to interpolate each of these lists - Lets try that quickly
#   here.

t <- bind_rows(t1)

#   Check for NA's

table(is.na(t$tide))


#-------------------------------------------------------------------------------
#                         Inteval interpolaton
#-------------------------------------------------------------------------------

#   Okay this looks like it could be reasonably complicated to work out, so I 
#   will leave it for now (it is the weekend after all...)

#   Vague plan for the future:

#   1. Check the Dynamic Coasts methods to see how exactly they interpolated
#      the values from points to raster - try and duplicate this.

#   2. Having had a look at the results from interpolation on qgis I think I 
#      need more points to produce a reasonable result.

#   3. More tide gauges are not available, so I need to do some nearest neighbor 
#      / linear interpolation on a new set of points.

#   4. With this in mind - create chainage points (or indeed steal the EA's)
#      and run liner interpolation between these points and my tide gauge points.

#   5. Save this new dataset and then run spline interpolation on them. This
#      should produce a much smoother and more realistic result. 


#   Okay - so I went for the quick and easy approach of pinching the EA's 
#   chainage points

#   This is good as I can validate against them later.

#   Okay first read them in 

setwd("C:\\Users\\hamis\\Desktop\\Uni\\Dissertation\\data")


#-------------------------------------------------------------------------------
#                    test calculate intervals for chainage
#-------------------------------------------------------------------------------

#   So in QGIS I did:

#     1. Deleted all outlying islands without a tidal gauge
#     2. Split islands with chainage gauges off from the mainland
#     3. Split chainage points into sections between the gauges (named)

#     4. Some tide points were NA - I changed these to the nearest values means


#   Now in R I can calculate the intervals by:

#     1. Splitting the chainage points into their sections by name
#     2. Renaming id as 1 to length 
#     3. Caculating the interval as above, using difference in elevation
#     4. There are a lot of these intervals, so this process must be loopable

#   Read in data
#---------------

#   Here I am making a validation run where I convert values to newlyn datum so
#   I an compare to UKCP18 for the mainland. 

#   To do so we run this calculation:

# chart datum + ( dif ) = newlyn datum

#datum1 <- t

#t <- left_join(t, datum, by = "site") 

#t$tide <- t$tide + t$dif

#t$dif <- NULL

#   This is now blanked out - as I only run it once. This will also be incomplte
#   as it does not include the irish gauges.

#   Mainland chainage

c <- st_read("tides\\mainland_chainage.gpkg")

#   Tidal gauges

#t <- st_read("tides\\tide_points.gpkg")


head(c)
head(t)

#   Splitting chainage into parts
#--------------------------------

cl <- list()

#   Get unique to call them
c$area <- ifelse(c$area == "newlyn_devonport", "devonport_newlyn", c$area)
suber <- unique(c$area)
suber


#   Split

for(i in 1:length(suber)){
  
  cl[[i]] <- subset(c, subset = c$area == suber[i])
  
}

#   Calculating intervals
#--------------------------------


#   Okay so this shows the order of the chainage points

unique(c$area)

#   And this is the order of the gauges

#   note that sites with no segments have been removed from here. 

sites <- c("Newlyn", "Ilfracombe",
           "Ilfracombe", "Hinkley Point",
           "Hinkley Point", "Portbury",
           "Avonmouth",  "Newport",
           "Newport", "Mumbles",
           "Mumbles",  "Milford Haven",
           "Milford Haven", "Fishguard",
           "Fishguard", "Barmouth",
           "Barmouth", "Holyhead",
           "Holyhead", "Llandudno",
           "Llandudno", "Liverpool, Gladstone Dock",
           "Liverpool, Gladstone Dock", "Heysham",
           "Heysham", "Workington",
           "Workington", "Portpatrick",
           "Portpatrick", "Millport", 
           "Millport", "Port Ellen (Islay)",
           "Port Ellen (Islay)", "Tobermory",
           "Tobermory", "Ullapool",
           "Ullapool", "Kinlochbervie",
           "Kinlochbervie", "Wick",
           "Wick", "Moray Firth",
           "Moray Firth", "Aberdeen",
           "Aberdeen", "Leith",
           "Leith", "North Shields",
           "North Shields", "Whitby",
           "Whitby", "Immingham",
           "Immingham", "Cromer",
           "Cromer", "Lowestoft",
           "Lowestoft", "Felixstowe",
           "Harwich", "Sheerness",
           "Sheerness", "Dover",
           "Dover", "Newhaven", 
           "Newhaven", "Portsmouth",
           "Portsmouth", "Bournemouth",
           "Bournemouth", "Weymouth",
           "Weymouth", "Devonport",
           "Devonport", "Newlyn")

#   Okay so now that is kinda easy to loop as well.

sites2 <- unique(sites)
sites3 <- c(sites2[-1], "Newlyn")

#   Here I am removing areasa with no segments - between portbury and avonmount
#   and felixstowe and harwich

sites2 <- sites2[-4]
sites2 <- sites2[-30]
sites3 <- sites3[-4]
sites3 <- sites3[-30]

suber2 <- paste0(sites2,"_", sites3)

#   Here we subset by area

for(i in 1:length(suber)){
  
  cl[[i]] <- subset(c, subset = c$area == suber[i])
  
}


#   This produces a data frame of the length of each chainage between sites

a <- list()

for(i in 1:length(cl)){
  
  a[[i]] <- as.data.frame(1:dim(cl[[i]])[1])
  
  a[[i]][,2] <- suber[i]
  
  a[[i]][,3] <- cl[[i]][,1]
  
}


#   This function produces intervals from adjacent sites on a linear interval 
#   along the chainage points for each day of the year. 


lun_fun <- function(num1){
  
  #   First from which gauge at which temporal slice
  
  seq(from = as.vector(as.numeric(t[t$lunar_day == i #                    Showing day of gauge record
                                    & t$lunar_month == j #                Showing month of gauge record
                                    & t$site == sites2[num1],3], 1)[1]),# Showing site of gauge record
      
      #   Showing to which gauge at which temporal slice
      
      to = as.vector(as.numeric(t[t$lunar_day == i #                    Showing day of gauge record
                                  & t$lunar_month == j #                Showing month of gauge record
                                  & t$site == sites3[num1],3], 1)[1]), # Showing site of gauge record
      
      #   Defining the length of sequence to calculate (and so the interval jumps)
      
      length.out = dim(a[[num1]])[1])
  
}


#   Now we run the function to produce intervals for each segment for each 
#   temporal slice.

for(z in 1:37){        #  Segement (i.e. between gauges)
  
  for(j in 1:12){      #  Month
    
    for(i in 1:30){    #  day
      
      b <- lun_fun(z)
      
      print(paste0("Processing ", sites2[z], " to ", sites3[z],  " month ", j, " day ", i))
      
      a[[z]] <- cbind(a[[z]], b)
      
      setnames(a[[z]], old = "b", new = paste0("m", j, "_d", i)) 
  
    }
  }
}


#   Now we bind the list back into one dataset containing all the tide gauge
#   locations

chain <- bind_rows(a)
colnames(chain)

#   Remove now uneeded chainage lenght details

chain[,1] <- NULL

#   Rename site

setnames(chain, old = "V2", new = "area")


#   Join to the chainage points (c) to once again make spatial 

chain <- left_join(chain, c, by = "chainage")

colnames(chain)

#   Remove unneeded reference numbers

chain$area.y <- NULL
setnames(chain, old = "area.x", new = "area")

#   Convert to sf to formailse change to spatial 

chain <- st_as_sf(chain)

#   So we check - and it works. Solved all prior issues.

#st_write(chain[,c("area", "m1_d1", "geom")], "tides\\check_chain.gpkg")

#     Okay good - now I need to attach the big island off scotland. Then 
#     run interpolation.


#===============================================================================
#                         Attaching Islands
#===============================================================================

#   Okay doke, so now I need to attach the islands to the dataset.


#   First read them in 

heb <- st_read("tides\\stornoway_chainage.gpkg")

ni <- st_read("tides\\ni_chainage.gpkg")

islay <- st_read("tides\\islay_chainage.gpkg")

iom <- st_read("tides\\iom_chaingage.gpkg")


#   Isolate tide points to the desired areas

#   iom is port erin

#   ni is portrush and bangor 

#   islay is Port Ellen (Islay)

#   Stronway is Stornoway

#   First do single gauge islands. Here we will:
#   
#   1. Join tidal gauges to chainage
#   2. Convert to wide
#   3. Join to mainland results

#   Now I want to loop the above for iom, islay and stornway

#   Create list of chainage to loop over

islands <- list(iom, islay, heb)

#   Create list of tide gauges to loop over

gauges <- list()

#   Create list of sites to loop over

sites <- c("Port Erin", "Port Ellen (Islay)", "Stornoway")

#   And now we loop through the islands with only one tidal gauge


islands_wide <- list()
isl_half <- list()

for(i in 1:3){
  
  #   First subset the tidal gauges by the island of interest
  
  print(paste0("Subsetting gauges to ", sites[i]))
  
  gauges[[i]] <- subset(t, subset = t$site == sites[i])
  
  #   Remove geom from the gauge data to allow join later

  g <- gauges[[i]]
  g$geometry <- NULL
  g <- as.data.frame(g)
  
  #   Extract the chainage data from islands to allow correct name assignation
  
  print(paste0("Extracting chainage for ", sites[i]))
  
  isl <- islands[[i]]
  isl$area <- sites[i]
  
  #   Join the gauge and the chainage by name of area
  
  print(paste0("Joining gauges and chainage for ", sites[i]))
  
  isl <- full_join(isl , g, by = c("area" = "site"))
  
  #   Remove chainage geometry and set as data table for dcast later
  
  isl$geom <- NULL
  isl <- as.data.table(isl)
  
  #   Create a month day header for dcast later
  
  isl$month_day <- ""
  
  print(paste0("Creating month and day col for ", sites[i]))
  
  #   Populate the month day column using the lunar_month and lunar_day cols
  
  for(j in 1:length(isl$area)){
    
    isl[j,6] <- paste0("m", isl[j,4], "_d", isl[j,3])
  }

  #   Convert from long to wide on the month_day col with the value of interest
  #   being tide
  
  print(paste0("Converting from long to wide on month and day col for ", sites[i]))
  
  isl <- dcast(isl, chainage ~ month_day, value.var = "tide")
  
  #   Reassign gauges and assign chains to regain geometry. 
  
  g <- gauges[[i]]
  chains <- islands[[i]]
  
  #   Set chainage as character to allow for later join
  
  isl$chainage <- as.character(isl$chainage)
  chains$chainage <- as.character(chains$chainage)

  #   Rejoin to geometry
  
  print(paste0("Rejoining chainage to geom for ", sites[i]))
  
  islands_wide[[i]] <- left_join(isl, chains, by = "chainage")
  
}

#   Collapse island list into one data table, reset geom, remove chainage,
#   and then bind to mainland

islands_wide <- bind_rows(islands_wide)
st_geometry(islands_wide) <- "geom"

plot(islands_wide$geom)

chain$chainage <- as.character(chain$chainage)
islands_wide$chainage <- as.character(islands_wide$chainage)

test <- bind_rows(chain, islands_wide)

table(test$area)
test <- st_as_sf(test)
st_geometry(test) <- "geom"

plot(test$geom)

#   Looks good - lets write it out and have a look in qgis

#st_write(test, "tides\\island_chain.gpkg")

chain <- test

rm(test)

#===============================================================================
#                         Attaching Northern Ireland
#===============================================================================

#   Now we want to run the interval function as above on the ni data to produce
#   intervals between its two points of portrush and larne 

#   After that we will also attach it to the mainland an islands chainage - this 
#   will leave us in a good postion to begin simplifying the data (to save
#   processing) before interpolating to 12km grid.

#   First lets select just the ni tidal gauges and remove their geom

t.ni <- subset(t, subset = t$site == "Bangor" | t$site == "Portrush")
t.ni$geometry <- NULL

#   Now lets create a list to store all the days of each month in

ni_chain <- list()

for(i in 1:12){
  
  ni_chain[[i]] <- list()
  
}

#   And now we run a loop to create the linear interval scale between the tide
#   gauges through the chainage.

for(i in 1:30){
  
  for(j in 1:12){
    
    #   This is the function that creates the linear scale for each day of each
    #   month.
    
    int.ni <- seq(from = t.ni[t.ni$lunar_day == i #                    Showing day of gauge record
                              & t.ni$lunar_month == j #                Showing month of gauge record
                              & t.ni$site == "Portrush",3],# Showing site of gauge record
                  
                  #   Showing to which gauge at which temporal slice
                  
                  to = t.ni[t.ni$lunar_day == i #                    Showing day of gauge record
                            & t.ni$lunar_month == j #                Showing month of gauge record
                            & t.ni$site == "Bangor",3], # Showing site of gauge record
                  
                  #   Defining the length of sequence to calculate (and so the interval jumps)
                  
                  length.out = length(ni$chainage))
    
    #   Here we bind the scale to the chainage
    
    mon_day_n <- cbind(ni, int.ni)
    
    #   Remove variables that will end up as a duplicate later on.
    
    mon_day_n$area <- NULL
    mon_day_n$geom <- NULL
    
    #   Define the names so we know which month is which.
    
    setnames(mon_day_n, old = "int.ni", new = paste0("m", month[j], "_d", day[i])) 
    
    #   Store in a list of lists
    
    ni_chain[[j]][[i]] <- mon_day_n
    
    
  }
}

#   Reduce the nested lists to one dataframe (i.e one dataframe for each month)

for(i in 1:12){
  
  ni_chain[[i]] <- ni_chain[[i]] %>% reduce(left_join, by = "chainage")
  
}

#   Reduce the list of dataframes to just one dataframe

ni_chain <- ni_chain %>% reduce(left_join, by = "chainage")

#   Join back to chainage geometry by chainage id var

ni_chain <- left_join(ni, ni_chain, by = "chainage")

#   Check

plot(ni_chain$geom)

#   Check in QGIS

#st_write(ni_chain, "tides\\ni_chain_check.gpkg")

#   And all good - ready to join to the main chain and check

ni_chain$chainage <- as.character(ni_chain$chainage)
chain <- bind_rows(chain, ni_chain)
plot(chain$geom)

#   And now write to disk as final point tide set

#st_write(chain, "tides\\complete_tidal_chain.gpkg")

#   Excellent - now I can consider how I will interpolate this - or simplfy
#   and then interpolate. 

#   Here I run IDW for just the UK tide gauges - this allows me to compare 
#   my surge results with UKCP18 

#-------------------------------------------------------------------------------
#                           Validation UK only run
#-------------------------------------------------------------------------------

#   This will be blanked after one run - I only need it as a validation set 

#r <- raster("UKCP18\\wind\\east\\01\\uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

#   Now we convert the chain to a Spatial (sp) object to allow interpolation
#   to be run on it.

#check <- as_Spatial(chain)

#   Insure the crs of both template raster and spatial object are the same

#crs(r) <- crs(chain)

#   Now we loop the creation of a gstat object from which to interpolate 
#   values, and save the interpolated values in a list.

#   Create object to allow calling names for formula

#mon_day <- colnames(chain[3:362])
#mon_day <- mon_day[-361]

#   Create list to save interpolated results for each day in 

#idw_tide <- list()

#   Run loop

#for(i in 1:360){
  
  #   Creating gstat object to interpolate from
  
#  print(paste0("Creating gstat object for ", mon_day[i]))
  
#  midway <- gstat(formula = as.formula(paste0(mon_day[i], "~", 1)),
                  
#                  data = chain)
  
  #   Interpolating from gstat object to template raster and saving in list
  
  #   Note the power value used for this is 2 (same as automatic in QGIS
  #   gdal) The power essentially means points must be close to each other
  #   to have an effect.
  
#  print(paste0("Interpolating to raster for ", mon_day[i]))
  
#  idw_tide[[i]] <- interpolate(r, midway)
  
#}

#   Check

#plot(idw_tide[[36]])

#   At this point I now move the dates to fit UKCP18
#---------------------------------------------------

#   To do so we move the 12th month to the beginning of the dataset 

#idw_tide <- brick(idw_tide)
#dec <- idw_tide[[331:360]]

#dec <- brick(dec)
#idw_tide <- idw_tide[[-331:-360]]

#idw_tide <- stack(dec, idw_tide)
#idw_tide <- brick(idw_tide)

#   And now write this out as the tide validation dataset 

#writeRaster(idw_tide, "tides\\final\\UKCP18_newlyn_val.nc", overwrite = T)

#===============================================================================
#                         Attaching Ireland
#===============================================================================

#   I have already run all the processing for the seperate Irish chainage 
#   in the dedicated script. Here we simply read it in and attach.

irsh <- st_read("tides/irish_tide/irish_intervals.gpkg")

#   Align col values

irsh$chainage <- as.character(irsh$chainage)

#   Check crs align

crs(irsh)
crs(chain)

#   Project irsh to osgb grid

irsh <- st_transform(irsh, crs = st_crs(chain))

chain <- bind_rows(chain, irsh)

plot(chain)


#===============================================================================
#               Inverse Distance Weighted Interpolation 
#===============================================================================

#   Here we will use inverse distance weighted interpolation to create a 
#   spatially continuous dataset for tide. To do this we will use the 
#   gstat package.

#   At this point I am just going to run it to create the dataset - but I 
#   should probably also consider 


#   Okay doke - the below works - so now I just need to run it on a loop

#   First read in the raster we will use as a template - this is the UKCP18
#   atmospheric pressure on a 12km grid. We only read in one as a template

r <- raster("UKCP18\\wind\\east\\01\\uas_rcp85_land-rcm_uk_12km_01_day_19801201-19901130.nc")

#   Now we convert the chain to a Spatial (sp) object to allow interpolation
#   to be run on it.

check <- as_Spatial(chain)

#   Insure the crs of both template raster and spatial object are the same

crs(r) <- crs(chain)

#   Now we loop the creation of a gstat object from which to interpolate 
#   values, and save the interpolated values in a list.

#   Create object to allow calling names for formula

mon_day <- colnames(chain[3:362])
mon_day <- mon_day[-361]

#   Create list to save interpolated results for each day in 

idw_tide <- list()

#   Run loop

for(i in 1:360){
  
  #   Creating gstat object to interpolate from
  
  print(paste0("Creating gstat object for ", mon_day[i]))
  
  midway <- gstat(formula = as.formula(paste0(mon_day[i], "~", 1)),
                  
                  data = chain)
  
  #   Interpolating from gstat object to template raster and saving in list
  
  #   Note the power value used for this is 2 (same as automatic in QGIS
  #   gdal) The power essentially means points must be close to each other
  #   to have an effect.
  
  print(paste0("Interpolating to raster for ", mon_day[i]))
  
  idw_tide[[i]] <- interpolate(r, midway)
  
}

#   Check

plot(idw_tide[[36]])

#   At this point I now move the dates to fit UKCP18
#---------------------------------------------------

#   To do so we move the 12th month to the beginning of the dataset 


idw_tide <- brick(idw_tide)
dec <- idw_tide[[331:360]]

dec <- brick(dec)
idw_tide <- idw_tide[[-331:-360]]

idw_tide <- stack(dec, idw_tide)
idw_tide <- brick(idw_tide)

#   Probably worth having percentiles here as well - so min mean and max

tide_max <- max(idw_tide)
plot(tide_max)

tide_mean <- mean(idw_tide)
plot(tide_mean)

tide_min <- min(idw_tide)
plot(tide_min)


#   Fetch actual percentiles 

percentiles <- calc(idw_tide, fun = function(x){
  
  quantile(x, probs = c(0.9,0.95,1),  na.rm = T)})

#   And now we write to disk

writeRaster(idw_tide, "tides\\final\\idw_tide_ire.nc", overwrite = T)
writeRaster(percentiles, "tides\\final\\tide_perc.nc", overwrite = T)
writeRaster(tide_mean, "tides\\final\\mean_tide_ire.tif", overwrite = T)

#   And thats all folks - Job is a goodun 

#   Some points to think of:

#   1. May be worth having less extreme percentiles rather than, min, mean and
#      max

#   2. Likely will be useful to have some kind of confidence interval map, to 
#      illustrate areas of greater uncertinty

#   3. Will almost certintly need to calculate RMSE against the Met Office 
#      dataset to evaluate model skill

#===============================================================================
#                                 END
#===============================================================================





