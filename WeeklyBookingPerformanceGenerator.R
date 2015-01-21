#Summarizes booking data and compares to ad hoc and Uber pricing
##Loads up and search citysprint pricing and append to booking data, also calculates summary distances (currently with some bugs)

#Load libraries
library('ggplot2')
library('ggmap')
library(plyr)
library(lubridate) #required for date and month manipulations

#Load Data
pricing <- read.csv('~/Desktop/LCYCitySprintAdHoc.csv')
allData <- read.csv('~/Desktop/bookings.csv')

#set variables

#Set fixed variables
uber = 1.5 #Uber pricing per mile for price comparison
#Black = 
dedicated = 195 #currently not in use
mileToKm = 1.609344

#extract useful data
bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries
row.names(bookings) <- NULL

###IDENTIFYING AD HOC PRICING BY MERGING TABLES
#convert postcodes into character strings (to enable string manipulation)
bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
#process postcodes for merger
bookings$POSTCODES <- sapply(bookings$to,toupper)
bookings$POSTCODES <- gsub("\\s", "", bookings$POSTCODES)#removes spaces
bookings$POSTCODES <- gsub("...$", "", bookings$POSTCODES)#removes last three characters in string
#bookings$POSTCODES <- substring(bookings$POSTCODES,1,4)#no longer needed, used to pick up first 4 characters in string
bookings$POSTCODES <- as.factor(bookings$POSTCODES)
pricing$POSTCODES <- gsub("\\s", "", pricing$POSTCODES)
pricing$POSTCODES <- as.factor(pricing$POSTCODES)

#merge two datasets by POSTCODES
pricedBookings <- merge(bookings, pricing, all.x = TRUE)
#pricedBookings <- merge(bookings, pricing)
pricedBookings <- pricedBookings[ order(as.Date(pricedBookings[,34],format = "%d/%m/%Y"), decreasing = TRUE),]
row.names(pricedBookings) <- NULL

#swap NA by 24.95 (assume zone 4)
pricedBookings$Minimum[is.na(pricedBookings$Minimum)] <- 24.95

###CALCULATE DISTANCES TRAVELLED USING GMAPS MAPDIST

#lazy initialization of data frame
pricedBookings[,c("km","hours")] <- 0
pricedBookings[1,c("km","hours")] <- mapdist(pricedBookings$from[1],pricedBookings$to[1],mode="driving")[,c("km","hours")]

#loop through postcodes
#NOTE: consider using apply family of functions to avoid loops
for(i in 2:dim(pricedBookings)[1]){
  
  #access google maps to obtain distances between origin and delivery point
  pricedBookings[i,c("km","hours")] <- mapdist(pricedBookings$from[i],pricedBookings$to[i],mode="driving")[,c("km","hours")]
}

##SAVE FULL DATABASE
#write.csv(pricedBookings, '~/desktop/pricedBookingsDatabase.csv')

##GENERATING SUMMARIES
#sorting the database by day, week and month
pricedBookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
pricedBookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
pricedBookings$week <- as.numeric( format(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y")+3, "%U" ))

#create summaries by date
pricedBookings$Outward.Journey.Delivery.Date <- as.Date(pricedBookings$Outward.Journey.Delivery.Date,format = "%d/%m/%Y")
byDate <- ddply (pricedBookings, c("Outward.Journey.Delivery.Date"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2, adHoc.Cost.Estimate = sum(Minimum), total.Distance = sum(km), total.Time = sum(hours))
byDate$Uber <- byDate$total.Distance*uber/mileToKm
byDate <- byDate[ order(byDate$Outward.Journey.Delivery.Date, decreasing = TRUE),]

#summaries by week
byWeek <- ddply (pricedBookings, c("week"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2, adHoc.Cost.Estimate = sum(Minimum), total.Distance = sum(km), total.Time = sum(hours))
byWeek$meanNetRevenue <- byWeek$netRevenue/byWeek$bookings
byWeek$Uber <- byWeek$total.Distance*uber/mileToKm

##saving the summaries
#save byDate summary
write.csv(byDate, '~/desktop/costByDate.csv')
write.csv(byWeek, '~/desktop/costByWeek.csv')

