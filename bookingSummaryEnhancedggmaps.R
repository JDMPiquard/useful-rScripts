#simple table summary method in R, loads data from CSV file
#consider enhancing summaries by looking at more detailed month by month analysis
#implementation plan
##calculate distance travelled per booking
##delivery cost per boioking
##comparison cost with Ad Hoc (requires looking into database style merging of CitySprint pricing table)

library(plyr)
library(lubridate) #required for date and month manipulations


#load data
allData <- read.csv('~/Desktop/bookings.csv')
#extract useful data
bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries

#sorting the database by day and month
bookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date))
bookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date))

#summarizing booking data in relation to desired stats
sumBookings <- ddply (bookings, c("month"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings

#Cleaning up postCodes
bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)

#Getting Distances from Google Maps
store <- mapdist(bookings$from[1], bookings$to[1], mode="driving")

#loop
for(i in 1:length(bookings$from)){
  store[i,] <- mapdist(bookings$from[i], bookings$to[i], mode="driving")
  (i%%11==0)*Sys.sleep(2)
}
  
#Merging results into a single data frame
merged <- merge(bookings, store)

write.csv(merged,file = "~/Desktop/bookingsTimeMerge.csv")