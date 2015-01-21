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
row.names(bookings) <- NULL

#sorting the database by day and month
bookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
bookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
bookings$week <- as.numeric( format(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y")+3, "%U" ))

#manipulating all data, sorting by week
byWeek <- ddply (bookings, c("week"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
byWeek$meanNetRevenue <- byWeek$netRevenue/byWeek$bookings

#summarizing booking data in relation to desired stats
byMonth <- ddply (bookings, c("month"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
byMonth$meanNetRevenue <- byMonth$netRevenue/byMonth$bookings

write.csv(byMonth,"~/desktop/bookingsByMonth.csv")
write.csv(byWeek,"~/desktop/bookingsByWeek.csv")