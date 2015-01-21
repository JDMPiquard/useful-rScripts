#Load up and search citysprint pricing and append to booking data

#Load Data
pricing <- read.csv('~/Desktop/LCYCitySprintAdHoc.csv')
allData <- read.csv('~/Desktop/bookings.csv')
#extract useful data
bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries
row.names(bookings) <- NULL
#convert postcodes into character strings
bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
#convert postcodes for merger
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

#save full database
write.csv(pricedBookings, '~/desktop/pricedBookingsDatabase.csv')

#create summaries by date
pricedBookings$Outward.Journey.Delivery.Date <- as.Date(pricedBookings$Outward.Journey.Delivery.Date,format = "%d/%m/%Y")

byDate <- ddply (pricedBookings, c("Outward.Journey.Delivery.Date"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2, adHoc.Cost.Estimate = sum(Minimum))
byDate <- byDate[ order(byDate$Outward.Journey.Delivery.Date, decreasing = TRUE),]
#save byDate summary
write.csv(byDate, '~/desktop/costByDate.csv')

