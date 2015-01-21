#Leaflet Postcode Plotter using Rmaps

# loading the required packages
require(shiny)
require(ggplot2)
require(ggmap)
require(rMaps)
require(lubridate)

#load data
allData <- read.csv('C:/Users/JD/Desktop/bookings.csv')
#extract useful data
bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries
bookings  <- head(bookings, n = 10)

#sorting the database by day and month
bookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date))
bookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date))

#Cleaning up postCodes
bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)

# creating a sample data.frame with your lat/lon points
df  <- geocode(bookings$to)

#create leafltet map
map <- Leaflet$new()
map$setView(c(mean(df$lat), mean(df$lon)), zoom = 12)
map$tileLayer(provider = 'Stamen.Watercolor')
#loop through postcode markers
for(i in 1:dim(df)[1]){
  map$marker(c(df[i,2],df[i,1]), bindPopup = paste(h3(bookings$to[i]), 
                                                   as.character(bookings$Outward.Journey.Deliver.To.Location.Name[i]),
                                                   "<br>", as.character(em("items:")), as.character(bookings$Total.Luggage.Count[i]),
                                                   "<br>", as.character(em("delivery time:")), as.character(bookings$Outward.Journey.Delivery.Time[i]),
                                                   "<br>", as.character(em("booking value")), as.character(bookings$Booking.Value[i]), "GBP"
                                                   ))
}
map