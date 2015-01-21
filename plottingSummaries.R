#Plotting the summary results

library(plyr)
library(lubridate) #required for date and month manipulations
library(rCharts)

#DATA
#load data
allData <- read.csv('C:/Users/JD/Desktop/bookings.csv')
#extract useful data
bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries

#CLEAN UP DATA
#remember conversion into date, considering format
bookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
bookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
bookings$year <- year(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
#Cleaning up postCodes
bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)


#CREATING SUMMARY SUBSETS
#By Month
#summarizing booking data in relation to desired stats
sumBookings <- ddply (bookings, c("month"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings
sumBookings$monthName  <- month.abb[sumBookings$month] #getting the month name fo plotting purposes

yearBookings <- ddply (bookings, c("year"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
yearBookings$meanNetRevenue <- yearBookings$netRevenue/yearBookings$bookings

#calculating cummulatives
sumBookings  <- within(sumBookings, cum  <- cumsum(netRevenue))

#PLOTTING
#generating dataframe to plot
df1  <- data.frame(sumBookings$month, sumBookings$monthName, sumBookings$cum)
names(df1)  <- c( "month","monthName","netRevenue")
df1$type <- "cum"

df2  <- data.frame(sumBookings$month, sumBookings$monthName, sumBookings$netRevenue)
names(df2)  <- c( "month","monthName","netRevenue")
df2$type <- "month"

df  <- rbind(df1,df2)

#nPlot now woking, just click to make it pop up in browser
dude  <- nPlot(x = list(var = "monthName", sort = "month"), y = "netRevenue", data = sumBookings, type = 'multiBarChart')
dude

#multiplot
test  <- nPlot(netRevenue ~ month, group ='type', data = df, type = 'multiChart')
test$params$multi  <-  
  list(cum = list(type = 'line', yAxis = 2, colour = 'red'), 
       month = list(type='bar', yAxis = 1, colour = 'blue'))
test$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"
))
test