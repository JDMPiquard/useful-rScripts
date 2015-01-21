#operational stats!

#load data
allData <- read.csv('~/Desktop/bookings.csv')

#finding pre-bookings (uses grepl)
preBook <- round(sum(grepl("prebook",allData$Department))/length(allData$Department), digits=2)*100
paste(preBook,"%")

#finding sex (uses ==)
Mr <- round(sum(allData$Title=="Mr")/length(allData$Title), digits=2)*100
paste(Mr,"%")

#finding hand luggage (use with pie chart?)
hand  <- sum(allData$Hand.Luggage.Count)
hold  <- sum(allData$Hold.Luggage.Count)

#Return Journeys
twoWay <- round(sum(allData$Journey.Mode=="Return")/length(allData$Journey.Mode), digits=2)*100
paste("Return Journeys:",twoWay,"%")

#delivery type: residential
residential <- sum(allData$Location.Organisation=="Residential")
residentialPct <- round(residential/length(allData$Location.Organisation),digits=2)*100
paste("Residential: ",residentialPct,"%")

#delivery type: airport to airport
#only just implemented, so not really possible
airToAir <- sum(allData$Journey.Direction=="AirportToAirport")
airToAirPct <- round(airToAir/length(allData$Journey.Direction),digits=2)*100
paste("Airport to airport: ",airToAirPct,"%")

#Nationality
nat.df <- ddply (allData, "Country", summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = round(mean(Total.Luggage.Count),digits=1), netRevenue = round(sum(Booking.Value)/1.2))
nat.df$avgRevenue <- round(nat.df$netRevenue/nat.df$bookings, digits=2)
nat.df <- nat.df[with(nat.df,order(-bookings,-avgRevenue)), ]
rownames(nat.df) <- NULL
View(nat.df)

#Repeat Users
reUser.df <- ddply (allData, "Email.Address", summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = round(mean(Total.Luggage.Count),digits=1), netRevenue = round(sum(Booking.Value)/1.2))
reUser.df$avgRevenue <- round(reUser.df$netRevenue/reUser.df$bookings, digits=2)
reUser.df <- reUser.df[with(reUser.df,order(-bookings,-avgRevenue)), ]
reUser.df <- reUser.df[reUser.df$bookings>1,]
reUser.df <- reUser.df[- grep("bagstorage@portr.com",reUser.df$Email.Address), ]
rownames(reUser.df) <- NULL

reUser <- length(reUser.df$bookings)
reUserpct <- round(reUser/length(allData$Is.Cancelled),digits=3)*100
paste("Repeat Users: ", reUser, "total, so about ",reUserpct,"%")

#Time of Day
