#hotel plotter

#load required libraries
library('ggplot2')
library('ggmap')

#read hotel file
allHotels  <- read.csv('~/desktop/hotels.csv')

#set variables
Center <- 'WC2N 5DN' #sets centre point for radius, restricting the deliveries to a certain area
#plotting properties
bins = 50
em = 1

#extract useful data
allHotels$POSTCODE  <- as.character(allHotels$POSTCODE)
df  <- geocode(allHotels$POSTCODE)

allHotels  <- cbind(allHotels,df)

write.csv(allHotels,'~/desktop/hotels2.csv')

qmap(location = Center, zoom = 11, maprange = TRUE, maptype = 'roadmap') +
  geom_point(data = allHotels, aes(x = lon, y = lat, fill = RATING, size = as.factor(ceiling(as.numeric(as.character(allHotels$ROOMS))/bins)*ceiling(bins*em))), shape = 21)+
  theme(legend.position = "none")
