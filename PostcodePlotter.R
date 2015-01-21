#Automatic Postcode Plotter

# loading the required packages
require(ggplot2)
require(ggmap)

# creating a sample data.frame with your lat/lon points
postcodesies  <-  c("SE11 5JH", "SW6 2DW", "W12 9LB")
df  <- geocode(postcodesies)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 12,
                      maptype = "roadmap", scale = 2)
#note that this map plotting technique is crude, in that it takes it does try and center the map according to the mean postcode location, but does not set the zoom level accordingly

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red"), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) #add alpha = 0.8 after fill = "red", for transparency of points

