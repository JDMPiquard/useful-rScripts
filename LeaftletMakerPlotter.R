#Leaflet Postcode Plotter using Rmaps

# loading the required packages
require(ggplot2)
require(ggmap)
require(rMaps)

# creating a sample data.frame with your lat/lon points
postcodesies  <-  c("SE11 5JH", "SW6 2DW", "W12 9LB")
df  <- geocode(postcodesies)

#create leafltet map
map <- Leaflet$new()
map$setView(c(mean(df$lat), mean(df$lon)), zoom = 12)
map$tileLayer(provider = 'Stamen.Watercolor')
#loop through postcode markers
for(i in 1:dim(df)[1]){
  map$marker(c(df[i,2],df[i,1]), bindPopup = toString(h4(postcodesies[i])))
}
map
