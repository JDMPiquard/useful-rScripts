#path version

##V1
#grab route from Google Maps 
legs_df  <- route("w12 9LB", "SE11 5JH", mode = "driving", alternatives = FALSE)

options('device')$device(width = 11.65, height = 4.17)

qmap(location = c(lon = mean(legs_df$startLon), lat = mean(legs_df$startLat)), zoom = 12, maprange = TRUE, maptype = 'hybrid') + geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),alpha = 3/4, size = 2, lineend ="round", colour = "blue", data = legs_df)

##V2
#attempt to join together different route stages
x  <-  c("LHR","W12 9LB","SE11 5JH")
y  <-  c("W12 9LB","SE11 5JH","LCY")

test  <- route(x[1],y[1], mode = "driving", alternatives = FALSE)

for(i in 2:dim(x)[1]){ temp  <- route(x[i],y[i], mode = "driving", alternatives = FALSE); test <- rbind(test,temp); temp  <-  0; }

##V2.1 -> for the route generator
test  <- route(rank$from[1], rank$to[1], mode = "driving", alternatives = FALSE)

for(i in 2:dim(rank)[1]){ temp  <- route(rank$from[i], rank$to[i], mode = "driving", alternatives = FALSE); test <- rbind(test,temp); temp  <-  0; }

qmap(location = c(lon = mean(test$startLon), lat = mean(test$startLat)), zoom = 12, maprange = TRUE, maptype = 'hybrid') + geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),alpha = 3/4, size = 2, lineend ="round", colour = "blue", data = test)


