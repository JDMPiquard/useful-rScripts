#Working code
#Calculates a simple delivery route 
#by JD Nov 2014

#Inspirations
#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#

#consider adding a restriction to central london destinations, by using a distance measured from postcode WC2N 5DN (trafalgar square)

#load required libraries
library('ggplot2')
library('ggmap')

#set variables
#file <- 'FridayDeliveries.csv' #not in use
Time1 <- 12 #time slot beginning time
Time2 <- 14 #time slot finish time
day <- Mon #choose from Mon Tue Wed Thu Fri Sat Sun
Center <- 'WC2N 5DN' #sets centre point for radius, restricting the deliveries to a certain area
Radius <- 7 #sets the radius from centre point (set to 0 to not use this option)

#initializing other variables
store <- NULL

#reading data table
data <- read.csv('~/Desktop/DeliveryOnlyDataset.csv');

#extracting the values we're interested in from the table (insert code below)
#data.lite <- data[1:dim(data)[1],] #use this for all data
data.lite <- subset(data, DBT.Slot>=Time1 & DBT.Slot<Time2) # use this for focusing on a specific time slot
data.lite <- subset(data, Day == day)
row.names(data.lite) <- NULL

#convert postcodes into character strings
data.lite$Collect.postcode <- as.character(data.lite$Collect.postcode)
data.lite$Deliver.postcode <- as.character(data.lite$Deliver.postcode)

#store into a separate data frame for simplicity
test <- data.lite[,c('Collect.postcode','Deliver.postcode')]

#lazy initialization of data frame
store <- mapdist(test[1,1],test[1,2],mode="driving")

#loop through postcodes
#NOTE: consider using apply family of functions to avoid loops
for(i in 1:dim(test)[1]){
  
  #access google maps to obtain distances between origin and delivery point
  store[i,] <- mapdist(test[i,1],test[i,2],mode="driving")
}

#order by shortest time
store <- store[ order(-store[,7], decreasing = TRUE),]

#save solutions
#write.csv(store,file = "~/Desktop/distancesFridayDeliveries.csv.csv")

## Onto nested loop to find a potential route
#keep closest in separate data frame, then delete it from store
rank <- store[1,] #rank stores the final solution, ranked by order of the route
row.names(rank) <- NULL  #restarts the numbering of the data frame
store <- store[-1,] #removes the first line of the data frame

#create temp data frame
temp <- rank
t <-2 #create variable for while loop
finish <- dim(store)[1]

while(dim(rank)[1]<=finish){
  
  for(i in 1:dim(store)[1]){
    temp[i,] <- mapdist(rank[(t-1),2],store[i,2],mode="driving")
  }

  temp <- temp[ order(-temp[,7], decreasing = TRUE),]
  
  rank[t,] <- temp[1,]
  row.names(rank) <- NULL
  temp <- temp[-1,]
  row.names(temp) <- NULL
  store <- temp
  
  t <- t+1

}


write.csv(rank,file = "~/Desktop/Monday12DeliveryRoute1.csv")
