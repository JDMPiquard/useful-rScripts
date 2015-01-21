#checking expressions 
#load data
allData <- read.csv('~/Desktop/bookings.csv')

#Define expression to look up
LCY <- "London City Airport"
LHR  <- "Heathrow"
LGW <- "Gatwick"
STN <- "Stansted"
Sto <- "Storage"

#use grepl to check if strings contain the above expressions
allData$filter  <- grepl(Sto,allData$Outward.Journey.Collect.From.Location.Name,ignore.case=TRUE)|grepl(Sto,allData$Outward.Journey.Deliver.To.Location.Name,ignore.case=TRUE)
#grepl("London City Airport|Heathrow",allData$Outward.Journey.Collect.From.Location.Name,ignore.case=TRUE)
subSet <- allData[allData$filter == 1,]
