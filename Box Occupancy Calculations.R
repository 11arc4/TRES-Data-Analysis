library(ggplot2)
library(ggthemes)
#Calculating Occupancy numbers for each year
outerdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016"
inputdir<-paste(outerdir, "Final QC Done", sep="/")
filelist<-list.files(inputdir)

Year <- c()
BoxOccupancy <- c()
BirdTotal<-c()
BirdYear<-c()
for (a in 1:length(filelist)){
  nestfile <-filelist[a]
  nestdata <- read.csv( paste(inputdir, nestfile, sep="/"), as.is=TRUE, na.strings=c("", "NA"))
  nestdata<-AssignRenestStatus(nestdata)
  year <- nestdata$Year[1]
  #First nests= 2 new birds
  #Female and Male renests= 1 new bird
  #Unknow= 2 new birds (????)
  BirdYear[a]<- nestdata$Year[1]
  BirdTotal[a] = 2 * sum(nestdata$renest.status=="First")+
   sum(nestdata$renest.status=="Female Renest") +
    sum(nestdata$renest.status=="Male Renest") +
    2 * sum(nestdata$renest.status=="Unknown")
}
BirdNumb<-data.frame(row.names=BirdYear, BirdTotal)
 


#Calculating Box and Territory Availability for each year
BoxesAndTerritories<- read.csv(paste(outerdir, "Box Occupancy 1975-2016.csv", sep="/"), 
                               as.is=TRUE, na.strings = c("", "NA"))
#this data set is in a little bit of a funny format because it's horizontal not
#vertical but Year is the column that includes all the different grids
Box_rows<- which(grepl("Box", as.character(BoxesAndTerritories$Year)))
Boxes <- BoxesAndTerritories[Box_rows,]
YearlyBoxes <-c()
for (i in 2:length(Boxes)){
  YearlyBoxes[i-1] <- sum(Boxes[, i], na.rm=TRUE)
}
Terr_rows <- which(grepl("Territory", BoxesAndTerritories$Year))
Terr<- BoxesAndTerritories[Terr_rows,]
YearlyTerr <- c()
for (i in 2:length(Terr)){
  YearlyTerr[i-1] <- sum(Terr[, i], na.rm=TRUE)
}
Year<-seq(from=1975, to= 2016, by=1)


Availability <- data.frame(row.names=Year, YearlyTerr, YearlyBoxes)


BoxOccupancy<-merge(Availability, BirdNumb, by="row.names")
colnames(BoxOccupancy)<-c("Year", "Territories", "Boxes", "BirdTotal")
#Note that since I don't have the 1999 data yet, 1999 is left out of the box occupancy dataframe
BoxOccupancy$Year <- as.integer(BoxOccupancy$Year)


##OOOOOOO let's make a quick little graph!

ggplot(BoxOccupancy, aes(Year, BirdTotal/(2*Territories)))+
  geom_point()+
  stat_smooth()+
  xlab ("Year") +
  ylab ("Territory Occupancy") +
  theme_classic()

ggplot(BoxOccupancy, aes(Year, BirdTotal/(2*Boxes)))+
  geom_point()+
  stat_smooth()+
  xlab ("Year") +
  ylab ("Box Occupancy") +
  theme_classic()
  

