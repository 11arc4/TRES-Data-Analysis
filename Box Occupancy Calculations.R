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
gridlist <- list(
  HUBirdTotal <- c(),
  NBBirdTotal <- c(),
  NESBirdTotal <-c(),
  SPBirdTotal <-c(),
  SRBBirdTotal <- c(),
  BGBirdTotal_prelim <-c(),
  GCBirdTotal_prelim <-c(),
  BGBirdTotal <-c()
)
GridIDCodes<- c("HU", "NB", "NE", "SP", "SR",  "BG", "GC" ) 

for (a in 1:length(filelist)){
  nestfile <-filelist[a]
  nestdata <- read.csv( paste(inputdir, nestfile, sep="/"), as.is=TRUE, na.strings=c("", "NA"))
  nestdata<-AssignRenestStatus(nestdata)
  year <- nestdata$Year[1]
  #First nests= 2 new birds
  #Female and Male renests= 1 new bird
  #Unknow= 2 new birds (????)
  BirdYear[a]<-year
  BirdTotal[a] = 2 * sum(nestdata$renest.status=="First")+
    sum(nestdata$renest.status=="Female Renest") +
    sum(nestdata$renest.status=="Male Renest") +
    2 * sum(nestdata$renest.status=="Unknown")
  
  nestdata$Grid<- substring(nestdata$BoxID, first=1, last=2)
  b=0
  for (grid in gridlist ){
    b=b+1
    rows<-which(nestdata$Grid==GridIDCodes[b]) 
    GridData<- nestdata[rows,]
    gridlist[[b]][a] <- 2 * sum(GridData$renest.status=="First")+
      sum(GridData$renest.status=="Female Renest") +
      sum(GridData$renest.status=="Male Renest") +
      2 * sum(GridData$renest.status=="Unknown")
  }
}

BirdNumb<-data.frame(row.names=BirdYear, BirdTotal)

BirdNumb$HUBirdTotal <-gridlist[[1]]
BirdNumb$NBBirdTotal <- gridlist[[2]]
BirdNumb$NESBirdTotal <-gridlist[[3]]
BirdNumb$SPBirdTotal <-gridlist[[4]]
BirdNumb$SRBBirdTotal <- gridlist[[5]]
BirdNumb$BGBirdTotal<-gridlist[[6]] + gridlist[[7]]



#Calculating Box and Territory Availability for each year
BoxesAndTerritories<- read.csv(paste(outerdir, "Box Occupancy 1975-2016.csv", sep="/"), 
                               as.is=TRUE, na.strings = c("", "NA"))
rownames(BoxesAndTerritories)<-BoxesAndTerritories$Year
BoxesAndTerritories<- subset(BoxesAndTerritories, select= -c(Year))
#this data set is in a little bit of a funny format because it's horizontal not
#vertical but Year is the column that includes all the different grids

#Need to replace all NAs with 0
BoxesAndTerritories[is.na(BoxesAndTerritories)]<-0

Box_rows<- which(grepl("Box", row.names(BoxesAndTerritories)))
Boxes <- BoxesAndTerritories[Box_rows,]
YearlyBoxes <-c()
for (i in 1:length(Boxes)){
  YearlyBoxes[i] <- sum(Boxes[, i], na.rm=TRUE)
}
HUBoxes <- unname(unlist(Boxes["Hughson's Box", ]))
NBBoxes <- unname(unlist(Boxes["Newbarn Box", ]))
NESBoxes <- unname(unlist(Boxes["North East Sanctuary Box", ]))
SPBoxes <- unname(unlist(Boxes["Sandpit Box", ]))
SRBBoxes <- unname(unlist(Boxes["SRB Box", ]))





Terr_rows <- which(grepl("Territory", row.names(BoxesAndTerritories)))
Terr <- BoxesAndTerritories[Terr_rows,]
#These territories include BG, GC, and Seans Grid boxes!  SG doesn't have it's
#own territories because they were just added in to make BG territories have 2
#boxes. IE their territories are already counted if we look at just BG boxes
BGBoxes <-unname( unlist(Boxes["Bridgets/Golf Course Box", ])) +  unname(unlist(Boxes["Seans Box", ]) )


YearlyTerr <- c()

for (i in 1:length(Terr)){
  YearlyTerr[i] <- sum(Terr[, i], na.rm=TRUE)
}
HUTerritories <- unname(unlist(Terr["Hughson's Territory", ]))
NBTerritories <- unname(unlist(Terr["Newbarn Territory", ]))
NESTerritories <- unname(unlist(Terr["North East Sanctuary Territory", ]))
SPTerritories <- unname(unlist(Terr["Sandpit Territory", ]))
SRBTerritories <- unname(unlist(Terr["SRB Territory", ]))
#These territories include BG, GC, and Seans Grid boxes!  SG doesn't have it's
#own territories because they were just added in to make BG territories have 2
#boxes. IE their territories are already counted if we look at just BG boxes
BGTerritories <-unname(unlist(Terr["Bridgets/Golf Course Territory", ])) + unname(unlist(Terr["Seans Territory", ]))



Year<-seq(from=1975, to= 2016, by=1)


Availability <- data.frame(row.names=Year, YearlyTerr, YearlyBoxes, 
                           HUBoxes, HUTerritories, 
                           NBBoxes, NBTerritories,
                           NESBoxes, NESTerritories, 
                           SPBoxes, SPTerritories, 
                           SRBBoxes, SRBTerritories, 
                           BGBoxes, BGTerritories
                           )


BoxOccupancy<-merge(Availability, BirdNumb,  by="row.names")
colnames(BoxOccupancy)[1]<- "Year"
#Note that since I don't have the 1999 data yet, 1999 is left out of the box occupancy dataframe
BoxOccupancy$Year <- as.integer(BoxOccupancy$Year)
BoxOccupancy$BoxOccTotal<- BoxOccupancy$BirdTotal/(2*BoxOccupancy$YearlyBoxes)
BoxOccupancy$BoxOccHU <-  BoxOccupancy$HUBirdTotal/(2*BoxOccupancy$HUBoxes)
BoxOccupancy$BoxOccNB <-  BoxOccupancy$NBBirdTotal/(2*BoxOccupancy$NBBoxes)
BoxOccupancy$BoxOccNES <-  BoxOccupancy$NESBirdTotal/(2*BoxOccupancy$NESBoxes)
BoxOccupancy$BoxOccSP <-  BoxOccupancy$SPBirdTotal/(2*BoxOccupancy$SPBoxes)
BoxOccupancy$BoxOccSRB <-  BoxOccupancy$SRBBirdTotal/(2*BoxOccupancy$SRBBoxes)
BoxOccupancy$BoxOccBG <-  BoxOccupancy$BGBirdTotal/(2*BoxOccupancy$BGBoxes)

BoxOccupancy$TerrOccTotal<- BoxOccupancy$BirdTotal/(2*BoxOccupancy$YearlyTerr)
BoxOccupancy$TerrOccHU<- BoxOccupancy$HUBirdTotal/(2*BoxOccupancy$HUTerritories)
BoxOccupancy$TerrOccNB<- BoxOccupancy$NBBirdTotal/(2*BoxOccupancy$NBTerritories)
BoxOccupancy$TerrOccNES<- BoxOccupancy$NESBirdTotal/(2*BoxOccupancy$NESTerritories)
BoxOccupancy$TerrOccSP<- BoxOccupancy$SPBirdTotal/(2*BoxOccupancy$SPTerritories)
BoxOccupancy$TerrOccSRB<- BoxOccupancy$SRBBirdTotal/(2*BoxOccupancy$SRBTerritories)
BoxOccupancy$TerrOccBG<- BoxOccupancy$BGBirdTotal/(2*BoxOccupancy$BGTerritories)






#Basic graph of box occupancy based on 
baseBoxGraph <- ggplot(BoxOccupancy, aes(x=Year, y= value, color=variable))+
  geom_point(aes(Year, y=BoxOccTotal, color="BoxOccTotal"), show.legend = F)+
  stat_smooth(aes(Year, y=BoxOccTotal, color="BoxOccTotal"))+
  xlab ("Year") +
  ylab ("Box Occupancy") +
  theme_classic()+
  theme(text = element_text(size=20)+axis.text.x = element_text(angle=90, hjust=1))

#Add on Hughson's
baseBoxGraph +
  geom_point(aes(Year, y=BoxOccHU, color="BoxOccHU"))+
  stat_smooth(aes(Year, y=BoxOccHU, color="BoxOccHU"), se=FALSE)

#Add on Newbarn

baseBoxGraph+
  geom_point(aes(Year, y=BoxOccNB, color= "BoxOccNB"))+
  stat_smooth(aes(Year, y=BoxOccNB, color= "BoxOccNB"))

#Add on NES
baseBoxGraph+
  geom_point(aes(Year, y=BoxOccNES, color= "BoxOccNES"))+
  stat_smooth(aes(Year, y=BoxOccNES, color= "BoxOccNES"))

#Add on SP
baseBoxGraph+
  geom_point(aes(Year, y=BoxOccSP, color= "BoxOccSP"))+
  stat_smooth(aes(Year, y=BoxOccSP, color= "BoxOccSP"))

#Add on SRB
baseBoxGraph+
  geom_point(aes(Year, y=BoxOccSRB, color= "BoxOccSRB"))+
  stat_smooth(aes(Year, y=BoxOccSRB, color= "BoxOccSRB"))
#Add on BG
baseBoxGraph+
  geom_point(aes(Year, y=BoxOccBG, color= "BoxOccBG"))+
  stat_smooth(aes(Year, y=BoxOccBG, color= "BoxOccBG"))



#Every grid
allgridBoxGraph <- baseBoxGraph +
  stat_smooth(aes(Year, y=BoxOccHU, color="BoxOccHU"), se=FALSE)+
  stat_smooth(aes(Year, y=BoxOccNB, color= "BoxOccNB"), se=FALSE)+
  stat_smooth(aes(Year, y=BoxOccNES, color= "BoxOccNES"), se=FALSE)+
  stat_smooth(aes(Year, y=BoxOccSP, color= "BoxOccSP"), se=FALSE)+
  stat_smooth(aes(Year, y=BoxOccSRB, color= "BoxOccSRB"), se=FALSE)+
  stat_smooth(aes(Year, y=BoxOccBG, color= "BoxOccBG"), se=FALSE)+
  scale_color_discrete(name="Grid",
                      breaks=c("BoxOccHU", "BoxOccNB", "BoxOccNES", "BoxOccSP", "BoxOccSRB","BoxOccBG",  "BoxOccTotal"),
                      labels=c("Hughson's", "New Barn", "North-East Sanctuary", "Sandpit", "SRB", "Bridget's", "All grids"))









#Let's make that basic Terretory boxplot
baseTerrGraph <- ggplot(BoxOccupancy, aes(x=Year, y=value, color=variable))+
  geom_point(aes(y=TerrOccTotal, color="TerrOccTotal"), show.legend = F)+
  stat_smooth(aes(y=TerrOccTotal, color="TerrOccTotal"))+
  xlab ("Year") +
  ylab ("Territory Occupancy") +
  theme_classic()

#Add on Hughson's
baseTerrGraph +
  geom_point(aes(y=TerrOccHU, color="TerrOccHU"))+
  stat_smooth(aes(y=TerrOccHU, color="TerrOccHU"))

#Add on Newbarn
baseTerrGraph +
  geom_point(aes(y=TerrOccNB, color="TerrOccNB"))+
  stat_smooth(aes(y=TerrOccNB, color="TerrOccNB"))

#Add on NES
baseTerrGraph +
  geom_point(aes(y=TerrOccNES, color="TerrOccNES"))+
  stat_smooth(aes(y=TerrOccNES, color="TerrOccNES"))

#Add on SP
baseTerrGraph +
  geom_point(aes(y=TerrOccSP, color="TerrOccSP"))+
  stat_smooth(aes(y=TerrOccSP, color="TerrOccSP"))

#Add on SRB
baseTerrGraph +
  geom_point(aes(y=TerrOccSRB, color="TerrOccSRB"))+
  stat_smooth(aes(y=TerrOccSRB, color="TerrOccSRB"))

#Add on BG
baseTerrGraph +
  geom_point(aes(y=TerrOccBG, color="TerrOccBG"))+
  stat_smooth(aes(y=TerrOccBG, color="TerrOccBG"))

#That one is fascinating. It's soooo different from the BG box occupancy, which
#parallels the total occupancy so closely


allgridTerrGraph <- baseTerrGraph +
  stat_smooth(aes(Year, y=TerrOccHU, color="TerrOccHU"), se=FALSE)+
  stat_smooth(aes(Year, y=TerrOccNB, color= "TerrOccNB"), se=FALSE)+
  stat_smooth(aes(Year, y=TerrOccNES, color= "TerrOccNES"), se=FALSE)+
  stat_smooth(aes(Year, y=TerrOccSP, color= "TerrOccSP"), se=FALSE)+
  stat_smooth(aes(Year, y=TerrOccSRB, color= "TerrOccSRB"), se=FALSE)+
  stat_smooth(aes(Year, y=TerrOccBG, color= "TerrOccBG"), se=FALSE)+
  scale_color_discrete(name="Grid",
                       breaks=c("TerrOccHU", "TerrOccNB", "TerrOccNES", "TerrOccSP", "TerrOccSRB", "TerrOccBG", "TerrOccTotal"),
                       labels=c("Hughson's", "New Barn", "North-East Sanctuary", "Sandpit", "SRB","Bridgets" ,"All grids"))


PresentationBoxOcc 
ggplot(BoxOccupancy, aes(x=Year, y= value), show.legend=F)+
  geom_point(aes(Year, y=BoxOccTotal, color="BoxOccTotal"), show.legend = F)+
  stat_smooth(aes(Year, y=BoxOccTotal, color="BoxOccTotal"), show.legend = F)+
  xlab ("Year") +
  ylab ("Box Occupancy") +
  theme_classic()+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))
