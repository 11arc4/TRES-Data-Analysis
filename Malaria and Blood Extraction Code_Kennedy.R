setwd("~/Documents/HONOURS THESIS/R")

maldat<- as.data.frame(matrix(nrow=1000, ncol=18))
colnames(maldat)<- c("BirdID", "Year", "Age", "Sex", "Infected","Mass", "Tarsus", 
                     "Wing", "NestboxID", "Renest", "ClutchSize", "FirstEggDate", 
                     "LastEggDate", "HatchSize", "HatchDate", "FledgeSize", "FledgeDate",
                     "WhyFailure")
#Naming table titles

#For statement does the bird have any blood collected?
r<- 0
for (bird in as.list(globalData$birds)){
  for(year in bird$yearsSeen$as.list()){
    if(year$observations$length>0){
      for(obs in year$observations$as.list()){
        if (obs$type=="MalariaStatus"){
          r<- r+1
          maldat$Infected[r]<- obs$status
          maldat$BirdID[r] <- bird$bandID
          maldat$Sex[r]<- bird$sex
          maldat$Age[r]<- year$age
          maldat$Year[r]<- year$year
          for (obs2 in year$observations$as.list()){
            if (obs2$type=="bodymeasurement"){
              maldat$Wing[r]<- obs2$wingChord
              maldat$Mass[r]<- obs2$mass
              maldat$Tarsus[r]<- obs2$tarsus
            }
          }
        } else {
          next
        }
      }
      nestPointer <- bird$nestList$buffer[[1]] #This should pull out data from their first nest ONLY
      if(!is.null(nestPointer)){
        nest <- get(nestPointer$m_key, globalData$nests)
        maldat$NestboxID[r]<- nestPointer$m_key
        maldat$Renest[r]<- nest$renestStatus
        maldat$ClutchSize[r]<- nest$clutchSize
        maldat$FirstEggDate[r]<- nest$firstEggDate
        maldat$LastEggDate[r]<- nest$lastEggDate
        maldat$HatchSize[r]<- nest$hatchSize
        maldat$HatchDate[r]<- nest$hatchDate
        maldat$FledgeSize[r]<- nest$fledgeSize
        maldat$FledgeDate[r]<- nest$fledgeDate
        maldat$WhyFailure[r]<- nest$reasonforFailure
      }
    }
    
  }
}
write.csv(maldat, file="Malaria Data Extraction.csv", na="", row.names = F)

#Boxplot of Fledge Date by Malaria 
plot(maldat$FledgeDate~maldat$Infected)
maldat$Infected<- as.factor(maldat$Infected)  


library(ggplot2)

#ggplot #Points for ggplot EXAMPLE
ggplot(maldat, aes(Infected, FledgeDate))+
  geom_point()

#Removing NA points
library(dplyr)
maldat<- maldat %>% filter (!is.na(Infected))

ggplot(maldat, aes(Infected, Year))+
  geom_point()







