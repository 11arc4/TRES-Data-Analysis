
maldat<- as.data.frame(matrix(nrow=4000, ncol=19))
colnames(maldat)<- c("BirdID", "Year", "Age", "Sex", "Infected", "BloodInFreezer", "Mass", "Tarsus", 
                     "Wing", "NestboxID", "Renest", "ClutchSize", "FirstEggDate", 
                     "LastEggDate", "HatchSize", "HatchDate", "FledgeSize", "FledgeDate",
                     "WhyFailure")
#Naming table titles

#For statement does the bird have any blood collected?
r<- 0
numberBloodSamples <- 0
for (bird in as.list(globalData$birds)){
  for(year in bird$yearsSeen$as.list()){
    #if we either have already tested blood for malaria or have access to blood,
    #lets see what information we have available for that bird
    if(year$observations$length>0 | length(year$bloodInFreezer) !=0){
      #if we have blood in the freezer make a new row for this bird and this year and set blood in teh freezer to yes
      if(length(year$bloodInFreezer) !=0){
        numberBloodSamples <- numberBloodSamples + 1
        r=r+1
        maldat$BloodInFreezer[r]<- "Yes"
      }
      #If there are any observations we need figure out whether there is a
      #malaria status measurement
      if(year$observations$length>0){
        for(obs in year$observations$as.list()){
          if (obs$type=="MalariaStatus"){
            #if there is no blood in the freezer, then we need to create a
            #new line but otherwise we don't!
            if(length(year$bloodInFreezer)==0){
              r<- r+1
              maldat$BloodInFreezer[r]<- "No"
            }
            maldat$Infected[r]<- obs$status
            break #no need to keeps going--there's only one malaria status per year
            
          }
        }
        #Pull out a body measurement info if there was either blood or tested samples
        if(length(year$bloodInFreezer) !=0 | obs$type=="MalariaStatus"){
          for (obs2 in year$observations$as.list()){
            if (obs2$type=="bodymeasurement"){
              maldat$Wing[r]<- obs2$wingChord
              maldat$Mass[r]<- obs2$mass
              maldat$Tarsus[r]<- obs2$tarsus
            }
          }
          
        }
        
        
      }
      if(length(year$bloodInFreezer) !=0 | obs$type=="MalariaStatus"){
        maldat$BirdID[r] <- bird$bandID
        maldat$Sex[r]<- bird$sex
        maldat$Age[r]<- year$age
        maldat$Year[r]<- year$year
      }
      #pull out nest infomartion
      nestPointer <- year$nest$buffer[[1]] #This should pull out data from their first nest that year ONLY
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




fullDataAvail <- maldat %>% filter( (!is.na(Infected) | BloodInFreezer=="Yes")  & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(fullDataAvail$Year, xlim=c(1988, 2016), breaks=28)

TestedData<- maldat %>% filter( !is.na(Infected)   & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(TestedData$Year, xlim=c(1988, 2016), breaks=28)

UntestedFreezerSamples<- maldat %>% filter( BloodInFreezer=="Yes"   & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(UntestedFreezerSamples$Year, xlim=c(1988, 2016), breaks=28)
#Hmmm we have a lot of a couple of years but really not much to fill in the gaps-- I think this likely means that there is a hell of a lot more blood available that we just haven't catalogued yet. 
