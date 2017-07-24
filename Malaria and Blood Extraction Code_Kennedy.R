
maldat<- as.data.frame(matrix(nrow=4000, ncol=20))
colnames(maldat)<- c("BirdID", "Year", "Age", "Sex", "Infected", "BloodInFreezer", "Mass", "Tarsus", 
                     "Wing", "NestboxID", "Renest", "ClutchSize", "FirstEggDate", 
                     "LastEggDate", "HatchSize", "HatchDate", "FledgeSize", "FledgeDate",
                     "WhyFailure", "Experiment")
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
        maldat$Experiment[r]<- nest$experiment
      }
      
    }
    
  }
}
write.csv(maldat, file="Malaria Data Extraction.csv", na="", row.names = F)

maldat$Infected<- as.factor(maldat$Infected)


fullDataAvail <- maldat %>% filter( (!is.na(Infected) | BloodInFreezer=="Yes")  & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(fullDataAvail$Year, xlim=c(1988, 2016), breaks=28)

TestedData<- maldat %>% filter( !is.na(Infected)   & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(TestedData$Year, xlim=c(1988, 2016), ylim=c(0, 30), breaks=28)

UntestedFreezerSamples<- maldat %>% filter( BloodInFreezer=="Yes"   & Age != "HY", Sex != "U", !is.na(ClutchSize), !is.na(HatchSize), !is.na(FledgeSize) )
hist(UntestedFreezerSamples$Year, xlim=c(1988, 2016), breaks=28)
#Hmmm we have a lot of a couple of years but really not much to fill in the gaps-- I think this likely means that there is a hell of a lot more blood available that we just haven't catalogued yet. 


NestlingUntested <- maldat %>% filter( BloodInFreezer=="Yes"   & Age == "HY")
hist(NestlingUntested$Year, xlim=c(1988, 2016), breaks=28)


TestedData_alladult<- maldat %>% filter( !is.na(Infected)   & Age != "HY" )
hist(TestedData_alladult$Year, xlim=c(1988, 2016),ylim=c(0, 30), breaks=28)



#If the bird was from a control nest then we didn't manipulate it and it can be included easily
maldat$Experiment [which(maldat$Experiment=="Control" | maldat$Experiment=="C")]<- NA
#ONLY USING THE NEST THAT WEREN'T EXPERIMENTED WITH. ULTIMATELY MAY BE ABLE TO
#USE NESTS THAT WERE EXPERIMENTED WITH DEPENDING ON THE EXPERIMENT BUT FOR NOW
#WE'LL PLAY IT SUPER SAFE
FMalDat <- maldat%>% filter(Sex=="F" & !is.na(Infected)&is.na(Experiment))
MMalDat <- maldat%>% filter(Sex=="M" & !is.na(Infected)& is.na(Experiment))

ggplot(data=FMalDat, aes(x=Infected, y=ClutchSize))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("Females")

ggplot(data=MMalDat, aes(x=Infected, y=ClutchSize))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("Males")

ggplot(data=MMalDat, aes(x=Year, y=ClutchSize, color=Infected))+
  geom_jitter()+
  geom_smooth()+
  ggtitle("Males")

ggplot(data=FMalDat, aes(x=Year, y=ClutchSize, color=Infected))+
  geom_jitter()+
  geom_smooth()+
  ggtitle("Females")
#clutchsize might be trending downward in uninfected females?



FClutchDat <- FMalDat %>% filter(!is.na(Age) & !is.na(Year) &!is.na(ClutchSize))
mod <- glm(ClutchSize~Infected*Age*Year, data=FClutchDat, family="poisson" )
plot(mod)
#There are a number of leverage points but otherwise things look pretty decent!
#Scale location plot could be better but also could be worse

plot(resid(mod)~FClutchDat$Infected) #perfect
plot(resid(mod)~FClutchDat$Year) #That also looks fine to me
plot(resid(mod)~as.factor(FClutchDat$Age)) 


FClutchDat$Age2 [which(FClutchDat$Age=="SY")]<- "SY"
FClutchDat$Age2 [which(FClutchDat$Age!="SY")]<- "ASY"


mod <- glm(ClutchSize~Infected*Age2*Year, data=FClutchDat, family="poisson" )
#Scale location plot is still kind of shitty but perhaps we can fix that. Not 
#really sure why the residuals are spread wider for larger predicted values. If
#you ignore the red line and focus on the dots its not that bad looking
plot(resid(mod)~as.factor(FClutchDat$Age2)) #looks good as do the other plots
#I think this one is a decent fit!

Anova(mod) #Wow nothing is significant at all!
options(na.action = "na.fail")
dredge(mod)
#OK so again nothing is in the top model but infected and age are both in the best models group so maybe all is not lost!