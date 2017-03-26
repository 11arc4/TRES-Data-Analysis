#Looking at recruitment a bit more closely
library(ggplot2)
library(dplyr)
library(reshape2)
#It's a clear issue that we don't catch ALL of the adults every year. 
#How many returning birds have a gap year?--they were almost definitely present that year but we didn't catch them. 

birdsSeenConsecutively <- 0
birdsSeenMissingYears <- 0

years<- seq(from=1975, to=2016, by=1)
BirdsSeen<- data.frame(years)
BirdsSeen$AdultsSeen <- rep(0, length(BirdsSeen$years)) #number of breeding adults we caught that year
BirdsSeen$FAdultsSeen <- rep(0, length(BirdsSeen$years)) #number of breeding adults we caught that year
BirdsSeen$MAdultsSeen <- rep(0, length(BirdsSeen$years)) #number of breeding adults we caught that year

BirdsSeen$NestlingsFledged <- rep(0, length(BirdsSeen$years)) #number of nestlings fledged from known nests
BirdsSeen$NestsWFledgeSizeKnown <- rep(0, length(BirdsSeen$years)) #number of nests where we know how many fledged!
BirdsSeen$BandedFledgelings <- rep(0, length(BirdsSeen$years)) # max number of the nestlings that we think were banded prior to fledging (should be pretty similar to number of nestlings fledged)

BirdsSeen$AdultsUnseen <- rep(0, length(BirdsSeen$years))
#number of adults we know were there because they returned but were not caught that year!
BirdsSeen$FAdultsUnseen <- rep(0, length(BirdsSeen$years))
BirdsSeen$MAdultsUnseen <- rep(0, length(BirdsSeen$years))

#Recruited nestlings is the number of nestlings from this year that recruited ANYTIME in the future
BirdsSeen$RecruitedNestlings <- rep(0, length(BirdsSeen$years))
BirdsSeen$FRecruitedNestlings <- rep(0, length(BirdsSeen$years))
BirdsSeen$MRecruitedNestlings <- rep(0, length(BirdsSeen$years))
BirdsSeen$FirstNests <- rep(0, length(BirdsSeen$years)) 
BirdsSeen$MaleRenests <- rep(0, length(BirdsSeen$years))
BirdsSeen$FemaleRenests <- rep(0, length(BirdsSeen$years))
BirdsSeen$Renests <- rep(0, length(BirdsSeen$years))


BirdsSeen$FloaterNumbers <- rep(0, length(BirdsSeen$years)) #number of nonbreeding adults caught that year
BirdsSeen$MFloaterNumbers <- rep(0, length(BirdsSeen$years))
BirdsSeen$FFloaterNumbers <- rep(0, length(BirdsSeen$years))

#Use this to calculate how many adults we probably should have had in more detail

for (bird in as.list(globalData$birds)){
  #If we saw the bird more than once
  if (bird$yearsSeen$length>1){
    birdsSeenConsecutively<- birdsSeenConsecutively + 1
    #did all of those years happen consecutively?
    years <- integer()
    for(year in bird$yearsSeen$as.list()){
      
      years[length(years)+1]<- year$year
      #clear a before resetting it
      a <- which(BirdsSeen$years==year$year)
      #If you're a hatch year but also have nests then you were recruited into the population
      if(year$age=="HY" ){
        if(bird$nestList$length>0){
          BirdsSeen$RecruitedNestlings[a] <- BirdsSeen$RecruitedNestlings[a]+1
          if(bird$sex=="F"){
            BirdsSeen$FRecruitedNestlings[a] <- BirdsSeen$FRecruitedNestlings[a]+1
          }
          if(bird$sex=="M"){
            BirdsSeen$MRecruitedNestlings[a] <- BirdsSeen$MRecruitedNestlings[a]+1
          }
        }
        
        #If you don't have any nests then you don't count as a recruit and I don't really care about you at all!
      } else {
        #If you are an adult
        if(year$nest$length==0){
          BirdsSeen$FloaterNumbers[a]<- BirdsSeen$FloaterNumbers[a]+1
          
        } else {
          BirdsSeen$AdultsSeen[a]<- BirdsSeen$AdultsSeen[a]+1
        }
        if(bird$sex=="F"){
          if(year$nest$length>0){
            BirdsSeen$FAdultsSeen[a] <- BirdsSeen$FAdultsSeen[a]+1
          } else {
            BirdsSeen$FFloaterNumbers[a]<- BirdsSeen$FFloaterNumbers[a]+1
            
          }
        }
        if(bird$sex=="M"){
          if(year$nest$length>0){
            BirdsSeen$MAdultsSeen[a] <- BirdsSeen$MAdultsSeen[a]+1
          } else {
            BirdsSeen$MFloaterNumbers[a]<-BirdsSeen$MFloaterNumbers[a]+1
          }
        }
        
        
      }
      
      
    }
    
    for (i in 2:length(years)){
      
      #if we skipped a year (they should be ordered from when we reordered them to assign age)
      if((years[i]-years[i-1])!=1){
        birdsSeenMissingYears <- birdsSeenMissingYears +1
        #need to add a missing bird to the list!
        for (b in 1:(years[i]-years[i-1])){
          BirdsSeen$AdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]<- BirdsSeen$AdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]+1
          if(bird$sex=="F"){
            BirdsSeen$FAdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]<- BirdsSeen$FAdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]+1
            
          }
          if(bird$sex=="M"){
            BirdsSeen$MAdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]<- BirdsSeen$MAdultsUnseen[which(BirdsSeen$years  ==years[i]+b)]+1
            
          }
        }
      }
    }
    
  } else {
    #Bird was only seen once so they just get added into the adults seen from that year if they're an adult
    year<-bird$yearsSeen$as.list()[[1]]
    if (year$age != "HY") {
      #If you are an adult and have no nests then you are a floater
      if(year$nest$length==0 ){
        BirdsSeen$FloaterNumbers[a]<- BirdsSeen$FloaterNumbers[a]+1
        
      }else {
        #Otherwise you are part of the breeding population!
        BirdsSeen$AdultsSeen[a]<- BirdsSeen$AdultsSeen[a]+1
      }
      #Now we need to do the same thing by sex
      if(bird$sex=="F"){
        if(year$nest$length>0){
          BirdsSeen$FAdultsSeen[a] <- BirdsSeen$FAdultsSeen[a]+1
        } else {
          BirdsSeen$FFloaterNumbers[a]<- BirdsSeen$FFloaterNumbers[a]+1
          
        }
      }
      if(bird$sex=="M"){
        if(year$nest$length>0){
          BirdsSeen$MAdultsSeen[a] <- BirdsSeen$MAdultsSeen[a]+1
        } else {
          BirdsSeen$MFloaterNumbers[a]<-BirdsSeen$MFloaterNumbers[a]+1
        }
      }
    }
  
    
  }
}

#### WOW there are 401 birds that are missing years seen out of 1400 birds we
#### saw consecutively. That's a huge number (28%)

for (nest in as.list(globalData$nests)){
 r<-  which(nest$year==BirdsSeen$years)
 if (nest$renestStatus=="First"){
   BirdsSeen$FirstNests[r]<- BirdsSeen$FirstNests[r]+1
 }
 if(nest$renestStatus=="Male Renest" | nest$renestStatus=="Male Renest/Female Unknown"){
   BirdsSeen$MaleRenests[r]<- BirdsSeen$MaleRenests[r]+1
 }
 if(nest$renestStatus=="Female Renest" | nest$renestStatus=="Female Renest/Male Unknown"){
   BirdsSeen$FemaleRenests[r]<- BirdsSeen$MaleRenests[r]+1
 }
 if(nest$renestStatus=="All Renest"){
   BirdsSeen$Renests[r]<- BirdsSeen$Renests[r]+1
 }
 
  #add the fledged nestlings to the number of fledged nestlings that year
  if(!is.na(nest$fledgeSize)){
    #How many nests do we actually know the fledge rates for?
    BirdsSeen$NestsWFledgeSizeKnown[r] <- BirdsSeen$NestsWFledgeSizeKnown[r] + 1
    BirdsSeen$NestlingsFledged[r]<- BirdsSeen$NestlingsFledged[r]+
      nest$fledgeSize
    #How many of those fledging nestlings were banded?
    bandedNestlings <- 0
    if(nest$nestlings$length>0){
      for(nestlingkey in nest$nestlings$as.list()){
        nestling<- get(nestlingkey$m_key, globalData$nestlings)
        if(!is.na(nestling$nestlingTRES$m_key)){
          bandedNestlings <- bandedNestlings + 1
          
        }
      }
    }
    
    if (bandedNestlings >= nest$fledgeSize){
      #if we have more nestlings banded than fledged or equal then the entire fledge group was banded
      BirdsSeen$BandedFledgelings[r]<- BirdsSeen$BandedFledgelings[r]+ nest$fledgeSize
        
    } else {
      #if less nestlings were banded than fledged then we MAX had the number of banded nestlings fledge with bands
      BirdsSeen$BandedFledgelings[r]<- BirdsSeen$BandedFledgelings[r]+ bandedNestlings
    }
    
    
  }
}


#Calculate how many birds we think we aren't seeing each year
BirdsSeen$EstimatePop <-  BirdsSeen$FirstNests*2 +BirdsSeen$MaleRenests+BirdsSeen$FemaleRenests +BirdsSeen$FloaterNumbers
BirdsSeen$FEstimatePop <- BirdsSeen$FirstNests+ BirdsSeen$MaleRenests +BirdsSeen$FFloaterNumbers
BirdsSeen$MEstimatePop <- BirdsSeen$FirstNests+ BirdsSeen$FemaleRenests +BirdsSeen$MFloaterNumbers

#Use those numbers to guess at how many birds we didn't see
BirdsSeen$AdultsUnknown<- BirdsSeen$EstimatePop- BirdsSeen$AdultsSeen -BirdsSeen$AdultsUnseen
BirdsSeen$FAdultsUnknown<- BirdsSeen$FEstimatePop - BirdsSeen$FAdultsSeen-BirdsSeen$FAdultsUnseen
BirdsSeen$MAdultsUnknown<- BirdsSeen$MEstimatePop - BirdsSeen$MAdultsSeen- BirdsSeen$MAdultsUnseen



#OK now I have this nice data set that this showing us how often we don't see a bird
#lets plot that

ggplot(data=BirdsSeen, aes(x=years, y=Value))+
  geom_point(aes(y=AdultsUnseen), shape=21)+  #The bubble is the adults unseen
  geom_point(aes(y=AdultsSeen))+
  xlab("Year Surveyed")+
  ylab("Aduts Caught")
#break that up by sex
ggplot(data=BirdsSeen, aes(x=years, y=Value))+
  geom_point(aes(y=FAdultsUnseen), color="red")+  #The bubble is the adults unseen
  geom_point(aes(y=MAdultsUnseen), color="blue")+  #The bubble is the adults unseen
  geom_point(aes(y=MAdultsSeen) , color="turquoise" )+
  geom_point(aes(y=FAdultsSeen), color="pink" )


ggplot(data=BirdsSeen, aes(x=years, y=AdultsUnknown))+
  geom_point()+
  xlab("Year Surveyed")+
  ylab("Esimated number of unidentified adults in population")

#OK now lets see what proportion of the populution we are able to account for
#either through being unseen but known, or being seen in the year based on the
#number of birds we expect from the number of nests
ggplot(data=BirdsSeen, aes(x=years, y=(AdultsSeen+AdultsUnseen)/EstimatePop))+
  geom_point()+  #The bubble is the adults unseen
  xlab("Year Surveyed")+
  ylab("Proportion of breeding population accounted for somehow")
#Huh it looks like as time goes on we are getting better at accounting for our
#adults. Let's break that up into how we are accounting for them!

#Will need a longdataset to make the stacked bargraphs
BirdsSeen2 <- melt(BirdsSeen, id=c("years"))
names(BirdsSeen2) <- c("years", "howKnown", "number") 
BirdsSeen2 <- BirdsSeen2 %>% filter(howKnown=="AdultsSeen" |
                                      howKnown=="AdultsUnseen" |
                                      howKnown=="AdultsUnknown")

ggplot(BirdsSeen2,aes(x = years, y = number, fill = howKnown)) + 
  geom_bar(stat = "identity", show.legend = T) + 
  xlab("Year")+
  ylab("Number of Adults")+
  scale_fill_discrete(name="What do we know about the adults?",
                       breaks=c("AdultsSeen", "AdultsUnseen", "AdultsUnknown"),
                       labels=c("Caught & known ID", "Caught previously & unknown ID", " Not caught & unknown ID" ))



BirdsSeen2F <- melt(BirdsSeen, id=c("years"))
names(BirdsSeen2F) <- c("years", "howKnown", "number") 
BirdsSeen2F <- BirdsSeen2F %>% filter(howKnown=="FAdultsSeen" |
                                      howKnown=="FAdultsUnseen" |
                                      howKnown=="FAdultsUnknown")

ggplot(BirdsSeen2F,aes(x = years, y = number, fill = howKnown)) + 
  geom_bar(stat = "identity", show.legend = TRUE) + 
  xlab("Year")+
  ylab("Number of Females Estimated")+
  scale_fill_discrete(name="What do we know about the females?",
                      breaks=c("FAdultsSeen", "FAdultsUnseen", "FAdultsUnknown"),
                      labels=c("Caught & known ID", "Caught previously & unknown ID", " Not caught & unknown ID" ))


BirdsSeen2M <- melt(BirdsSeen, id=c("years"))
names(BirdsSeen2M) <- c("years", "howKnown", "number") 
BirdsSeen2M <- BirdsSeen2M %>% filter(howKnown=="MAdultsSeen" |
                                        howKnown=="MAdultsUnseen" |
                                        howKnown=="MAdultsUnknown")

ggplot(BirdsSeen2M,aes(x = years, y = number, fill = howKnown)) + 
  geom_bar(stat = "identity", show.legend = TRUE) + 
  xlab("Year")+
  ylab("Number of Males Estimated")+
  scale_fill_discrete(name="What do we know about the males?",
                      breaks=c("MAdultsSeen", "MAdultsUnseen", "MAdultsUnknown"),
                      labels=c("Caught & known ID", "Caught previously & unknown ID", " Not caught & unknown ID" ))





#Lets use some of this information to calculate recruitment rates each year. 
ggplot(BirdsSeen, aes(x=years, y=RecruitedNestlings))+
  geom_point()

#Since what we know about males and females is so different probably the best 
#way to do this is calculate the proportion of male recruits to female recruits 
#and use that to divy up the unknown recruits. Then we can use the proportion of
#each sex caught each year to scale up to what we expect actually recruited that
#year and then use that to calculate number of recruits to divide by number of
#fledgelings to get a recruitment from that year!
#Make sure we use the number of nestlings fledgeing that had bands on them not just the number of nestlings fledging
proportFRecruit<- sum(BirdsSeen$FRecruitedNestlings)/(sum(BirdsSeen$FRecruitedNestlings)+sum(BirdsSeen$MRecruitedNestlings))
#I had to average it across all years because there are low recruitment numbers. Hopefully that's OK....
proportMRecruit<- sum(BirdsSeen$MRecruitedNestlings)/(sum(BirdsSeen$FRecruitedNestlings)+sum(BirdsSeen$MRecruitedNestlings))

BirdsSeen$RecruitF <- BirdsSeen$FRecruitedNestlings* BirdsSeen$FEstimatePop/BirdsSeen$FAdultsSeen

BirdsSeen$EstimatedRecruitF <- BirdsSeen$RecruitedNestlings*proportFRecruit * BirdsSeen$FEstimatePop/BirdsSeen$FAdultsSeen
BirdsSeen$EstimatedRecruitM <- BirdsSeen$RecruitedNestlings*proportMRecruit * BirdsSeen$MEstimatePop/BirdsSeen$MAdultsSeen
#There are a couple of years where we caught 0 males-- therefore we know nothing
#about male recruitment those years, it hasn't got to Infinity
BirdsSeen$EstimatedRecruitM[which(BirdsSeen$EstimatedRecruitM==Inf)]<- NA

#I have no banded fledgelings in 2007 (actually I just have no damn data for
#it.... useless people) I'm going to toss the 2007 banded fledgelings--> There
#are banded nestlings that year but they arne't attached to a nest so I don't
#really have a good idea what's going on

BirdsSeen$Frecruitmentrate <- BirdsSeen$EstimatedRecruitF/(BirdsSeen$BandedFledgelings/2)
#Toss 1975, 1976, 1977 , 2007 and 2016 (First three years we caught too few females 2007 doesn't have fledgelings with known bands,  2016 
#hasn't had the chance to recruit yet, and we only caught 2 females in 1977 so
#our estimate of recruitment is shitee)
mean(BirdsSeen$Frecruitmentrate[-c(1,2,3, 33, 42)]) #0.06226302
#1976 recruitment rates
BirdsSeen$Frecruitmentrate[2]  #0.09623431

BirdsSeen$Mrecruitmentrate <- BirdsSeen$EstimatedRecruitM/(BirdsSeen$BandedFledgelings/2)
#Same as for the females I'm going to toss years 1975-1981 because they didn't
#really start catching males until then plus, 2007 and 2016
BirdsSeen$Mrecruitmentrate[c(1, 2, 3,4,5,6,7, 33, 42)]<- NA

mean(BirdsSeen$Mrecruitmentrate[-c(1, 2, 3,4,5,6,7, 33, 42)], na.rm=T) #0.06856466

 
ggplot()+
   geom_point( data=BirdsSeen[-c(1, 2, 3,4,5,6,7, 33, 42), ], aes( x=years, y=Mrecruitmentrate), color="blue")+
  #geom_smooth(data=BirdsSeen[-c(1, 2, 3,4,5,6,7, 33, 42), ], aes( x=years, y=Mrecruitmentrate), method="lm", color="blue")+
  geom_point( data=BirdsSeen[-c( 1, 2, 3, 33, 42), ], aes( x=years, y=Frecruitmentrate), color="red")+
  #geom_smooth(data=BirdsSeen[-c(1,2, 3, 33, 42), ], aes( x=years, y=Frecruitmentrate), method="lm", color="red")+
  xlab("Year")+
  ylab("Recruitment Rate")

#Huh male recruitment rate is doing something really different than female
#recruitment rate. It's dropping way more dramatically

#If I try to model that what happens?
#First need to make different vectors where the removed points are NA
BirdsSeen$Mrecruitmentrate2 <- BirdsSeen$Mrecruitmentrate
BirdsSeen$Mrecruitmentrate2[c(1, 2, 3,4,5,6,7, 33, 42)]<- NA
BirdsSeen$Frecruitmentrate2 <- BirdsSeen$Frecruitmentrate
BirdsSeen$Frecruitmentrate2[c(1,2,3, 33, 42)] <- NA

#WHat might influence recruitment? year if fledgelings are lower quality now sex
#if there are differential recruitment rates  and there definitely appears to
#be! Maybe the number of hatched nestlings? That could have influenced the
#quality of the fledgelings if they were competing with all these
recruitData <- melt(BirdsSeen, id=c("years")) %>% filter (variable=="Frecruitmentrate2" |variable=="Mrecruitmentrate2")
recruitData$variable<- as.character(recruitData$variable)
recruitData$variable[which(recruitData$variable=="Frecruitmentrate2")] <- "F"
recruitData$variable[which(recruitData$variable=="Mrecruitmentrate2")] <- "M"
colnames(recruitData) <- c("year", "sex", "recruitmentrate")
recruitData$sex <- as.factor(recruitData$sex)

recruitData <- recruitData %>% filter (!is.na(sex) & !is.na(recruitmentrate))
recruitMod <- lm(recruitmentrate ~ year*sex, data=recruitData)
plot(recruitMod) #Q-Q plot is bad for large values and scale location plot trends down
hist(resid(recruitMod, type="pearson"))
shapiro.test(resid(recruitMod, type="pearson")) #yup no surprise those residuals aren't normal!
plot(resid(recruitMod, type="pearson")~ recruitData$year) #variance is pretty good except in the late 2000s where we skyrocket
plot(resid(recruitMod, type="pearson")~ recruitData$sex) #Variance is fine
plot(resid(recruitMod, type="pearson")~ recruitData$recruitmentrate) 
#hmmmmm very obvious trend here..... that's super problematic. we are a lot
#worse at predicting recruitment for larger recruitment values
#What if I log recruitment?
recruitMod2 <- lm(log(recruitmentrate+0.0001) ~ year*sex, data=recruitData)
plot(recruitMod2) #nope that's arguably worse. Sigh

#Fran used the arcsin transformation. I don't understand why she did that because I have no idea how you'd interpret that but I can try
recruitMod3 <- lm(asin(recruitmentrate) ~ year*sex, data=recruitData)
plot(recruitMod3) 
hist(resid(recruitMod3, type="pearson"))
plot(resid(recruitMod3, type="pearson")~ recruitData$recruitmentrate) 


#How do floaters do over the years? 
ggplot(BirdsSeen, aes(x=years, y=FloaterNumbers))+
  geom_point()+
  xlab("Year Surveyed")+
  ylab("Adults without known nests")+
  geom_smooth()
#They follow the exact same trends as the box population




