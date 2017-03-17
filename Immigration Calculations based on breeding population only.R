#How have immigration rates changed over the years?
#This entire set of calculations is only including the breeding population--not looking at floaters!
outerdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016"

BoxesAndTerritories<- read.csv(paste(outerdir, "Box Occupancy 1975-2016.csv", sep="/"), 
                               as.is=TRUE, na.strings = c("", "NA"))
rownames(BoxesAndTerritories)<-BoxesAndTerritories$Year
BoxesAndTerritories<- subset(BoxesAndTerritories, select= -c(Year))
Box_rows<- which(grepl("Box", row.names(BoxesAndTerritories)))
Boxes <- BoxesAndTerritories[Box_rows,]
Terr_rows <- which(grepl("Territory", row.names(BoxesAndTerritories)))
Terr <- BoxesAndTerritories[Terr_rows,]



years<- seq(from=1975, to=2016, by=1)

Immigration <- data.frame(years)
Immigration$TotalBoxes <- c()
for(y in 1:ncol(Boxes)){
  Immigration$TotalBoxes[y] <- sum(Boxes[,y], na.rm = T)
    
}
Immigration$TotalTerr <- c()
for(y in 1:ncol(Terr)){
  Immigration$TotalTerr[y] <- sum(Terr[,y], na.rm = T)
  
}
Immigration$NewBirdsCaught <- rep(0, length(Immigration$years))
Immigration$NewFemalesCaught <- rep(0, length(Immigration$years))
Immigration$NewSYFemalesCaught <- rep(0, length(Immigration$years))
Immigration$NewASYFemalesCaught <- rep(0, length(Immigration$years))

Immigration$NewMalesCaught <- rep(0, length(Immigration$years))

Immigration$ReturningBirdsCaught <- rep(0, length(Immigration$years))
Immigration$ReturningFemalesCaught <- rep(0, length(Immigration$years))
Immigration$ReturningMalesCaught <- rep(0, length(Immigration$years))

Immigration$RecruitedBirdsCaught <- rep(0, length(Immigration$years))
Immigration$RecruitedFemalesCaught <- rep(0, length(Immigration$years))
Immigration$RecruitedMalesCaught <- rep(0, length(Immigration$years))

Immigration$TotalBirdsCaught <- rep(0, length(Immigration$years))

Immigration$TotalFemalesCaught <- rep(0, length(Immigration$years))
Immigration$TotalMalesCaught <- rep(0, length(Immigration$years))

Immigration$FirstNests <- rep(0, length(Immigration$years))
Immigration$MaleRenests <- rep(0, length(Immigration$years))
Immigration$FemaleRenests <- rep(0, length(Immigration$years))
Immigration$Renests <- rep(0, length(Immigration$years))

for (bird in as.list(globalData$birds)){
  for(year in bird$yearsSeen$as.list()){
    if(year$nest$length>0){
      y <- which(Immigration$years==year$year)
      
      #Need to add to the bird counts
      Immigration$TotalBirdsCaught[y]<- Immigration$TotalBirdsCaught[y]+1
      if(bird$sex=="F"){
        Immigration$TotalFemalesCaught[y]<- Immigration$TotalFemalesCaught[y]+1
      
      }
      if(bird$sex=="M"){
        Immigration$TotalMalesCaught[y] <- Immigration$TotalMalesCaught[y]+1
      }
      #Now need to add to the return status totals
      if(year$returnStatus=="Recruit"){
        Immigration$RecruitedBirdsCaught[y] <- Immigration$RecruitedBirdsCaught[y]+1
        if(bird$sex=="F"){
          Immigration$RecruitedFemalesCaught[y]<- Immigration$RecruitedFemalesCaught[y]+1
        }
        if(bird$sex=="M"){
          Immigration$RecruitedMalesCaught[y] <- Immigration$RecruitedMalesCaught[y]+1
        }
      }
      
      
      if(year$returnStatus=="Return"){
        Immigration$ReturningBirdsCaught[y] <- Immigration$ReturningBirdsCaught[y]+1
        if(bird$sex=="F"){
          Immigration$ReturningFemalesCaught[y]<- Immigration$ReturningFemalesCaught[y]+1
        }
        if(bird$sex=="M"){
          Immigration$ReturningMalesCaught[y] <- Immigration$ReturningMalesCaught[y]+1
        }
      }
      if(year$returnStatus=="New"){
        Immigration$NewBirdsCaught[y] <- Immigration$NewBirdsCaught[y]+1
        if(bird$sex=="F"){
          
          Immigration$NewFemalesCaught[y]<- Immigration$NewFemalesCaught[y]+1
          if(year$age=="SY"){
            Immigration$NewSYFemalesCaught[y]<- Immigration$NewSYFemalesCaught[y]+1
            
          }
          if(year$age!="SY" & !is.na(year$age)){
            Immigration$NewASYFemalesCaught[y]<- Immigration$NewASYFemalesCaught[y]+1
            
          }
        }
        if(bird$sex=="M"){
          Immigration$NewMalesCaught[y] <- Immigration$NewMalesCaught[y]+1
        }
      }
      
      
      
      
    }
 
  }
}


#Now lets go through and fill in the numbers for nest types to get estimate population sizes!
for (nest in as.list(globalData$nests)){
  r<-  which(nest$year==Immigration$years)
  if (nest$renestStatus=="First"){
    Immigration$FirstNests[r]<- Immigration$FirstNests[r]+1
  }
  if(nest$renestStatus=="Male Renest" | nest$renestStatus=="Male Renest/Female Unknown"){
    Immigration$MaleRenests[r]<- Immigration$MaleRenests[r]+1
  }
  if(nest$renestStatus=="Female Renest" | nest$renestStatus=="Female Renest/Male Unknown"){
    Immigration$FemaleRenests[r]<- Immigration$MaleRenests[r]+1
  }
  if(nest$renestStatus=="All Renest"){
    Immigration$Renests[r]<- Immigration$Renests[r]+1
  }
  
  #add the fledged nestlings to the number of fledged nestlings that year
  if(!is.na(nest$fledgeSize)){
    Immigration$NestlingsFledged[r]<- Immigration$NestlingsFledged[r]+
      nest$fledgeSize
  }
}



Immigration$EstimatePop <-  Immigration$FirstNests*2 +Immigration$MaleRenests+Immigration$FemaleRenests 
Immigration$FEstimatePop <- Immigration$FirstNests+ Immigration$MaleRenests 
Immigration$MEstimatePop <- Immigration$FirstNests+ Immigration$FemaleRenests 

Immigration$UnknownReturnStatusBirds<- Immigration$EstimatePop-Immigration$NewBirdsCaught-Immigration$ReturningBirdsCaught-Immigration$RecruitedBirdsCaught
Immigration$UnknownReturnStatusFemales<- Immigration$FEstimatePop-Immigration$NewFemalesCaught-Immigration$ReturningFemalesCaught-Immigration$RecruitedFemalesCaught
Immigration$UnknownReturnStatusMales <- Immigration$MEstimatePop-Immigration$NewMalesCaught-Immigration$ReturningMalesCaught-Immigration$RecruitedMalesCaught




ggplot(Immigration, aes(x=years, y=NewBirdsCaught))+
  geom_point()
ggplot(Immigration, aes(x=years, y=Value))+
  geom_point(aes(y=NewFemalesCaught), color="red")+
  geom_point(aes(y=NewMalesCaught), color="blue")+
  xlab("Years")+
  ylab("New birds caught")



Immigration2 <- melt(Immigration, id=c("years"))
names(Immigration2) <- c("years", "returnStatus", "number") 
#Dataset for all birds
Immigration2_Birds<- Immigration2 %>% filter( returnStatus=="NewBirdsCaught"|
                                 returnStatus=="ReturningBirdsCaught"|
                                 returnStatus=="RecruitedBirdsCaught"|
                                 returnStatus=="UnknownReturnStatusBirds"  )

#Only the females

Immigration2_Females<- Immigration2 %>% filter( returnStatus=="NewFemalesCaught"|
                                                      returnStatus=="ReturningFemalesCaught"|
                                                      returnStatus=="RecruitedFemalesCaught"|
                                                      returnStatus=="UnknownReturnStatusFemales"  )
#Only the males 
Immigration2_Males<- Immigration2 %>% filter( returnStatus=="NewMalesCaught"|
                                                returnStatus=="ReturningMalesCaught"|
                                                returnStatus=="RecruitedMalesCaught"|
                                                returnStatus=="UnknownReturnStatusMales"  )

#plots that type of bird with population size
ggplot(Immigration2_Birds, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity", show.legend = T)+
  scale_fill_discrete(name="Type of Adult",
                      breaks=c("NewBirdsCaught", "ReturningBirdsCaught", "RecruitedBirdsCaught", "UnknownReturnStatusBirds"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))


ggplot(Immigration2_Females, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity", show.legend = T)+
  scale_fill_discrete(name="Type of Female",
                      breaks=c("NewFemalesCaught", "ReturningFemalesCaught", "RecruitedFemalesCaught", "UnknownReturnStatusFemales"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))


ggplot(Immigration2_Males, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity", show.legend = T)+
  scale_fill_discrete(name="Type of Female",
                      breaks=c("NewMalesCaught", "ReturningMalesCaught", "RecruitedMalesCaught", "UnknownReturnStatusMales"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))

#plots that type of bird as a proportion of the population
ggplot(Immigration2_Birds, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity", position="fill", show.legend = T)+
  scale_fill_discrete(name="Type of Adult",
                      breaks=c("NewBirdsCaught", "ReturningBirdsCaught", "RecruitedBirdsCaught", "UnknownReturnStatusBirds"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))


ggplot(Immigration2_Females, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity",position="fill", show.legend = T)+
  scale_fill_discrete(name="Type of Female",
                      breaks=c("NewFemalesCaught", "ReturningFemalesCaught", "RecruitedFemalesCaught", "UnknownReturnStatusFemales"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))


ggplot(Immigration2_Males, aes(x=years, y=number, fill=returnStatus))+
  geom_bar(stat = "identity", position="fill",show.legend = T)+
  scale_fill_discrete(name="Type of Female",
                      breaks=c("NewMalesCaught", "ReturningMalesCaught", "RecruitedMalesCaught", "UnknownReturnStatusMales"),
                      labels=c("New", "Return", "Recruit", "Unknown" ))






#Calculate yearly proportion of New, Return and Recruit Birds out of the known birds--maybe this is what changes
#Proportion of Birds
Immigration$PropNewBirds <- Immigration$NewBirdsCaught/Immigration$TotalBirdsCaught
Immigration$PropReturnBirds <- Immigration$ReturningBirdsCaught/Immigration$TotalBirdsCaught
Immigration$PropRecruitBirds <- Immigration$RecruitedBirdsCaught/Immigration$TotalBirdsCaught
#Proportion of Females
Immigration$PropNewFemales <- Immigration$NewFemalesCaught/Immigration$TotalFemalesCaught
Immigration$PropReturnFemales <- Immigration$ReturningFemalesCaught/Immigration$TotalFemalesCaught
Immigration$PropRecruitFemales <- Immigration$RecruitedFemalesCaught/Immigration$TotalFemalesCaught
#Proportion of Males 
Immigration$PropNewMales <- Immigration$NewMalesCaught/Immigration$TotalMalesCaught
Immigration$PropReturnMales <- Immigration$ReturningMalesCaught/Immigration$TotalMalesCaught
Immigration$PropRecruitMales <- Immigration$RecruitedMalesCaught/Immigration$TotalMalesCaught

#Let's plot that proportion over time to see what we think is going on
Immigration3 <- melt (Immigration, id=c("years"))
names(Immigration3) <- c("year", "typeofbird", "proportion")
Immigration3_birds <- Immigration3 %>% filter (typeofbird== "NewBirdsCaught" |
                                                 typeofbird=="ReturningBirdsCaught"|
                                                 typeofbird=="RecruitedBirdsCaught")

ggplot(Immigration3_birds, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity", position="fill", show.legend = T)+
  xlab("Year")+
  ylab("Proportion Adults")+
  scale_fill_discrete(name="Return Status", 
                      breaks= c("PropNewBirds", "PropReturnBirds", "PropRecruitBirds"), 
                      labels=c("New", "Return", "Recruit"))

Immigration3_females <- Immigration3 %>% filter (typeofbird== "NewFemalesCaught" |
                                                   typeofbird=="ReturningFemalesCaught"|
                                                   typeofbird=="RecruitedFemalesCaught")

ggplot(Immigration3_females, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity", position="fill", show.legend = T)+
  xlab("Year")+
  ylab("Proportion Females")+
  scale_fill_discrete(name="Return Status", 
                      breaks= c("NewFemalesCaught", "ReturningFemalesCaught", "RecruitedFemalesCaught"), 
                      labels=c("New", "Return", "Recruit"))



Immigration3_males <- Immigration3 %>% filter (typeofbird== "NewMalesCaught" |
                                                 typeofbird=="ReturningMalesCaught"|
                                                 typeofbird=="RecruitedMalesCaught")

ggplot(Immigration3_males, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity",position="fill", show.legend = T)+
  xlab("Year")+
  ylab("Proportion Males")+
  scale_fill_discrete(name="Return Status", 
                      breaks= c("NewMalesCaught", "ReturningMalesCaught", "RecruitedMalesCaught"), 
                      labels=c("New", "Return", "Recruit"))





#What about absolute numbers based on estimate population sizes? 

Immigration$EstimateNewBirds <- Immigration$EstimatePop * Immigration$PropNewBirds
Immigration$EstimateReturnBirds <- Immigration$EstimatePop * Immigration$PropReturnBirds
Immigration$EstimateRecruitBirds <- Immigration$EstimatePop * Immigration$PropRecruitBirds

Immigration$EstimateNewFemales <- Immigration$FEstimatePop * Immigration$PropNewFemales
Immigration$EstimateReturnFemales <- Immigration$FEstimatePop * Immigration$PropReturnFemales
Immigration$EstimateRecruitFemales <- Immigration$FEstimatePop * Immigration$PropRecruitFemales


Immigration$EstimateNewMales <- Immigration$MEstimatePop * Immigration$PropNewMales
Immigration$EstimateReturnMales <- Immigration$MEstimatePop * Immigration$PropReturnMales
Immigration$EstimateRecruitMales <- Immigration$MEstimatePop * Immigration$PropRecruitMales

Immigration3_birdsTot <- Immigration3 %>% filter (typeofbird== "EstimateNewBirds" |
                                                      typeofbird=="EstimateReturnBirds"|
                                                      typeofbird=="EstimateRecruitBirds")
ggplot(Immigration3_birdsTot, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity", show.legend = T)+
  xlab("Year")+
  ylab("Total Adults")+
  scale_fill_discrete(name="Return Status", 
                      breaks=c("EstimateNewBirds", "EstimateReturnBirds", "EstimateRecruitBirds"), 
                      labels=c("New", "Return", "Recruit")
                    )

Immigration3_femalesTot <- Immigration3 %>% filter (typeofbird== "EstimateNewFemales" |
                                                    typeofbird=="EstimateReturnFemales"|
                                                    typeofbird=="EstimateRecruitFemales")
ggplot(Immigration3_femalesTot, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity", show.legend = T)+
  xlab("Year")+
  ylab("Total Females")+
  scale_fill_discrete(name="Return Status", 
                      breaks=c("EstimateNewFemales", "EstimateReturnFemales", "EstimateRecruitFemales"), 
                      labels=c("New", "Return", "Recruit")
  )


Immigration3_malesTot <- Immigration3 %>% filter (typeofbird== "EstimateNewMales" |
                                                 typeofbird=="EstimateReturnMales"|
                                                 typeofbird=="EstimateRecruitMales")

ggplot(Immigration3_malesTot, aes(x=year, y=proportion, fill=typeofbird))+
  geom_bar(stat="identity", show.legend = T)+
  xlab("Year")+
  ylab("Total Males")+
  scale_fill_discrete(name="Return Status", 
                      breaks=c("EstimateNewMales", "EstimateReturnMales", "EstimateRecruitMales"), 
                      labels=c("New", "Return", "Recruit"))

#If there is a clear pattern in the numbers of new birds by year then I could use absolute numbers
ggplot(Immigration, aes(x=years, y=EstimateNewBirds))+
  geom_point()
mean(Immigration$EstimateNewBirds) 
#doesn't really have a clear pattern
mean(Immigration$PropNewBirds)

ggplot(Immigration, aes(x=years, y=PropNewBirds))+
  geom_point()
#The proportion of new birds in the population is definitely more constant. I'm
#leaning towards using this and multiplying the current population of adults I
#would might use that?
mod <- lm(EstimateNewFemales~years+years^2, data=Immigration)
#THis model isn't a good fit for the data
plot(mod)
hist(resid(mod, type="pearson"))




#Is there a difference between the immigration of females by age class?

ggplot(Immigration, aes(x=years, y=Value))+
  geom_point(aes(y=NewASYFemalesCaught), color="black")+
  geom_point(aes(y=NewSYFemalesCaught), color="grey")
#Does look like there might be a ratio


ggplot(Immigration, aes(x=years, y= 100 *NewASYFemalesCaught/(NewSYFemalesCaught+ NewASYFemalesCaught)))+
  geom_point()+
  geom_smooth()+
  xlab("Year")+
  ylab("Percent ASY immigrant females")
##immigrants are more likely to be SY now than they used to be but I'm not sure if that's significnat

Immigration_NewFemales <- Immigration2 %>% filter (returnStatus=="NewASYFemalesCaught"|
                           returnStatus=="NewSYFemalesCaught")
names(Immigration_NewFemales)<- c("year", "age", "number")
ggplot(Immigration_NewFemales, aes(x=year, y=number, fill=age))+
  geom_bar(stat="identity")
ggplot(Immigration_NewFemales, aes(x=year, y=number, fill=age))+
  geom_bar(stat="identity", position="fill")+
  scale_fill_discrete(name="Female Age", 
                      breaks=c("NewASYFemalesCaught","NewSYFemalesCaught" ), 
                      labels=c("ASY", "SY"))+
  xlab("Year")+
  ylab("Proportion of New Females")



#does box availability precict new birds?
Immigration$AvailableBirdSpaces <- 2*Immigration$TotalBoxes-Immigration$EstimateRecruitBirds- Immigration$EstimateReturnBirds
Immigration$AvailableFemaleSpaces <- Immigration$TotalBoxes- Immigration$EstimateReturnFemales - Immigration$EstimateRecruitFemales
Immigration$AvailableMaleSpaces <- Immigration$TotalBoxes - Immigration$EstimateRecruitMales- Immigration$EstimateReturnMales
#How does the available boxes influence immigration rates?
#I've included the years that NES was present between two vertical lines
ggplot(Immigration, aes(y=EstimateNewBirds/AvailableBirdSpaces, x=years))+
  geom_point()+
  geom_vline(xintercept=1981)+
  geom_vline(xintercept=1998)
#Huh it really really looks like NES brought in the birds at a higher rate, but
#otherwise it was pretty stable....

ggplot(Immigration, aes(y=EstimateNewFemales/AvailableFemaleSpaces, x=years))+
  geom_point()+
  geom_vline(xintercept=1981)+
  geom_vline(xintercept=1998)
#Yup we're seeing the same thing for the females alone...

ggplot(Immigration, aes(y=EstimateNewMales/AvailableMaleSpaces, x=years))+
  geom_point()+
  geom_vline(xintercept=1981)+
  geom_vline(xintercept=1998)
