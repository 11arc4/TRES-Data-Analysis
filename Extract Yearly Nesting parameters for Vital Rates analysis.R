#I need to get the population level statistics for each year of our data. 
library(dplyr)
library(popbio)
library(popdemo)

#To do that I need to pull out all the data for each of the nests

firsteggdate<- rep(NA, length(as.list(globalData$nests)))
clutch<- rep(NA, length(as.list(globalData$nests)))
hatch <- rep(NA, length(as.list(globalData$nests)))
fledge <- rep(NA, length(as.list(globalData$nests)))
FAge <- rep(NA, length(as.list(globalData$nests)))
FBand <- rep(NA, length(as.list(globalData$nests)))
year <- rep(NA, length(as.list(globalData$nests)))
parameters <- data.frame(firsteggdate, clutch, hatch, fledge, FBand, FAge, year)

i=0
for(nest in as.list(globalData$nests)){
  i=i+1
  parameters$clutch[i] <- nest$clutchSize
  parameters$firsteggdate[i]<- nest$firstEggDate
  parameters$hatch[i] <- nest$hatchSize
  parameters$fledge[i] <- nest$fledgeSize
  parameters$year[i]<- nest$year
  parameters$renestStatus[i] <- nest$renestStatus
  
  parameters$experiment[i] <- nest$experiment
  
  if (!is.na(nest$femaleID$m_key)){
    #message("Known female", nest$femaleID$m_key)
    
    bird <- get(nest$femaleID$m_key, globalData$birds)
    for (year in bird$yearsSeen$as.list()){
      if(nest$year==year$year){
        parameters$FAge[i]<- year$age
        parameters$FBand[i]<- bird$bandID
      }
    }
  }
}
    

#Also need to pull out how many nesting attempts are average for each year
nestsinyear <- rep(NA, length(as.list(globalData$birds)))
year <- rep(NA, length(as.list(globalData$birds)))
sex <- rep(NA, length(as.list(globalData$birds)))
age <- rep(NA, length(as.list(globalData$birds)))
birdband <- rep(NA, length(as.list(globalData$birds)))
reprod<- data.frame(nestsinyear, year, sex, age, birdband)
a=0
for(bird in as.list(globalData$birds)){
  #if a bird was seen as a nestling BUT was seen in multiple years or the bird wasn't a nestling
  if((!is.na(bird$hatchnest$m_key) & bird$yearsSeen$length>1)| is.na(bird$hatchnest$m_key)){
    for(year in bird$yearsSeen$as.list()){
      if(year$nest$length>0){
        a=a+1
        if(is.na(year$hatchNest$m_key)){
          reprod$nestsinyear[a]<- year$nest$length
          reprod$age[a] <- year$age
          reprod$sex[a] <- bird$sex
          reprod$birdband[a] <- bird$bandID
          reprod$year[a]<- year$year
        }
        
      }
      
    }
  }
}
reprod <- reprod[1:a,]


#Fill in a population level data sheet for each year
year<- seq(1975, 2017, 1)
PopData <- data.frame(year)

#THIS IS NOT DOING WHAT I WANT IT TO
a <- 0
for (y in 1975:2017){
  a<- a+1
  #fill in the average number of renests (for females) known in that year
  PopData$averageNests[a] <- 
    sum(reprod$nestsinyear[ which(reprod$year==y & reprod$sex=="F")]) / length(which(reprod$year==y & reprod$sex=="F")) 
  #Fill in average clutch size that year
  clutchnests <- parameters %>% filter (!is.na(clutch) & year==y)
  PopData$clutchSize[a] <-  sum(clutchnests$clutch) / nrow(clutchnests)
  #Fill in average hatchrate
  hatchnests <- parameters %>% filter (clutch>0 & !is.na(hatch) & year==y)
  PopData$hatchRate[a] <- sum(hatchnests$hatch) / sum (hatchnests$clutch)
  #fill in average fledge size
  fledgenests <- parameters %>% filter (!is.na(fledge) & hatch>0 & year==y)
  PopData$fledgeRate[a] <- sum(fledgenests$fledge) / sum(fledgenests$hatch)
}


write.csv(PopData, file= "file:///C:/Users/Amelia/Documents/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Yearly Vital Rates.csv", na="", row.names = F)
