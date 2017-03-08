#Baby matric model
library(dplyr)
library(popdemo)
library(popbio)

#Calculate the average number of nests a bird is involved in 
totalnests <- rep(NA, length(as.list(globalData$birds)))
years <- rep(NA, length(as.list(globalData$birds)))
sex <- rep(NA, length(as.list(globalData$birds)))
reprod<- data.frame(totalnests, years, sex)
a=0
for(bird in as.list(globalData$birds)){
  #if a bird was seen as a nestling BUT was seen in multiple years or the bird wasn't a nestling
  if((!is.na(bird$hatchnest$m_key) & bird$yearsSeen$length>1)| is.na(bird$hatchnest$m_key)){
    a=a+1
    reprod$totalnests[a] <- 0
    for(year in bird$yearsSeen$as.list()){
      reprod$totalnests[a]<- reprod$totalnests[a] + year$nest$length
    }
    
    reprod$sex[a] <- bird$sex
    if(is.na(bird$hatchnest$m_key)){
      reprod$years[a] <- bird$yearsSeen$length
    } else {
      reprod$years[a] <- bird$yearsSeen$length-1
    }
    
  }
}
reprod <- reprod[1:a,]

#calculate the mean number of nests for females 

reprod$nestsperyear<- reprod$totalnests/reprod$years
Freprod<- reprod %>% filter(sex=="F")
meanclutches <- mean(Freprod$nestsperyear, na.rm = T) #0.6446686
#I think this is problematic because there is unequal effort to catch
#nonreproducing females each year-- will be the same issue with the males
Mreprod <- reprod%>% filter(sex=="M")
mean(Mreprod$nestsperyear, na.rm = T) #0.7259783
######################################################################################

#Now calculate average clutch size, hatch size, and fledge success.
clutch<- rep(NA, length(as.list(globalData$nests)))
hatch <- rep(NA, length(as.list(globalData$nests)))
fledge <- rep(NA, length(as.list(globalData$nests)))
i=0
for(nest in as.list(globalData$nests)){
  i=i+1
  clutch[i] <- nest$clutchSize
  hatch[i] <- nest$hatchSize
  fledge[i] <- nest$fledgeSize
}



parameters <- data.frame(clutch, hatch, fledge)

#removed these three nests where we are getting infinite clutch size because they had no eggs but a nestling was transfered in
parameters <- parameters [-c(which(parameters$hatch>parameters$clutch & parameters$clutch==0)), ]
parameters$hatchrate <- parameters$hatch/parameters$clutch
parameters$fledgerate <- parameters$fledge/parameters$hatch


hatchrate <- mean(parameters$hatchrate, na.rm=T) #0.7398289

fledgerate <- mean(parameters$fledgerate, na.rm=T) #0.628297
#Layrate for adults is the mean number of nests*mean eggs laid!
layrate <- meanclutches * mean(parameters$clutch, na.rm=T) #3.399451

#Yay! Now we just needd estimates of recruitment and return!
totalnestlings <- 0
recruit<- rep(NA, length(as.list(globalData$nestlings)))
for(nestling in as.list(globalData$nestlings)){
  totalnestlings <- totalnestlings + 1
  if(is.na(nestling$nestlingTRES$m_key)){
    recruit[totalnestlings] <- 0
    
  } else {
    adultbird <- get(nestling$nestlingTRES$m_key, nestling$nestlingTRES$m_hash)
    if (adultbird$yearsSeen$length>1){
      recruit[totalnestlings] <- 1
    } else {
      recruit[totalnestlings] <- 0
    }
  }
}
recruitrateoffledgelings <- sum(recruit)/sum(parameters$fledge, na.rm=T)

#Need to estimate return rates. I will also do this based on sex. 
return <- rep(NA, length(as.list(globalData$birds)))
Return <- data.frame(return)
Return$sex <- rep(NA, length(as.list(globalData$birds)))
Return$immigrant<- rep(NA, length(as.list(globalData$birds)))
totaladults <- 0
for (bird in as.list(globalData$birds)){
  y=0
  for (year in bird$yearsSeen$as.list()){
    y=y+1
    
    if(!is.na(year$hatchNest$m_key)){
      next
    } else {
      totaladults<- totaladults +1
      Return$sex[totaladults] <- bird$sex
      if(y<bird$yearsSeen$length){
        Return$return[totaladults] <- 1
      } else {
        Return$return[totaladults] <- 0
        
      }
      if(y==1){
        if(is.na(bird$hatchnest$m_key)){
          Return$immigrant[totaladults] <- 1
        } else {
          Return$immigrant[totaladults] <- 0
        }
      } 
      
    }
  }
}
Return <- Return[which(!is.na(Return$return)), ] 
FReturn <- Return %>% filter(sex=="F")
MReturn <- Return %>% filter(sex=="M")

Freturnrate <- mean(FReturn$return) #0.2307692
Mreturnrate <- mean(MReturn$return) #0.2730853



##########################################################
#OK now I have all the relevent rates so now I want to create the matrices and put them into a analysis

#I will look at only females-- therefore since I am assuming equal sex ratios
#(seems valid after looking at Delmore et al. 2008) I will have to divide all
#the rates in half
#also currently treating SY and ASY females the same

stages <- c("egg", "nestling", "fledgelings", "Adult")
A <- matrix(0, nrow=4, ncol=4, dimnames = list(stages, stages))

A[1,4]<- layrate/2
A[2,1]<- hatchrate/2
A[3,2]<- fledgerate/2
A[4,3] <- recruitrateoffledgelings/2
A[4,4]<- Freturnrate


#Should both be true!
is.matrix_irreducible(A) 
#YAY
is.matrix_ergodic(A)
!YAY

#N0 is the initial population sizes. 
#I will set time 0 to be 1976, before breeding begins. There were 54 nests, so 54 females. Really this should be checked. There could have been floaters too but I won't know that. 
N0 <- c(0, 0, 0, 54)

p<- pop.projection(A=A, n=N0, iterations=42)
p

#Plot when the tree swallows will stabilize
par(mar=c(2.5, 2.5, 0.5, 0.5))
stage.vector.plot(stage.vectors = p$stage.vectors, col=2:4, mgp=c(1.2,0.4, 0))

#Plot how the tree swallow population change stabilizes
lambda <- p$pop.changes
time <- c(2:42)
plot(lambda~time, ylab="Population growth", xlab="Years after grid establishment")

#Plot how the population changes over time
p2 <- project(A=A, vector=N0, time=42)
plot(p2 )
