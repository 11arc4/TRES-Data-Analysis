#Baby matric model
library(dplyr)
library(popdemo)
library(popbio)


#We are only including birds who were part of the breeding population



#Calculate the average number of nests a bird is involved in 
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

#calculate the mean number of nests for females 
Freprod<- reprod %>% filter(sex=="F")
meanclutchesF <- mean(Freprod$nestsinyear, na.rm = T) #0.7154278
#Does it differ if you are SY vs ASY?
SYFreprod <- reprod %>% filter(sex=="F" & age== "SY")
meanclutchesSYF <- mean(SYFreprod$nestsinyear, na.rm=T) #0.4108068

ASYFreprod <- reprod %>% filter(sex=="F" & age != "SY")
meanclutchesASYF <- mean(ASYFreprod$nestsinyear, na.rm=T) #0.8410131

#mean number of nests per male
Mreprod <- reprod%>% filter(sex=="M")
mean(Mreprod$nestsinyear, na.rm = T) #0.7851204
######################################################################################

#Now calculate average clutch size, hatch size, and fledge success.
clutch<- rep(NA, length(as.list(globalData$nests)))
hatch <- rep(NA, length(as.list(globalData$nests)))
fledge <- rep(NA, length(as.list(globalData$nests)))
FAge <- rep(NA, length(as.list(globalData$nests)))
FBand <- rep(NA, length(as.list(globalData$nests)))
year <- rep(NA, length(as.list(globalData$nests)))
Mrecruits <- rep(0, length(as.list(globalData$nests)))
Frecruits <- rep(0, length(as.list(globalData$nests)))
Urecruits <- rep(0, length(as.list(globalData$nests)))
parameters <- data.frame(clutch, hatch, fledge, FBand, FAge, year, Mrecruits, Frecruits, Urecruits)

i=0
for(nest in as.list(globalData$nests)){
  i=i+1
  parameters$clutch[i] <- nest$clutchSize
  parameters$hatch[i] <- nest$hatchSize
  parameters$fledge[i] <- nest$fledgeSize
  parameters$year[i]<- nest$year
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
  if(nest$nestlings$length>0){
    for(nestlingEnvP in nest$nestlings$as.list()){
      nestling <- get(nestlingEnvP$m_key, globalData$nestlings)
      if(!is.na(nestling$nestlingTRES$m_key)){
        adult <- get(nestling$nestlingTRES$m_key, globalData$birds)
        if(adult$yearsSeen$length>1){
          if(adult$nestList$length>0){
            message("Joined breeding population ", adult$bandID)
          }
          if(adult$sex=="M"){
            parameters$Mrecruits[i] <- parameters$Mrecruits[i] +1
            
          } else {
            if (adult$sex=="F"){
              parameters$Urecruits[i] <- parameters$Frecruits[i] +1
              
            } else {
              parameters$Urecruits[i] <- parameters$Urecruits[i] +1
              
            }
          }
          parameters$recruits[i] <- parameters$recruits[i] +1
        } 
      }
      
    }
  }
  
}




#removed these three nests where we are getting infinite clutch size because
#they had no eggs but a nestling was transfered in
parameters <- parameters [-c(which(parameters$hatch>parameters$clutch & parameters$clutch==0)), ]
parametersSYF <- parameters %>% filter(FAge=="SY")
parametersASYF <- parameters %>% filter(FAge!="SY" & !is.na(FAge))
 #Mean clutch size
meanClutchSize <- mean(parameters$clutch, na.rm=T)  #5.273177
#Does it differ by SY and ASY?
meanClutchSizeSYF <- mean(parametersSYF$clutch, na.rm=T) #4.946609
meanclutchSizeASYF <- mean(parametersASYF$clutch, na.rm=T) #5.459556

#mean Layrate
layrate <- meanClutchSize * meanclutches #3.772577
#Mean layrate by age of female
layrateSY <- meanClutchSizeSYF * meanclutchesSYF #2.032101
layrateASY <- meanclutchSizeASYF * meanclutchesASYF #4.591558


#Can only calculate hatchrate for clutches laid-- already dealt with above though
hatchPar <- parameters %>% filter(!is.na(hatch) & clutch>0)
hatchrate <- mean(hatchPar$hatch/hatchPar$clutch) #0.7398289
#Does if differ by SY and ASY?
hatchParSY <- parameters %>% filter(!is.na(hatch) & !is.na(clutch) & FAge=="SY")
hatchrateSY <- mean(hatchParSY$hatch/ hatchParSY$clutch) #0.7660019
hatchParASY <- parameters %>% filter(!is.na(hatch) & !is.na(clutch) & FAge!="SY" & !is.na(FAge) & clutch>0)
hatchrateASY <- mean(hatchParASY$hatch/hatchParASY$clutch) #0.7913602
#Hatch success is slightly higher for ASY females but not much


#Fledge rate
fledgePar <- parameters %>% filter (hatch>0 & !is.na(fledge))
fledgerate <- mean(fledgePar$fledge/fledgePar$hatch) #0.6280591
#Does fledge success vary by female age?
fledgeParSY <- fledgePar %>% filter(FAge=="SY")
fledgerateSY <- mean(fledgeParSY$fledge/fledgeParSY$hatch) #0.5870798
fledgeParASY <- fledgePar %>% filter(FAge=="ASY" & !is.na(FAge))
fledgerateASY <- mean(fledgeParASY$fledge/fledgeParASY$hatch) #0.6234409


#Estimate recruitment
###THIS IS ALL MESSED UP NOW-- ESIMATING WRONG
recruitPar <- parameters %>% filter(fledge>0 )
recruitrate <- mean((recruitPar$Mrecruits + recruitPar$Frecruits+ recruitPar$Urecruits)/recruitPar$fledge) #0.01979139


#Does it differ by SY and ASY
recruitParSY <- parameters %>% filter(fledge>0 &  FAge == "SY" )
recruitrateSY <- mean((recruitParSY$Mrecruits+ recruitParSY$Urecruits + recruitParSY$Frecruits)/recruitParSY$fledge) #0.0154047

recruitParASY <- parameters %>% filter(fledge>0 &  FAge != "SY" & !is.na(FAge))
recruitrateASY <- mean((recruitParASY$Mrecruits+ recruitParASY$Frecruits +recruitParASY$Urecruits)/recruitParASY$fledge) #0.02253803

#OH shit wow it really does!



#Need to estimate return rates. I will also do this based on sex. 
returnstatus <- rep(NA, 82000)
Adults <- data.frame(returnstatus)
Adults$sex <- rep(NA, 82000)
Adults$age<- rep(NA, 82000)
Adults$band <- rep(NA, 82000)

i <- 0
for (bird in as.list(globalData$birds)){
  if(bird$yearsSeen$length>1){
    #message("Sorting years")
    l2 <- bird$yearsSeen$as.list()
    l3 <- l2[order(sapply(l2, function(v) { v$year} ))]
    bird$yearsSeen$replaceList(l3)
  }
  y=0
  for (year in bird$yearsSeen$as.list()){
    i=i+1
    y=y+1
    Adults$age[i] <- year$age 
    if (y<bird$yearsSeen$length){
      Adults$returnstatus[i]<- "returned"
    } else {
      Adults$returnstatus[i] <- "died"
    }
    Adults$sex[i] <- bird$sex
    Adults$band[i] <- bird$bandID
  }
}

Adults <- Adults[which(!is.na(Adults$band)),]
#make the adults actually only the adults
Adults <- Adults[which(Adults$age != "HY"),]
Adults$band[which(Adults$returnstatus=="Recruit")]

Adults$sex[which(Adults$returnstatus=="Return" & Adults$sex=="F")]
FReturn <- length()




FReturn <- Adults %>% filter(sex=="F")
MReturn <- Adults %>% filter(sex=="M")




Freturnrate <-mean(FReturn$return) #0.2307692
Mreturnrate <- mean(MReturn$return) #0.2730853



##########################################################
#OK now I have all the relevent rates so now I want to create the matrices and put them into a analysis

#I will look at only females-- therefore since I am assuming equal sex ratios
#(seems valid after looking at Delmore et al. 2008) I will have to divide all
#the rates in half
#also currently treating SY and ASY females the same

stages <- c("egg", "nestling", "fledgelings", "Adult")
A <- matrix(0, nrow=4, ncol=4, dimnames = list(stages, stages))

A[1,4]<- mean(parameters$clutch, na.rm=T)/2 #I'm going to use this instead of layrate for a moment (this assumes 1 clutch per bird)
#Assume the same rates for males and females
A[2,1]<- hatchrate
A[3,2]<- fledgerate
A[4,3] <- recruitrateoffledgelings 
#Don't need to assume the same rates for males and females
A[4,4]<- Freturnrate

A[4,4]<-0.23
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



#Not lets do an eigen analysis

eigA <- eigen.analysis(A)
eigA
#We are most sensetive to changes in the recruitment rate
#Elasticity is highest in the fledging and recritment rate. For a 1% change, we will have a 20.4% change in lambda for both of these


return<- seq(from=0.01, to= 0.7, by=0.01)

lam <- c()
A2 <- A
i=0
for (returnrate in return){
  i=i+1
  A2[4,4]<- returnrate
  p2 <- pop.projection(A2, N0, 50)
  lam[i]<- p2$lambda
}
lam

plot(lam~return, ylab="Population growth rate", xlab="Adult Return")
abline(h=1, col="red")

#Even with the assumption that all birds get one nest per year we still are having trouble. I think this is maybe 
