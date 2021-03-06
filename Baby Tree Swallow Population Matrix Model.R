#Baby matric model
library(dplyr)
library(popdemo)
library(popbio)


#We are only including birds who were part of the breeding population!



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
meanclutchesF <- mean(Freprod$nestsinyear, na.rm = T) #1.066505
#Does it differ if you are SY vs ASY?
SYFreprod <- reprod %>% filter(sex=="F" & age== "SY")
meanclutchesSYF <- mean(SYFreprod$nestsinyear, na.rm=T) #1.035661
sd(SYFreprod$nestsinyear)

ASYFreprod <- reprod %>% filter(sex=="F" & age != "SY")
meanclutchesASYF <- mean(ASYFreprod$nestsinyear, na.rm=T) #1.074427
sd(ASYFreprod$nestsinyear)

#mean number of nests per male
Mreprod <- reprod%>% filter(sex=="M")
meanclutchesM <- mean(Mreprod$nestsinyear, na.rm = T) #1.091163
######################################################################################

#Now calculate average clutch size, hatch size, and fledge success.
firsteggdate<- rep(NA, length(as.list(globalData$nests)))
clutch<- rep(NA, length(as.list(globalData$nests)))
hatch <- rep(NA, length(as.list(globalData$nests)))
fledge <- rep(NA, length(as.list(globalData$nests)))
FAge <- rep(NA, length(as.list(globalData$nests)))
FBand <- rep(NA, length(as.list(globalData$nests)))
year <- rep(NA, length(as.list(globalData$nests)))
Mrecruits <- rep(0, length(as.list(globalData$nests)))
Frecruits <- rep(0, length(as.list(globalData$nests)))
Urecruits <- rep(0, length(as.list(globalData$nests)))
parameters <- data.frame(firsteggdate, clutch, hatch, fledge, FBand, FAge, year, Mrecruits, Frecruits, Urecruits)

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
  if(nest$nestlings$length>0){
    for(nestlingEnvP in nest$nestlings$as.list()){
      nestling <- get(nestlingEnvP$m_key, globalData$nestlings)
      if(!is.na(nestling$nestlingTRES$m_key)){
        adult <- get(nestling$nestlingTRES$m_key, globalData$birds)
        if(adult$nestList$length>0){
          
            message("Joined breeding population ", adult$bandID, adult$sex)
          
          if(adult$sex=="M"){
            parameters$Mrecruits[i] <- parameters$Mrecruits[i] +1
            
          } else {
            if (adult$sex=="F"){
              parameters$Frecruits[i] <- parameters$Frecruits[i] +1
              
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
#if your experimental group is control then it's not actually an experiment....
parameters$experiment[which(parameters$experiment=="Control" | parameters$experiment=="control" | parameters$experiment=="CON")] <- NA


#removed these three nests where we are getting infinite clutch size because
#they had no eggs but a nestling was transfered in
parameters <- parameters [-c(which(parameters$hatch>parameters$clutch & parameters$clutch==0)), ]
parametersSYF <- parameters %>% filter(FAge=="SY")
parametersASYF <- parameters %>% filter(FAge!="SY" & !is.na(FAge))
 #Mean clutch size
meanClutchSize <- mean(parameters$clutch, na.rm=T)  #5.273177
hist(parameters$clutch)
abline(v=meanClutchSize)
clutchSD <- sd(parameters$clutch, na.rm = T)
abline(v=meanClutchSize+clutchSD, col="red")
abline(v=meanClutchSize-clutchSD, col="red")


descdist
#Does it differ by SY and ASY?
meanClutchSizeSYF <- mean(parametersSYF$clutch, na.rm=T) #4.946609
hist(parametersSYF$clutch)
sd(parametersSYF$clutch, na.rm = T)

meanclutchSizeASYF <- mean(parametersASYF$clutch, na.rm=T) #5.459556
sd(parametersASYF$clutch, na.rm=T)

meanclutchSizeUnexp <- mean(parametersUnExp$clutch, na.rm=T) #5.263739

t.test(parametersSYF$clutch, parametersASYF$clutch) #Are significantly different


#mean Layrate-- must be the same for males and females! (actually may differ bit
#if we account for extrapair paternity but I can't at all and need to just
#accept that this has been taken account in the averaging of the population
#since everyone is doing it and succumbing to it)
layrateF <- meanClutchSize * meanclutchesF #5.623868
#Mean layrate by age of female
layrateSY <- meanClutchSizeSYF * meanclutchesSYF #5.123011
layrateASY <- meanclutchSizeASYF * meanclutchesASYF #5.865897



#Can only calculate hatchrate for clutches laid-- already dealt with above though
hatchPar <- parameters %>% filter(!is.na(hatch) & clutch>0)
hatchrate <- mean(hatchPar$hatch/hatchPar$clutch) #0.7398289
sd(hatchPar$hatch/hatchPar$clutch)
#Does if differ by SY and ASY?
hatchParSY <- parameters %>% filter(!is.na(hatch) & !is.na(clutch) & FAge=="SY")
hatchrateSY <- mean(hatchParSY$hatch/ hatchParSY$clutch) #0.7660019
hatchParASY <- parameters %>% filter(!is.na(hatch) & !is.na(clutch) & FAge!="SY" & !is.na(FAge) & clutch>0)
hatchrateASY <- mean(hatchParASY$hatch/hatchParASY$clutch) #0.7913602
#Hatch success is slightly higher for ASY females but not much
t.test(hatchParASY$hatch/hatchParASY$clutch, hatchParSY$hatch/ hatchParSY$clutch)

#Fledge rate
fledgePar <- parameters %>% filter (hatch>0 & !is.na(fledge))
fledgerate <- mean(fledgePar$fledge/fledgePar$hatch) #0.628297



#Does fledge success vary by female age?
fledgeParSY <- fledgePar %>% filter(FAge=="SY")
fledgerateSY <- mean(fledgeParSY$fledge/fledgeParSY$hatch) #0.5870798
fledgeParASY <- fledgePar %>% filter(FAge=="ASY" & !is.na(FAge))
fledgerateASY <- mean(fledgeParASY$fledge/fledgeParASY$hatch) #0.6239397


#Estimate recruitment
recruitPar <- parameters %>% filter(fledge>0 )
recruitrate <- mean((recruitPar$Mrecruits + recruitPar$Frecruits+ recruitPar$Urecruits)/recruitPar$fledge) #0.0175883
#This is likely an underestimate because we don't catch all the birds every year--how do I correct for that?


#Does it differ by SY and ASY
recruitParSY <- parameters %>% filter(fledge>0 &  FAge == "SY" )
recruitrateSY <- mean((recruitParSY$Mrecruits+ recruitParSY$Urecruits + recruitParSY$Frecruits)/recruitParSY$fledge)
#0.01553525

recruitParASY <- parameters %>% filter(fledge>0 &  FAge != "SY" & !is.na(FAge))
recruitrateASY <- mean((recruitParASY$Mrecruits+ recruitParASY$Frecruits +recruitParASY$Urecruits)/recruitParASY$fledge)
#0.01961603

#OH shit wow it really does!
#Number of female recruits
length(which(parameters$Frecruits>0)) 

length(which(parameters$Mrecruits>0)) 


#Need to estimate return rates. I will also do this based on sex.
#Currently this is based on whether you come back or not (I'm assuming that if you come back you're breeding!)
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

Freturnrate <- nrow(Adults %>% filter (returnstatus=="returned" & sex=="F")) /
                        nrow(Adults %>% filter (sex=="F"))
#0.2344073
SYFreturnrate <- nrow(Adults %>% filter (returnstatus=="returned" & sex=="F" & age=="SY")) / 
                        nrow(Adults %>% filter (sex=="F" & age=="SY"))
#0.1522606
ASYFreturnrate <- nrow(Adults %>% filter (returnstatus=="returned" & sex=="F" & age!="SY" & !is.na(age))) /
                          nrow(Adults %>% filter (sex=="F" & age!="SY" & !is.na(age)))

#0.27142


Mreturnrate <- nrow(Adults %>% filter (returnstatus=="returned" & sex=="M")) /
  nrow(Adults %>% filter (sex=="M"))
#0.2739166
#Oh look at that! Males return about like a ASY female. That makes sense in relation to everyone else. 


#Need mean dates for laying eggs!
meanfirsteggdate <- mean (parameters$firsteggdate, na.rm=T) #140.1392
SYparameters <- parameters%>% filter (FAge=="SY")
meanSYfirsteggdate <- mean(SYparameters$firsteggdate, na.rm=T) #143.4294
ASYparameters <- parameters%>% filter (FAge!="SY" & !is.na(FAge))
meanASYfirsteggdate <- mean(ASYparameters$firsteggdate, na.rm=T) #138.2539


#######################
#Do any of the parameters I estimated above vary based on whether you are part of the experiments or not?

parametersExp <- parameters %>% filter(!is.na(experiment))
parametersExpSY <- parametersExp %>% filter(FAge=="SY")
parametersExpASY <- parametersExp %>% filter(FAge!="SY" & !is.na(FAge))
parametersUnExp <- parameters %>% filter(is.na(experiment))
parametersUnExpSY <- parametersUnExp %>% filter(FAge=="SY")
parametersUnExpASY <- parametersUnExp %>% filter(FAge!="SY" & !is.na(FAge))


mean(parametersExp$clutch, na.rm = T) #5.28461
mean(parametersExpSY$clutch, na.rm=T) #4.976
mean(parametersExpASY$clutch, na.rm=T) #5.340691

mean(parametersUnExp$clutch, na.rm = T) #5.264358
mean(parametersUnExpSY$clutch, na.rm = T) #4.944444
mean(parametersUnExpASY$clutch, na.rm = T) #5.362633

#Experiment doesn't really make a difference to clutch size
hatchparametersExp <- parametersExp %>% filter (clutch>0 & !is.na(hatch))
hatchparametersExpSY <- parametersExpSY %>% filter (clutch>0 & !is.na(hatch))
hatchparametersExpASY <- parametersExpASY %>% filter (clutch>0 & !is.na(hatch))

hatchparametersUnExp <- parametersUnExp %>% filter (clutch>0 & !is.na(hatch))
hatchparametersUnExpSY <- parametersUnExpSY %>% filter (clutch>0 & !is.na(hatch))
hatchparametersUnExpASY <- parametersUnExpASY %>% filter (clutch>0 & !is.na(hatch))

mean(hatchparametersExp$hatch/hatchparametersExp$clutch) #0.7435187
mean(hatchparametersExpSY$hatch/hatchparametersExpSY$clutch) #0.7624393
mean(hatchparametersExpASY$hatch/hatchparametersExpASY$clutch) #0.7481205

mean(hatchparametersUnExp$hatch/hatchparametersUnExp$clutch) #0.7383338
mean(hatchparametersUnExpSY$hatch/hatchparametersUnExpSY$clutch) #0.7451074
mean(hatchparametersUnExpASY$hatch/hatchparametersUnExpASY$clutch) #0.7552444
#Eh maybe very slightly but overall not much difference to hatch success


fledgeparametersExp <- parametersExp %>% filter(hatch>0 & !is.na(fledge))
fledgeparametersExpSY <- parametersExpSY %>% filter(hatch>0 & !is.na(fledge))
fledgeparametersExpASY <- parametersExpASY %>% filter(hatch>0 & !is.na(fledge))
fledgeparametersUnExp <- parametersUnExp %>% filter(hatch>0 & !is.na(fledge))
fledgeparametersUnExpSY <- parametersUnExpSY %>% filter(hatch>0 & !is.na(fledge))
fledgeparametersUnExpASY <- parametersUnExpASY %>% filter(hatch>0 & !is.na(fledge))

mean(fledgeparametersExp$fledge/fledgeparametersExp$hatch) #0.6404473
mean(fledgeparametersExpSY$fledge/fledgeparametersExpSY$hatch) #0.6147503
mean(fledgeparametersExpASY$fledge/fledgeparametersExpASY$hatch) #0.64047
mean(fledgeparametersUnExp$fledge/fledgeparametersUnExp$hatch) #0.6226796
mean(fledgeparametersUnExpSY$fledge/fledgeparametersUnExpSY$hatch) #0.5778127
mean(fledgeparametersUnExpASY$fledge/fledgeparametersUnExpASY$hatch) #0.6330943
#SY birds are fledging more nestlings in the experimental group than the non-experiemental birds

##########################################################
#OK now I have all the relevent rates so now I want to create the matrices and put them into a analysis

#I will look at only females-- therefore since I am assuming equal sex ratios 
#(seems valid after looking at Delmore et al. 2008) I will have to divide the
#lay rate in half and carry that through subsequently also currently treating SY
#and ASY females the same

stages <- c("egg", "nestling", "fledgelings", "SY", "ASY")
A <- matrix(0, nrow=5, ncol=5, dimnames = list(stages, stages))

A[1,4] <- layrateSY / 2 #(because we assume half will be male)
A[1,5] <- layrateASY / 2 #(because we assume half will be male)
#Now I know that hatch, fledge and recruitment differs a bit between SY and ASY
#female's nests but I don't know how to deal with this right now. 
A[2,1]<- hatchrate
A[3,2]<- fledgerate
A[4,3] <-  0.08936758
#I've stolen this number as the mean female recruitment rate after correcting for
#effort to catch males and females each year (See Effect of Adult Catch Effort
#on Recruitment file) 
A[5,4]<- SYFreturnrate

A[5,5]<- ASYFreturnrate
A
#Should both be true!
is.matrix_irreducible(A) 
#YAY
is.matrix_ergodic(A)
#YAY

#N0 is the initial population sizes. 
#I will set time 0 to be 1976, before breeding begins. There were 54 nests, so 54 females. Really this should be checked. There could have been floaters too but I won't know that. 
N0 <- c(0, 0,0, 16, 38)

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
plot(p2)
title(xlab="Years", ylab="Female Populaiton Size")



#Not lets do an eigen analysis

eigA <- eigen.analysis(A)
eigA
#We are most sensetive to changes in the recruitment rate. NO surprize there! Elasticity 
#is highest for return rate and hatch success (1% change in either will result in a 20% change in lambda)


recruitval<- seq(from=0.01, to= 1, by=0.01)

lam <- c()
A2 <- A
i=0
for (recruitment in recruitval){
  i=i+1
  A2[4,3]<- recruitment
  p2 <- pop.projection(A2, N0, 50)
  lam[i]<- p2$lambda
}
lam

plot(lam~recruitval, ylab="Population growth rate", xlab="Recruitment")
abline(h=1, col="red")

#Need to have about 67% return to have a stable population like this! That's
#pretty unreasonable....Perhaps I'm doing something else super wrong



hatchsuccess<- seq(from=0.01, to= 1, by=0.01)

lam <- c()
A2 <- A
i=0
for (h in hatchsuccess){
  i=i+1
  A2[2,1]<- h
  p2 <- pop.projection(A2, N0, 50)
  lam[i]<- p2$lambda
}
lam

plot(lam~hatchsuccess, ylab="Population growth rate", xlab="Hatch Success")
abline(h=1, col="red")
#changing hatchsuccess won't actually allow us to ever have a stable population (hatch success can't get over 1....)


