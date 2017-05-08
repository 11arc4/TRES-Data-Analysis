#Vital Rates Analysis
# nestVR <- as.data.frame(matrix(nrow=4810, ncol=6))
# colnames(nestVR)<- c("SYNrenests", "ASYNrenests", "SYclutchsize", "ASYclutchsize", "hatchrate", "fledgerate")
#   
# nSYrenests <- length(SYFreprod$nestsinyear [which(!is.na(SYFreprod$nestsinyear))])  
# nestVR$SYNrenests[1:nSYrenests] <-   SYFreprod$nestsinyear [which(!is.na(SYFreprod$nestsinyear))]
# 
# nASYrenests <- length(ASYFreprod$nestsinyear [which(!is.na(ASYFreprod$nestsinyear))])
# nestVR$ASYNrenests[1:nASYrenests]  <- ASYFreprod$nestsinyear [which(!is.na(ASYFreprod$nestsinyear))]
# 
# nestVR$SYclutchsize[1:length(parametersSYF$clutch [which(!is.na(parametersSYF$clutch))])] <-  parametersSYF$clutch [which(!is.na(parametersSYF$clutch))]
# nestVR$ASYclutchsize[1:length(parametersASYF$clutch [which(!is.na(parametersASYF$clutch))])]  <- parametersASYF$clutch [which(!is.na(parametersASYF$clutch))]
# nestVR$hatchrate[1:length( hatchPar$hatch/ hatchPar$clutch[which(!is.na(hatchPar$hatch/ hatchPar$clutch))])] <-  hatchPar$hatch/ hatchPar$clutch[which(!is.na(hatchPar$hatch/ hatchPar$clutch))]
# nestVR$fledgerate[1:length(fledgePar$fledge/ fledgePar$hatch[which(!is.na(fledgePar$fledge/ fledgePar$hatch))])] <-  fledgePar$fledge/ fledgePar$hatch[which(!is.na(fledgePar$fledge/ fledgePar$hatch))]
library(popbio)

write.csv(nestVR, file="~/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Vital Rates.csv", row.names = F )
nestVR <- read.csv("~/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Vital Rates.csv", na.strings=c("NA"), as.is=T)


vrdat <- as.data.frame(matrix(nrow=10000, ncol=13))
colnames(vrdat)<- c("hatchrate", "fledgerate", "recruitrate", "eggRecruitRate", "SYrenests", "SYclutchsize", "SYlayrate", "ASYrenests", "ASYclutchsize", "ASYlayrate", "SYReturn", "ASYReturn", "lambda" )

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))
N0 <- c(0, 16 , 38)
for (i in 1:nrow(vrdat)){
  #Pull out a a random ratch rate, fledge rate, renest status and clutch size from the vectors I made above. 
  vrdat$hatchrate[i] <-  sample (nestVR$hatchrate[which(!is.na(nestVR$hatchrate))], 1)
  vrdat$fledgerate[i] <- sample (nestVR$fledgerate[which(!is.na(nestVR$fledgerate))], 1)
  #recruitment is randomly generated from the data that I brought to RMark
  vrdat$recruitrate[i] <-  rnorm(n=1, mean=0.1663771, sd=0.0124860) #Taken from my RMark analysis of the nesltings

  vrdat$eggRecruitRate[i] <- vrdat$hatchrate[i] * vrdat$fledgerate[i] * vrdat$recruitrate[i]
  
  vrdat$SYrenests[i] <- sample(nestVR$SYNrenests[which(!is.na(nestVR$SYNrenests))], 1)
  vrdat$SYclutchsize[i] <- sample(nestVR$SYclutchsize[which(!is.na(nestVR$SYclutchsize))], 1) / 2 #since only half are female
  vrdat$SYLayrate[i] <- vrdat$SYrenests[i] *vrdat$SYclutchsize[i]  
    
  vrdat$ASYrenests[i] <- sample(nestVR$ASYNrenests[which(!is.na(nestVR$ASYNrenests))],1)
  vrdat$ASYclutchsize[i] <- sample(nestVR$ASYclutchsize[which(!is.na(nestVR$ASYclutchsize))],1) / 2 #since only half are female
  vrdat$ASYLayrate [i] <- vrdat$ASYrenests[i] * vrdat$ASYclutchsize[i]  
  
  vrdat$SYReturn[i] <- rnorm(n=1, mean=0.2941570, sd=0.0100714)#Taken from my RMark analysis of the adults (Phi=age, p=time)
  vrdat$ASYReturn[i] <- rnorm(n=1, mean=0.4599774 , sd=0.0154843)#Taken from my RMark analysis of the adults (Phi=age, p=time)
  #For all of the recruitment and return measurements I chose to use the model
  #in MARK that had the lowest AIC scores and didn't let Phi change over time
  
  #Put those vital rates into the matrix
  #Rows= what stage the birds are in now
  #Columns=what stage the birds are going to
  A[1, 2] <- vrdat$SYLayrate[i] 
  A[1, 3] <- vrdat$ASYLayrate [i]
  A[2, 1] <- vrdat$eggRecruitRate[i]
  A[3, 2] <- vrdat$SYReturn[i]
  A[3, 3] <- vrdat$ASYReturn[i]
  p<- pop.projection(A=A, n=N0, iterations=42)
  
  vrdat$lambda[i] <- p$lambda

}



plot(lambda~ASYReturn, data=vrdat)

plot(lambda~SYReturn, data=vrdat)
plot(lambda~SYLayrate, data=vrdat)
plot(lambda~ASYLayrate, data=vrdat)
plot(lambda~eggRecruitRate, data=vrdat) #Ooooo we have a winner!!
#What within the eggRecruitRate is important? \
plot(lambda~recruitrate, data=vrdat) #There's so little variation in the recruitrate that it's not as important!
plot(lambda~hatchrate, data=vrdat) #I think this means that you need both hatchrate and fledgerate to be high
plot(lambda~fledgerate, data=vrdat) 

modASYR <- lm(lambda~ASYReturn, data=vrdat)
plot(modASYR) #The Q-Q plot looks like shit
hist(resid(modASYR)) #Oh those residuals really aren't normal even a little bit 
#Oh it's that damn straight line. I have no idea why we have that..... #I guess it's because there's a hard minimum if we have no eggs recruit that year
summary(modASYR)
#R^2=0.002134 

modSYR <- lm(lambda~SYReturn, data=vrdat)
plot(modSYR)
summary(modSYR)
#R^2 = -6.094e-06 

#Basically return rates aren't a big driver of lambda


modSYlay <- lm(lambda~SYLayrate, data=vrdat)
summary(modSYlay)dd
#R^2 <0.01302

modASYlay <- lm(lambda~ASYLayrate, data=vrdat)
summary(modASYlay)
#R^2 < 0.01423 

modeggRecruit <- lm(lambda~eggRecruitRate, data=vrdat)
summary(modRecruit)
#R^2 = 0.924
#OOOO there's an actual R^2. Let's investigate further. 


modrecruit <- lm(lambda~recruitrate, data=vrdat)
summary(modrecruit)
#R^2= 0.002382

modhatch <- lm(lambda~hatchrate, data=vrdat)
summary(modhatch)
#R^2 = 0.2675
modfledge <- lm(lambda~fledgerate, data=vrdat)
summary(modfledge)
#R^2=0.5471 THIS IS THE MOSt IMPORTANT PARAMETER BY THIS ESTIMATE









################################ I feel a bit weird about using nest level data
#to parameterize the model since the fledge, hatch, and and all those rates are
#nest level. Perhaps it would be better to calculate population level rates for
#each of the years and then use that to parameterize the model
yearlyParam <- as.data.frame(matrix(nrow=42, ncol=5))
colnames(yearlyParam)<- c("year", "SYclutch", "ASYclutch", "hatchrate", "fledgerate")
r=0
for (y in 1975:2016){
  r=r+1
  yearly <- parameters %>% filter (year==y)
  yearlyParam$year[r] <- y
  yearlyParam$SYclutch[r]<- mean(yearly[which(yearly$FAge=="SY"), "clutch"], na.rm=T)
  yearlyParam$ASYclutch[r]<- mean(yearly[which(yearly$FAge=="ASY"), "clutch"], na.rm=T)
  
  yearlyParam$hatchrate[r] <- mean(yearly[which(yearly$clutch>0 & !is.na(yearly$hatch)), "hatch" ] /
                                     yearly[which(yearly$clutch>0 & !is.na(yearly$hatch)), "clutch" ])
  yearlyParam$fledgerate[r] <- mean(yearly[which(yearly$hatch>0 & !is.na(yearly$fledge)), "fledge" ] /
                                     yearly[which(yearly$hatch>0 & !is.na(yearly$fledge)), "hatch" ])
}

plot(SYclutch~year, data=yearlyParam)
plot(ASYclutch~year, data=yearlyParam)
plot(hatchrate~year, data=yearlyParam)
plot(fledgerate~year, data=yearlyParam)


hist(yearlyParam$hatchrate)
abline(v=mean(yearlyParam$hatchrate), col="red")
abline(v=mean(yearlyParam$hatchrate)+sd(yearlyParam$hatchrate), col="blue")
abline(v=mean(yearlyParam$hatchrate)-sd(yearlyParam$hatchrate), col="blue")

mean(yearlyParam$hatchrate)
sd(yearlyParam$hatchrate)
shapiro.test(yearlyParam$hatchrate) #it's normal

hist(yearlyParam$fledgerate)
abline(v=mean(yearlyParam$fledgerate), col="red")
abline(v=mean(yearlyParam$fledgerate)+sd(yearlyParam$fledgerate), col="blue")
abline(v=mean(yearlyParam$fledgerate)-sd(yearlyParam$fledgerate), col="blue")

mean(yearlyParam$fledgerate)
sd(yearlyParam$fledgerate)
shapiro.test(yearlyParam$fledgerate) #it's normal


library(fitdistrplus)
descdist(yearlyParam$SYclutch[which(!is.na(yearlyParam$SYclutch))],  discrete=T) #looks like a negative binomial 
rnbinom(n=1, size= ??????, mu=5.063771)

#Hmmmm Unsure how to get the best estimates for clutch size. Maybe could I mix the methods and use population level 