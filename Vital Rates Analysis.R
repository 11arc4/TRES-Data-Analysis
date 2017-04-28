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
  

write.csv(nestVR, file="~/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Vital Rates.csv", row.names = F )
nestVR <- read.csv("~/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Vital Rates.csv", na.strings=c("NA"), as.is=T)


vrdat <- as.data.frame(matrix(nrow=1000, ncol=13))
colnames(vrdat)<- c("hatchrate", "fledgerate", "recruitrate", "eggRecruitRate", "SYrenests", "SYclutchsize", "SYlayrate", "ASYrenests", "ASYclutchsize", "ASYlayrate", "SYReturn", "ASYReturn", "lambda" )

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))
N0 <- c(0, 16 , 38)
for (i in 1:1000){
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


