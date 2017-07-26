
PopData<- read.csv(file= "file:///C:/Users/Amelia/Documents/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Yearly Vital Rates.csv", na.strings=c(""), as.is=T)

#Function to estimate the parameters for a beta distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


#Calculate parameters for all the vital rates
plot(fledgeRate~year, data=PopData) #huh that could actually be interesting
hist(PopData$fledgeRate)
shapiro.test(PopData$fledgeRate) 
#not significnatly different from a normal distribution, but it does need to be
#bound by 0 and 1.... could I use a beta distribution anyway?
fledgeParameters <- estBetaParams(mu=mean(PopData$fledgeRate), var=var(PopData$fledgeRate))


plot(hatchRate~year, data=PopData)
shapiro.test(PopData$hatchRate) #definitely not normal
#Hatch rate is typically done using a beta distribution because it is bounded by
#0 and 1 on either side so I'll try that
hatchrateParameters <- estBetaParams(mu=mean(PopData$hatchRate), var=var(PopData$hatchRate))


plot(clutchSize~year, data=PopData)
shapiro.test(PopData$clutchSize) #can totally use a normal distribution

plot(averageNests~year, data=PopData, ylim=c(0.8, 1.5))
shapiro.test(PopData$averageNests) #not normal
fitdistrplus::descdist(PopData$averageNests-1, discrete = F) 
#looks like a beta but we range from 1 to 1.94, so that's impossible.... One way
#to deal with that would be to do the number of actual renests (ie subtract 1
#for the first nest!) That works beautifully
averageNestParameters<- estBetaParams(mu=mean(PopData$averageNests-1), var=var(PopData$averageNests-1))

#These numbers are from the RMark script. They will likely need to be changed. 
SYReturn <-0.294157 #ts8$results$real$estimate[1] 
SYReturnSE <-0.0100714 #ts8$results$real$se[1] 


ASYReturn <-0.4599774 #ts8$results$real$estimate[2] 
ASYReturnSE <-0.0154843 #ts8$results$real$se[2] 


#A better way to deal with the recruit and return rates would be to use the
#estimates from the best RMark models using discrete time and then use those
#estimates to create a distribution

#Start with the nestlings 
nestlingMark <- readRDS( "Best MARK Results for Vital Rates Nestlings Analysis.rda")
summary <- summary(nestlingMark)
PopData$recruitment <- c(summary$reals$Phi$`Group:ageHY.sexF`[[1]][1,], NA)
#we'll use a beta distribution
recruitmentParameters <- estBetaParams(mu=mean(PopData$recruitment, na.rm=T), var=var(PopData$recruitment, na.rm=T))


FemaleMark <- readRDS("Best MARK Results for Vital Rates Adults Analysis.rda")
summaryF <- summary(FemaleMark)
# I need to pull the number out on the diagonal for the SY birds
for(i in 1:42){
  PopData$SYReturn[i] <- summaryF$reals$Phi$`Group:age1`[[1]][i,i]
  
}
#remove 1975 from both of these columns-- it's set to 1 for both the SY and ASY
#but so few birds were banded it's a terrible estimate
PopData$SYReturn[1]<- NA
SYReturnParameters<- estBetaParams(mu=mean(PopData$SYReturn, na.rm=T ), var=var(PopData$SYReturn, na.rm=T))
PopData$ASYReturn <- c(summaryF$reals$Phi$`Group:age2`[[1]][1,], NA)
PopData$ASYReturn[1]<- NA

ASYReturnParameters<- estBetaParams(mu=mean(PopData$ASYReturn, na.rm=T), var=var(PopData$ASYReturn, na.rm=T))



#Are there inherent correlations between the different vital rates that will
#need to be taken into account when we generate randomly?
cor(PopData, method = "pearson", use="complete.obs")
res2 <-Hmisc::rcorr(as.matrix(PopData), type = c("pearson"))
#Borrowed from online--makes the matrices readable....
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P)
#I will need to include a huge number of correlations in my matrix now!! Oh my
#goodness. Survival correlates with pretty much everthing which makes a lot of sense. 






#Vital Rates analysis drawing repeatedly from the known distributions Currently
#we are not seperating out SY and ASY clutch sizes etc, even though ultimately
#we may want to.

vrdat <- as.data.frame(matrix(nrow=10000, ncol=9)) 
#Doing 10,000 because that's what Taylor et al 2012 did)
colnames(vrdat)<- c("hatchrate", "fledgerate", "recruitrate", "eggRecruitRate", "clutchSize", "averageNests", "SYReturn", "ASYReturn", "lambda" )

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))
N0 <- c(0, 16 , 38)
for (i in 1:nrow(vrdat)){
  
  vrdat$averageNests[i] <- 1+ rbeta (n=1, shape1= averageNestParameters$alpha , shape2=averageNestParameters$beta) #oesn't work because neither should be negative! What's wrong with this?
  
  
  vrdat$clutchSize[i] <- rnorm(n=1, sd=sd(PopData$clutchSize), mean=mean(PopData$clutchSize))
  vrdat$layrate[i]<- vrdat$clutchSize[i]*vrdat$averageNests[i]
  
  #Pull out a a random ratch rate, fledge rate, renest status and clutch size from the vectors I made above. 
  vrdat$hatchrate[i] <- rbeta (n=1, shape1= hatchrateParameters$alpha , shape2=hatchrateParameters$beta)
  
  vrdat$fledgerate[i] <- rbeta (n=1, shape1= fledgeParameters$alpha , shape2=fledgeParameters$beta)
  #recruitment is randomly generated from the data that I brought to RMark
  vrdat$recruitrate[i] <-  rbeta (n=1, shape1= recruitmentParameters$alpha , shape2=recruitmentParameters$beta)
  
  vrdat$eggRecruitRate[i] <- vrdat$hatchrate[i] * vrdat$fledgerate[i] * vrdat$recruitrate[i]
  
  
  vrdat$SYReturn[i] <- rbeta (n=1, shape1= SYReturnParameters$alpha , shape2=SYReturnParameters$beta)
  vrdat$ASYReturn[i] <- rbeta (n=1, shape1= ASYReturnParameters$alpha , shape2=ASYReturnParameters$beta)

  
  #Put those vital rates into the matrix
  #Rows= what stage the birds are in now
  #Columns=what stage the birds are going to
  A[1, 2] <- vrdat$layrate[i] 
  A[1, 3] <- vrdat$layrate [i]
  A[2, 1] <- vrdat$eggRecruitRate[i]
  A[3, 2] <- vrdat$SYReturn[i]
  A[3, 3] <- vrdat$ASYReturn[i]
  p<- pop.projection(A=A, n=N0, iterations=42)
  
  vrdat$lambda[i] <- p$lambda
  
}



#Now for some preliminary plotting
plot(lambda~ASYReturn, data=vrdat)
plot(lambda~SYReturn, data=vrdat)
plot(lambda~layrate, data=vrdat)
plot(lambda~eggRecruitRate, data=vrdat) #Ooooo we have a winner!!
#What within the eggRecruitRate is important? \
plot(lambda~recruitrate, data=vrdat) #There's so little variation in the recruitrate that it's not as important!
plot(lambda~hatchrate, data=vrdat) #hatch rate looks important!
plot(lambda~fledgerate, data=vrdat)  #fledge rate is likely most important



#Now I use a simple linear regression to figure out what the correlation
#coefficients are most sensitive following Taylow et al. 2012
modASYReturn <- lm(lambda~ ASYReturn, data=vrdat)
summary(modASYReturn) #Adjusted R^2 = 0.007159 so quite unimportant

modSYReturn <- lm(lambda~ SYReturn, data=vrdat)
summary(modSYReturn) # Adjusted R^2 = 0.02582 so even less important

modClutchSize <- lm(lambda ~ clutchSize, data=vrdat)
summary(modClutchSize) #Adjusted R^2 = 0.002936 so it's not that!

modAverageNests<- lm(lambda ~ averageNests, data=vrdat) 
summary(modAverageNests) #Adjusted R^2 =0.04222, better but not a whole lot of the variation

modEggtoAdult <- lm(lambda~eggRecruitRate, data=vrdat)
summary(modEggtoAdult) #Adjusted R^2 = 0.8759 so explains almost all the variation!
#What within this explains variation? 

modHatch <- lm(lambda ~ hatchrate, data=vrdat)
summary(modHatch) # Adjussted R^2 =0.02609 so that's a huge portion of the variation!!!

modFledge <- lm (lambda ~fledgerate, data=vrdat)
summary(modFledge) #Adjusted R^2 =0.1907 so another large portion of the variation, 

modRecruit <- lm(lambda~recruitrate, data=vrdat)
summary(modRecruit) #R^2= 0.6508

#Now I use a log-log transformed regression to figure out what aspects have the 
#highest elasticity 
modASYReturn_elasticity <- lm(log(lambda)~ log(ASYReturn), data=vrdat)
summary(modASYReturn_elasticity) #R^2=0.008559

modSYReturn_elasticity <- lm(log(lambda) ~ log(SYReturn), data=vrdat)
summary(modSYReturn_elasticity) #R^2 = 0.0225

modClutchSize_elasticity <- lm(log(lambda) ~ log(clutchSize), data=vrdat)
summary(modClutchSize_elasticity) #Adjusted R^2 = 0.002293 so it's not that!

modAverageNests_elasticity<- lm(log(lambda) ~ log(averageNests), data=vrdat) 
summary(modAverageNests_elasticity) #Adjusted R^2 =0.03216, better but not a whole lot of the variation

modEggtoAdult_elasticity <- lm(log(lambda)~log(eggRecruitRate), data=vrdat)
summary(modEggtoAdult_elasticity) #R^2 = 0.89

plot(log(lambda)~log(hatchrate), data=vrdat)
modhatch_elasticity <- lm(log(lambda)~log(hatchrate), data=vrdat)
summary(modhatch_elasticity) #R^2 =0.02237 so low elasticity

modFledge_elasticity <- lm (log(lambda) ~log(fledgerate), data=vrdat)
summary(modFledge_elasticity) #Adjusted R^2 =0.1802 so another large portion of the variation, 

modRecruit_elasticity <- lm(log(lambda)~log(recruitrate), data=vrdat)
summary(modRecruit_elasticity) #r^2=0.691

#Keep in mind that this is still not including the correlation structure. 
