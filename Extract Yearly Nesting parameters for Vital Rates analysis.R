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


a <- 0
for (y in 1975:2016){
  a<- a+1
  #fill in the average number of renests (for females) known in that year
  PopData$averageNests[a] <- 
    sum(reprod$nestsinyear[ which(reprod$year==y & reprod$sex=="F")]) / length(which(reprod$year==y & reprod$sex=="F")) 
  #Fill in average clutch size that year
  clutchnests <- parameters %>% filter (!is.na(clutch) & year==y)
  PopData$clutchSize[a] <-  sum(clutchnests$clutch) / nrow(clutchnests)
  #Fill in average hatchrate
  hatchnests <- parameters %>% filter (!is.na(clutch) & !is.na(hatch) & year==y)
  PopData$hatchRate[a] <- sum(hatchnests$hatch) / sum (hatchnests$clutch)
  #fill in average fledge size
  fledgenests <- parameters %>% filter (!is.na(fledge) & !is.na(hatch) & year==y)
  PopData$fledgeRate[a] <- sum(fledgenests$fledge) / sum(fledgenests$hatch)
}


plot(fledgeRate~year, data=PopData) #huh that could actually be interesting
hist(PopData$fledgeRate)
shapiro.test(PopData$fledgeRate) #not significnatly different from a normal distribution
plot(hatchRate~year, data=PopData)
shapiro.test(PopData$hatchRate) #definitely not normal
fitdistrplus::descdist(PopData$hatchRate, discrete = F) #Propbably a logistic distribution
#For a logistic distribution you just need location = m and scale = s, mean m and variance pi^2 (s^2)/3. 
s=sqrt(pi^2 * var(PopData$hatchRate)/3)
l= mean(PopData$hatchRate)


plot(clutchSize~year, data=PopData)
shapiro.test(PopData$clutchSize) #can totally use a normal distribution
plot(averageNests~year, data=PopData, ylim=c(0.8, 1.5))
shapiro.test(PopData$averageNests) #not normal
fitdistrplus::descdist(PopData$averageNests-1, discrete = F) 
#looks like a beta but we range from 1 to 1.94, so that's impossible.... One way
#to deal with that would be to do the number of actual renests (ie subtract 1
#for the first nest!) That works beautifully
summary(PopData$averageNests)

#Function to estimate the parameters for random generation later on
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
#my paramters
averageNestParameters<- estBetaParams(mu=mean(PopData$averageNests-1), var=var(PopData$averageNests-1))


SYReturn <-0.294157 #ts8$results$real$estimate[1] 

SYReturnSE <-0.0100714 #ts8$results$real$se[1] 


ASYReturn <-0.4599774 #ts8$results$real$estimate[2] 

ASYReturnSE <-0.0154843 #ts8$results$real$se[2] 


ts_vitalRates1$results$real$estimate #THese are all the estimates
ts_vitalRates1$results$real

#Vital Rates analysis drawing repeatedly from the known distributions
#Currently we are not seperating out SY and ASY clutch sizes etc, even though ultimately we may want to. 
vrdat <- as.data.frame(matrix(nrow=10000, ncol=9)) #Doing 10,000 because that's what Taylor et al 2012 did)
colnames(vrdat)<- c("hatchrate", "fledgerate", "recruitrate", "eggRecruitRate", "clutchSize", "averageNests", "SYReturn", "ASYReturn", "lambda" )

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))
N0 <- c(0, 16 , 38)
for (i in 1:nrow(vrdat)){
  
  vrdat$averageNests[i] <- 1+ rbeta (n=1, shape1= averageNestParameters$alpha , shape2=averageNestParameters$beta) #oesn't work because neither should be negative! What's wrong with this?
  
  
  vrdat$clutchSize[i] <- rnorm(n=1, sd=sd(PopData$clutchSize), mean=mean(PopData$clutchSize))
  vrdat$layrate[i]<- vrdat$clutchSize[i]*vrdat$averageNests[i]
  
  #Pull out a a random ratch rate, fledge rate, renest status and clutch size from the vectors I made above. 
  vrdat$hatchrate[i] <- rlogis(n=1, location=mean(PopData$hatchRate), scale= sqrt(pi^2 * var(PopData$hatchRate)/3)) 
  
  vrdat$fledgerate[i] <- rnorm(n=1, sd=sd(PopData$fledgeRate), mean=mean(PopData$fledgeRate))
  #recruitment is randomly generated from the data that I brought to RMark
  vrdat$recruitrate[i] <-  rnorm(n=1, mean=0.1663771, sd=0.0124860) #Taken from my RMark analysis of the nesltings
  
  vrdat$eggRecruitRate[i] <- vrdat$hatchrate[i] * vrdat$fledgerate[i] * vrdat$recruitrate[i]
  
  
  vrdat$SYReturn[i] <- rnorm(n=1, mean=0.2941570, sd=0.0100714)#Taken from my RMark analysis of the adults (Phi=age, p=time)
  vrdat$ASYReturn[i] <- rnorm(n=1, mean=0.4599774 , sd=0.0154843)#Taken from my RMark analysis of the adults (Phi=age, p=time)
  #For all of the recruitment and return measurements I chose to use the model
  #in MARK that had the lowest AIC scores and didn't let Phi change over time
  
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
plot(lambda~ASYReturn, data=vrdat)

plot(lambda~SYReturn, data=vrdat)
plot(lambda~layrate, data=vrdat)
plot(lambda~eggRecruitRate, data=vrdat) #Ooooo we have a winner!!
#What within the eggRecruitRate is important? \
plot(lambda~recruitrate, data=vrdat) #There's so little variation in the recruitrate that it's not as important!
plot(lambda~hatchrate, data=vrdat) #I think this means that you need both hatchrate and fledgerate to be high
plot(lambda~fledgerate, data=vrdat) 



#Now I use a simple linear regression to figure out what the correlation
#coefficients are most sensitive following Taylow et al. 2012
modASYReturn <- lm(lambda~ ASYReturn, data=vrdat)
summary(modASYReturn) #Adjusted R^2 = 0.00105 so quite unimportant

modSYReturn <- lm(lambda~ SYReturn, data=vrdat)
summary(modSYReturn) # Adjusted R^2 = 0.0006235 so even less important

modEggtoAdult <- lm(lambda~eggRecruitRate, data=vrdat)
summary(modEggtoAdult) #Adjusted R^2 = 0.8684 so explains almost all the variation!
#What within this explains variation? 
modClutchSize <- lm(lambda ~ clutchSize, data=vrdat)
summary(modClutchSize) #Adjusted R^2 = 0.00403 so it's not that!

modAverageNests<- lm(lambda ~ averageNests, data=vrdat) 
summary(modAverageNests) #Adjusted R^2 =0.04863, better but not a whole lot of the variation

modHatch <- lm(lambda ~ hatchrate, data=vrdat)
summary(modHatch) # Adjussted R^2 =0.5094 so that's a huge portion of the variation!!!

modFledge <- lm (lambda ~fledgerate, data=vrdat)
summary(modFledge) #Adjusted R^2 =0.3212 so another large portion of the variation, 


#Now I use a log-log transformed regression to figure out what aspects have the
#highest elasticity This is a probelm though because my lambdas are negative and
#I can't log a negative number



#Are there inherent correlations between 
cor(PopData, method = "pearson", use="complete.obs")
res2 <-rcorr(as.matrix(PopData), type = c("pearson"))
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
#None of the parameters correlate significantly with each other so it's safe to not include correlation structure in the vital rates analysis


#I'd really like to include esimates of survival into that. SO for that I guess
#I need a RMARK analysis that estimates a survival probability for each year
#huh? THey're very likely correlated because they are all exposed to the same
#conditions