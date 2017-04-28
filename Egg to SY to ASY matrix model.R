#Matrix Model based on a yearly timescale
library(popdemo)
library(popbio)

#egg--> SY--> Adult
hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
Recruitrate <-  0.1494102 #Taken from my RMark analysis of the nesltings



eggRecruitRate <- hatchrate * fledgerate * Recruitrate

#I have these super low return rates for my data but De Steven et al. calculated 0.579 return rates for adults
SYFreturnrate <- 0.2941571 #Taken from my RMark analysis of the female adults
ASYFreturnrate <- 0.4599775 #Taken from my RMark analysis of the female adults
SYLayrate <- 5.123011 / 2 #Only half will be female
ASYLayrate <- 5.865897 / 2 #Only half will be female

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))

#Rows= what stage the birds are in now
#Columns=what stage the birds are going to
A[1, 2] <- SYLayrate 
A[1, 3] <- ASYLayrate 
A[2, 1] <- eggRecruitRate
#May also want to try this with De Stevens return rates (0.75% of SY and 0.533 of ASY) #De Steven 1980
A[3, 2] <- SYFreturnrate
A[3, 3] <- ASYFreturnrate
A
#That looks right :)
is.matrix_irreducible(A)
is.matrix_ergodic(A)

#N0 is the initial population size
N0 <- c(0, 16, 38)

#project the population for 42 years (course of our study system)
p <- pop.projection(A=A, n=N0, iterations=42)
p

#lambda is 0.6753534! That's low. 


#lets see what age structure the population stableizes at
stage.vector.plot(stage.vectors = p$stage.vectors) #HUh well that is still not really what should be going on....

#How does the population change?
lambda <- p$pop.changes
time<- c(2:42)
par(type="b")
plot(lambda~time, ylab="Population growth", xlab="Year")

#Population Size 
size <- p$pop.sizes
time2 <- c(1:42)
plot(size~time2)

#Population density
p2 <- project(A=A, vector=N0, time=42)
par(type="b")
plot(p2) #Same deal as before where I don't actually get a population growing!











##############################################################################################################################
#What happens if we design the matrix to take into account that eggs from a SY
#nest are not the same as eggs from a ASY nest?

#I know hatch and fledgeing rates are slightly different for SY and ASY female's nests
hatchrateSY <- 0.7660019#Taken from my Baby Tree Swallow Population Matrix Model script
fledgerateSY <- 0.5870798  #Taken from my Baby Tree Swallow Population Matrix Model script
hatchrateASY <- 0.7913602 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerateASY <- 0.6239397 #Taken from my Baby Tree Swallow Population Matrix Model script
#From that we can calculate egg recruitment rates that take that into account!
eggRecruitRateSY<- hatchrateSY* fledgerateSY * Recruitrate
eggRecruitRateASY<- hatchrateASY* fledgerateASY * Recruitrate #Slightly higher for the ASY females


stages <- c("eggSY", "eggASY", "SY", "ASY")
A <- matrix(0, nrow=4, ncol=4, dimnames = list(stages, stages))

#Rows= what stage the birds are in now
#Columns=what stage the birds are going to
A[3,1]<- eggRecruitRateSY
A[3,2]<- eggRecruitRateASY
A[1, 3]<- SYLayrate
A[2,4] <- ASYLayrate
A[4, 3]<- SYFreturnrate
A[4,4]<- ASYFreturnrate
A
#Of course we have the same N0 as before
is.matrix_irreducible(A)
is.matrix_ergodic(A)
#Nice still all good
N0 <- c(0,0,16,38)
#project the population for 42 years (course of our study system)
p <- pop.projection(A=A, n=N0, iterations=42)
p


#lets see what age structure the population stableizes at
stage.vector.plot(stage.vectors = p$stage.vectors) #HUh well that is still not really what should be going on....



#The population growth rate is 0.68-- the population will decline over time
p$lambda

#Stage stage distribtuion is 31% SY eggs, 39% ASY Eggs, 8% SY and 11% ASY
p$stable.stage

#Reproductive value is very high for an adult ASY bird, and slightly lower for a
#SY bird. Eggs of both types have low reproductive value although ASY eggs are
#slightly more valueable
reproductive.value(A)



#Now lets look at the transient dynamics
#Adults are over represented in the initial population so 
reactivity(A, N0) #The population amplified because reactivity =4.758148
maxamp(A) #If we had ALLL ASY adults the first year then we would have had reactivity at 4.99
inertia(A, N0) #The population settles to a density 3.97X  that predicted by stable growth

#Now we need to see how perturbations of the different stages will ingluence the inertia
tfamat<- tfamatrix(A)
plot(tfamat)
#increasing any of the parameters with increase lambda in all cases. However increasing ASY return will increase it fastest 

inertiatfamat <- inertia.tfamatrix(A, vector=N0)
plot(inertiatfamat)
#inertia will increase if we increase perterbation ASY layrate. Inertia decreases if eggs of
#either type transition to SY at higher rates, or SY transition to ASY at higher
#rates. There is a optimal value for ASY retrun perturbation to maintain peak inertia.




#OK time to to the long term analysis of perterbation
eigA <- eigen.analysis(A)
eigA$sensitivities
#We are most sensitive to changes in recruitment of ASY Eggs, with a pretty close second in SY eggs. 
#absolute changes
eigA$elasticities
#The most elastic value is the ASY return rates. 
#proportional cahgnes


#OK Well than I think we'd get the most bang for our buck by cahnging recruitment of nestlings from ASY and SY eggs
#lets plot that
ASYeggSur <- seq(0, 1, 0.01)
lam<-c() # vector for lambda's
A2<-A # new projection matrix
### loop to calculate lambda for each
### adult survival
for (i in 1:length(ASYeggSur)){
  A2[3,3]<-ASYeggSur[i]
  p2<-pop.projection(A2,N0,42)
  lam[i]<-p2$lambda
  }
eggRecruitRateASY


plot(lam~ASYeggSur,type="l", ylab="Population growth rate",xlab="ASY egg survival",
     mgp=c(1,0.4,0))
abline(h=1,col="red") # horizontal line at lambda = 1
#Can't get to a stable population like this!
