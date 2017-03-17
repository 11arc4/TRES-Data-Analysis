#Matrix Model based on a yearly timescale
library(popdemo)
library(popbio)

#egg--> SY--> Adult
hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
FRecruitrate <-  0.08936758 #Taken from my Effect of Adult Catch Effort on Recruitment


eggRecruitRate <- hatchrate * fledgerate * FRecruitrate


SYFreturnrate <- 0.1522606 #Taken from my Baby Tree Swallow Population Matrix Model script
ASYFreturnrate <-0.27142 #Taken from my Baby Tree Swallow Population Matrix Model script
SYLayrate <- 5.123011 / 2 #Only half will be female
ASYLayrate <- 5.865897 / 2 #Only half will be female

stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))

#Rows= what stage the birds are in now
#Columns=what stage the birds are going to
A[1, 2] <- SYLayrate 
A[1, 3] <- ASYLayrate 
A[2, 1] <- eggRecruitRate
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


#lets see what age structure the population stableizes at
stage.vector.plot(stage.vectors = p$stage.vectors) #HUh well that is still not really what should be going on....

#How does the population change?
lambda <- p$pop.changes
time<- c(2:42)
plot(lambda~time, ylab="Population growth", xlab="Year")

#Population Size 
size <- p$pop.sizes
time2 <- c(1:42)
plot(size~time2)

#Population density
p2 <- project(A=A, vector=N0, time=42)
par(type="b")
plot(p2) #Same deal as before where I don't actually get a population growing!



#I know hatch and fledgeing rates are slightly different for SY and ASY female's nests
hatchrateSY <- 0.7660019#Taken from my Baby Tree Swallow Population Matrix Model script
fledgerateSY <- 0.5870798  #Taken from my Baby Tree Swallow Population Matrix Model script
hatchrateASY <- 0.7913602 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerateASY <- 0.6239397 #Taken from my Baby Tree Swallow Population Matrix Model script
#From that we can calculate egg recruitment rates that take that into account!
eggRecruitRateSY<- hatchrateSY* fledgerateSY * FRecruitrate
eggRecruitRateASY<- hatchrateASY* fledgerateASY * FRecruitrate #Slightly higher for the ASY females


#What happens if we design the matrix to take into account that eggs from a SY
#nest are not the same as eggs from a ASY nest?

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
N0 <- (0,0,16,38)
#project the population for 42 years (course of our study system)
p <- pop.projection(A=A, n=N0, iterations=42)
p


#lets see what age structure the population stableizes at
stage.vector.plot(stage.vectors = p$stage.vectors) #HUh well that is still not really what should be going on....

