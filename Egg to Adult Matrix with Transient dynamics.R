#Egg to Adult Matrix Model
library(popbio)
library(popdemo)
hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
FRecruitrate <-  0.06226302 #Taken from my Effect of Adult Catch Effort on Recruitment


eggRecruitRate <- hatchrate * fledgerate * FRecruitrate



FReturnrate <- 0.2344073
Flayrate <- 5.623868
FReturn <- 0.3582621 
#This is from the RMark model where Female return depends only on the year being
#recaptured , it is constant across all years so by default has been averaged. I
#know that's not the best model though!

stages <- c("Egg", "Adult")
A<- matrix(0, nrow=2, ncol=2, dimnames = list(stages, stages))
A[2,1] <- eggRecruitRate
A[1,2] <- Flayrate
A[2,2] <- FReturn
A
is.matrix_irreducible(A)
is.matrix_ergodic(A)
is.matrix_primitive(A)
N0 <- c(0, 54)
p <- pop.projection(A=A, n=N0, iterations=42)
stage.vector.plot(stage.vectors = p$stage.vectors)
lambda <- p$pop.changes
time<- c(2:42)
plot(lambda~time, ylab="Population growth", xlab="Year", type="l")
size <- p$pop.sizes
time2 <- c(1:42)
plot(size~time2, type="l")

#Population density
p2 <- project(A=A, vector=N0, time=42)
par(type="b")
plot(p2) #Same deal as before where I don't actually get a population growing!

p3 <- project(A=A, vector="n", time=42)
par(type="b")
plot(p3) #if we set the bias toward adults, vs toward eggs

#Transient dynamics might be able to explain what's going on better than the overall pattern (?)
firststepatt(A, vector=N0) #tells us that we amplify not attenuate so that we should've used reactivity. 
#This means that adults are over represented initially which totally makes sense
reactivity(A, vector = N0) #reactivity =9.64 so the population is amplifying in the first time step
maxamp(A, vector=N0) #the largest amplification is at the first time step (9.640002)
inertia(A, vector= N0)
#7.073078 so the populaiton should stabilize at 7x higher than expected. 
inertia(A, vector= "n", bound="upper") #if the initial state was all adults
inertia(A, vector= "n", bound="lower") #if the initial state was all eggs

