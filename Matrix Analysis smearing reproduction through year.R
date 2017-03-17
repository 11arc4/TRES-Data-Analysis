#Tree Swallow Population Matrix Model Taking into account the time period of each transition!
library(popdemo)
library(popbio)

hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
FRecruitrate <-  0.08936758 #Taken from my Effect of Adult Catch Effort on Recruitment

SYFreturnrate <- 0.1522606 #Taken from my Baby Tree Swallow Population Matrix Model script
ASYFreturnrate <-0.27142 #Taken from my Baby Tree Swallow Population Matrix Model script
SYLayrate <- 5.123011 / 2 #Only half will be female
ASYLayrate <- 5.865897 / 2 #Only half will be female

#To do the stage structured model that is calibrated to keep everyone in the
#classes for the first amount of time first we need to know about how long the
#transitions take. Obviously Fledge to SY and SY to ASY take 1 year but what
#about egg to nestlings and nestling to fledgeling?
daystohatch <- c()
daystofledge<- c()
dayFirstEgg <- c()
for (nest in as.list(globalData$nests)){
  if(!is.na(nest$firstEggDate)){
    dayFirstEgg[length(dayFirstEgg)+1]<- nest$firstEggDate
  }
  if(!is.na(nest$hatchDate) & !is.na(nest$lastEggDate) & !is.na(nest$hatchSize)){
    
    if(nest$hatchSize>0){
      daystohatch[length(daystohatch) + 1] <- nest$hatchDate - nest$lastEggDate
      
    }
  }
  if (!is.na(nest$fledgeDate) & !is.na(nest$hatchDate) & !is.na(nest$fledgeSize)){
    if(nest$fledgeSize>0){
      daystofledge[length(daystofledge)+1]<- nest$fledgeDate-nest$hatchDate
      
    }
  }
  
  
}
mean(dayFirstEgg) #usually have eggs on day 140



eggtonestlingstime <- mean(daystohatch) #14.17244
nestlingstofledgelingtime <- mean(daystofledge) #19.6886
hist(daystofledge, breaks=100)
#Since egg to nestlings transition time is shortest we will set that time period to be the time period of interest
#Let's calculate probabilities of staying in that holding pattern! Then we can put them into a matrix and see what's going on

#mean wait time = 1/(probability of success)


meanwaitasnestling <- nestlingstofledgelingtime/eggtonestlingstime #1.389217
pstaynestling <- 1-  1/meanwaitasnestling #0.2801702

meanwaitFledgeandAdult <- 365/eggtonestlingstime #25.84815
pstayfledgeandadult <- 1- 1/meanwaitFledgeandAdult #0.9613125


ASYreturnholdcombined <- 1-ASYFreturnrate/meanwaitFledgeandAdult 
#instead of having a 1 chance of making it through you only have part of a change of success!
  


stages <- c("egg", "nestling", "fledgelings", "SY", "ASY")
A <- matrix(0, nrow=5, ncol=5, dimnames = list(stages, stages))

A[2, 1]<- hatchrate
A[2, 2]<- pstaynestling
A[3,2] <- fledgerate
A[3, 3] <- pstayfledgeandadult
A[4,3] <- FRecruitrate
A[1,4] <- SYLayrate
A[4,4] <- pstayfledgeandadult
A[5,4] <- SYFreturnrate/ (365/eggtonestlingstime) #This smears reproduction throughout the year 
A[1,5] <- ASYLayrate/ (365/eggtonestlingstime) #This smears reproduction throughout the year 
A[5, 5] <-ASYreturnholdcombined
A


#That looks right :)
is.matrix_irreducible(A)
is.matrix_ergodic(A)

#N0 is the initial population size
N0 <- c(0, 0,0, 16, 38)

#project the population for 42 years (course of our study system)
p <- pop.projection(A=A, n=N0, iterations=100)
p
stage.vector.plot(stage.vectors = p$stage.vectors) 
#This looks much better than my pervious models. We should still be seeing more returning ASY birds I think

#How does the population change?
lambda <- p$pop.changes
time<- c(2:100)
plot(lambda~time, ylab="Population growth", xlab="Year")

#Population Size 
size <- p$pop.sizes
time2 <- c(1:100)
plot(size~time2, xlim=c(1,100), ylim=c(0, 1000))

#Population density
p2 <- project(A=A, vector=N0, time=42)
par(type="b")
plot(p2) #Same deal as before where I don't actually get a population growing!



