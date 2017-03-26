#Matrix Analysis Smearing reproduction using De Steven's Return Rates



hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
FRecruitrate <-  0.06226302 #Taken from my Effect of Adult Catch Effort on Recruitment

SYFreturnrate <- 0.53 #De Steven
ASYFreturnrate <-0.75 #De Steven
SYLayrate <- 5.123011 / 2 #Only half will be female
ASYLayrate <- 5.865897 / 2 #Only half will be female


eggtonestlingstime <- 14.17244
nestlingstofledgelingtime <- 19.6886
meanwaitasnestling <- nestlingstofledgelingtime/eggtonestlingstime #1.389217
pstaynestling <- 1-  1/meanwaitasnestling #0.2801702


meanwaitFledgeandAdult <- 365/eggtonestlingstime #25.84815
pstayfledgeandadult <- 1- 1/meanwaitFledgeandAdult #0.9613125


ASYreturnholdcombined <- 1-ASYFreturnrate/meanwaitFledgeandAdult #0.9708785
stages <- c("egg", "nestling", "fledgelings", "SY", "ASY")
A <- matrix(0, nrow=5, ncol=5, dimnames = list(stages, stages))
#Row=what you're growing into
#Column=what you are currently
A[2, 1]<- hatchrate
A[2, 2]<- pstaynestling
A[3,2] <- fledgerate
A[3, 3] <- pstayfledgeandadult
A[4,3] <- FRecruitrate
A[1,4] <- SYLayrate /(365/eggtonestlingstime)
A[4,4] <- pstayfledgeandadult
A[5,4] <- SYFreturnrate/(365/eggtonestlingstime) #THIS IS WRONG SOMEHOW I"M NOT SURE WHAT SHOULD BE HEre But Not thiS
A[1,5] <- ASYLayrate/ (365/eggtonestlingstime) #This smears reproduction throughout the year 
A[5, 5] <-ASYreturnholdcombined
A


#That looks right :)
is.matrix_irreducible(A)
is.matrix_ergodic(A)

#N0 is the initial population size
N0 <- c(0, 0,0, 16, 38)
p <- pop.projection(A=A, n=N0, iterations=100)
p

# lambda = 1.032696 when we use return rates from De Steven
stage.vector.plot(stage.vectors = p$stage.vectors) 

