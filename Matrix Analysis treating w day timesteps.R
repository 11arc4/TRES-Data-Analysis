#Matrix Analysis with Discrete days
library(popdemo)
library(popbio)
#Build the matrix
Stages <- c(paste("Egg", c(1:14)),
            paste("Nestling", c(1:20)),
            paste("Fledge", c(1:187)),
            paste("SY", c(1:365)),
            paste("ASY", c(1:365)))
A <- matrix(0, nrow=length(Stages), ncol=length(Stages), dimnames = list(Stages, Stages))
#Rows are what you are growing into and column are what you are now!

hatchrate <- 0.7398289 #Taken from my Baby Tree Swallow Population Matrix Model script
A["Nestling 1", "Egg 14"]<- hatchrate


fledgerate <- 0.628297 #Taken from my Baby Tree Swallow Population Matrix Model script
A["Fledge 1", "Nestling 20"]<- fledgerate

FRecruitrate <-  0.06226302 #Taken from my Effect of Adult Catch Effort on Recruitment
A["SY 1", "Fledge 187"] <- FRecruitrate #Need to become a SY bird on Jan 1 to match recruits breeding schedules with everyone else's! (This only works if we all breed at the same time)


SYFreturnrate <- 0.1522606 #Taken from my Baby Tree Swallow Population Matrix Model script
A["ASY 1", "SY 365"]<- SYFreturnrate

ASYFreturnrate <-0.27142 #Taken from my Baby Tree Swallow Population Matrix Model script
A["ASY 1", "ASY 365"]<- ASYFreturnrate

SYLayrate <- 5.123011 / 2 #Only half will be female
#SY lay on day 143 on average so we'll put that in later when we break up their eggs and nestlings!
A["Egg 1", "SY 140"]<- SYLayrate


ASYLayrate <- 5.865897 / 2 #Only half will be female
#ASY lay on day 138 on average so we'll use that later#
A["Egg 1", "ASY 140"]<- ASYLayrate

#OK so that's all the important transitional phases but we still need to account
#for what we are concluding is 100% probability of transitioning from egg 1 to
#egg 2 to egg 3 etc. up through egg 13 (egg 14 hatches!)
for (i in 1:13){
  A [ paste("Egg", i+1), paste("Egg", i)] <- 1
    
}
#now lets deal with the nestlings
for (i in 1:19){
  A [ paste("Nestling", i+1), paste("Nestling", i)] <- 1
  
}
#now lets deal with the Fledges,
for(i in 1: 186){
  A [ paste("Fledge", i+1), paste("Fledge", i)] <- 1
  
}

#now lets deal with the SYs and ASY (can all be done at once since the time period is all 1 year)
for (i in 1:364){
  A [ paste("SY", i+1), paste("SY", i)] <- 1
  A [ paste("ASY", i+1), paste("ASY", i)] <- 1
}
is.matrix_ergodic(A) #True good
is.matrix_irreducible(A) #True good!

n0 <- c(rep(0, 14) #0 eggs of any age
        , rep(0, 20), #0 nestlings of any age
        rep(0, 187), #0 fledgelings of any age
        16, rep(0, 364), #16 SY birds on the first day of the year (doesn't matter when that is because we don't let birds die until the last day anyway) and none on any other day
        38, rep(0, 364)) # 38 ASY birds on the first day and none on other days
p <- pop.projection(A=A, n=n0, iterations= 7300)  #To do 20 years we need 20* 365= 7300 iterations

par(mar=c(2.5,2.5,0.5,0.5))
stage.vector.plot(stage.vectors=p$stage.vectors,col=2:4,
                  mgp=c(1.2,0.4,0),cex.axis=0.8,lwd=0.6)
#There are too many stages to make this a useful graph!

lambda<-p$pop.changes # growth rate
time<-c(2:7300) # time
par(mar=c(2.5,2.5,0.5,0.5))
plot(lambda~time,type="l",
     ylab="Population growth",xlab="Years after invasion",
     mgp=c(1.2,0.4,0),cex.axis=0.6,cex.lab=0.8)
#Oh cool you can see the iterations! You can really see that I am having death
#happen all on one day in the winter (Jan 1), and all the breeding happen on 2
#days This averages out to a lambda = 1 because most of the time lambda does
#equal 1 but that's not all that's going on... YOu can see that that is not
#what's happening at all when you look at the population projection! I"m still
#consufed. This effect doesn't actually go away.... hmmmm
p2<-project(A=A,vector=n0,time=900)
#Here I get a warning that my matrix is imprimitive. I am somewhat unsure
#whether this is a problem or not. It is a result of having these very LONG
#loops as you run through the whole year
par(mar=c(2.5,2.5,1,1))
plot(p2,mgp=c(1.2,0.4,0),cex.axis=0.8,lwd=0.6)


eigA<-eigen.analysis(A)

#These proportions make sense!
PropEggStable <- sum(eigA$stable.stage[1:14])
PropNestlingStable <- sum(eigA$stable.stage[15:34]) #0.05095036
PropFledgelingStable <- sum(eigA$stable.stage[35:221]) #0.4089243
PropSYStable <- sum(eigA$stable.stage[222: 587]) #0.1647359
PropASYStable <- sum(eigA$stable.stage[588: 951]) #0.3295029

#Let's take a look at the important sensitivities!
SenstoHatch <- eigA$sensitivities["Nestling 1", "Egg 14"] #0.03677901
SenstoFledge <- eigA$sensitivities["Fledge 1", "Nestling 20"] #0.1364412
SensetoRecruit <- eigA$sensitivities["SY 1", "Fledge 187"] #0.00730766
SenstoSYReturn <- eigA$sensitivities["ASY 1", "SY 365"] #0.002989756
SenstoASYReturn <- eigA$sensitivities["ASY 1", "ASY 365"] #0.006007899
SenstoASYlay <- eigA$sensitivities["Egg 1", "ASY 140"] #0.009057042
SenstoSYlay <- eigA$sensitivities["Egg 1", "SY 140"] #0.004507123
#This sensitivity analysis suggest that it's actually Fledgerate that is what is
#most important-- if you look at the population graph that really makes a lot of
#sense-- fledging time is when the population drops the most

ElastoHatch <- eigA$elasticities["Nestling 1", "Egg 14"] #0.02728915
ElastoFledge <- eigA$elasticities["Fledge 1", "Nestling 20"] #0.08597442
ElastoRecruit <- eigA$elasticities["SY 1", "Fledge 187"] #0.0006549634
ElastoSYReturn <- eigA$elasticities["ASY 1", "SY 365"] #0.0004565433
ElastoASYReturn <- eigA$elasticities["ASY 1", "ASY 365"] #0.001635397
ElastoASYlay <- eigA$elasticities["Egg 1", "ASY 140"] #0.02664094
ElastoSYlay <- eigA$elasticities["Egg 1", "SY 140"] #0.01157853
#A 1% change in fledge rate will have a 8.5% change in growth rate--therefore this is the most imp. transition


#lets try to see what happens if we vary the fledging rate
#This isn't working properly! I'm not sure why that is? 
A2 <- A
fledgerateoptions <- seq(from=0, to=1, by=0.05)
lam<- c()
for (i in 1:length(fledgerateoptions)){
  A2["Fledge 1", "Nestling 20"]<-fledgerateoptions[i]
  p2<-pop.projection(A2,n0,900)
  p3<-project(A=A2,vector=n0,time=900)
  par(mar=c(2.5,2.5,1,1))
  plot(p3,mgp=c(1.2,0.4,0),cex.axis=0.8,lwd=0.6)
  
  lam[i]<-p2$lambda
  }
lam

