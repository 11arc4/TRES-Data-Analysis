#RMark analysis of birds
setwd("~/Masters Thesis Project/TRES Data Analysis/RMark Preliminary Survival Analysis")
#Now all the stupid mark files will get put in their own folder away from
#everything important.
library(dplyr)
library(tidyr)
#Want to take out capture history, age at first capture and sex
years<- (seq(from= 1975, to = 2016, by=1))
dummy <- data.frame(matrix(ncol = length(years), nrow = length(as.list(globalData$birds))))
colnames(dummy) <- years
dummy$sex <- c()
dummy$ageAtFirstSight <- c()

#For each bird, we need to make a row of their capture history 
#This takes a while so make sure you're all set and ready to look at stuff
b <- 0
for (bird in as.list(globalData$birds)){
  b <- b+1
  y<- 0
  #We'll also set their sex because sex might change survival and I know it
  #changes capture probabilities (males were caught less)
  dummy$sex[b] <- bird$sex
  for (year in bird$yearsSeen$as.list()){
    y=y+1 #years are sorted so if y=1 then it's the first time we saw the bird
    if(y==1){
      #Age when we first saw you might also change your survival
      dummy$ageAtFirstSight[b] <- year$age
    }
    
    #Put a 1 in every year where the bird was seen 
    dummy[b, which(years==year$year)] <- 1
  }
  #Fill in all the empty spaces with 0s
  dummy[b, which(is.na(dummy[b,]))] <- 0
}


#Now let's make this dummy dataset into a data set that's in the right format for RMark. 

dummy$Ch <- apply(dummy[,1:42], 1, paste, collapse="")
dummy$Sex[which(datMark$Sex==7)] <- "U"


datMark <- data.frame(dummy$Ch, dummy$ageAtFirstSight, dummy$sex)
colnames(datMark) <- c("ch", "AgeAtFirstSight", "Sex")
datMark$FirstSighting[which(datMark$AgeAtFirstSight=="HY")] <- "Nestling"
datMark$FirstSighting[which(datMark$AgeAtFirstSight!="HY")] <- "Adult"
datMark$FirstSighting <- as.factor(datMark$FirstSighting)
datMark$ch <- as.character(datMark$ch)


#I want a data set that is JUST adult bird sightings. I need to remove the first
#sighting of birds that were caught as nestlings, and if they have no other
#sighting they are removed from the dataset
adummy <- dummy %>% filter(ageAtFirstSight!="HY")

ndummy <- dummy %>% filter(ageAtFirstSight=="HY")

for (i in 1:nrow(ndummy)){
  ndummy[i, which(ndummy[i,]==1)[1]]<- 0
  ndummy$sightings[i] <- sum(ndummy[i,1:42])
}

ndummy2 <- ndummy %>% filter(sightings>0)
ndummy2 <- ndummy2[, 1:(ncol(ndummy2)-1)] #remove the sightings column

dummy_adult <- rbind(ndummy2, adummy)

#Now we just have to put the adut data into the right Mark format and we're good to go
dummy_adult$Ch <- apply(dummy_adult[,1:42], 1, paste, collapse="")
dummy_adult$sex[which(dummy_adult$sex==7)] <- "U"
datMark_adult <- data.frame(dummy_adult$Ch, dummy_adult$ageAtFirstSight, dummy_adult$sex)
colnames(datMark_adult) <- c("ch", "AgeAtFirstSight", "Sex")
#Since we removed all the times we saw nestlings, I will put those birds in as SY at first sighting instead
datMark_adult$AgeAtFirstSight[which(datMark_adult$AgeAtFirstSight=="HY")] <- "SY"
datMark_adult$ch <- as.character(datMark_adult$ch)
datMark_adult$Sex[which(datMark_adult$Sex=="F ")] <- "F"
datMark_adult$Sex[which(datMark_adult$Sex==6)] <- "U"

#Since we only need survival for female birds to put into the matrix, lets do that....

datMark_F<- datMark_adult %>% filter(Sex=="F")

datMark_F$age[which(datMark_F$AgeAtFirstSight=="ASY")]<- 2
datMark_F$age[which(datMark_F$AgeAtFirstSight!="ASY")]<- 1

#OK let's start doing this!
library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975 as we should We're going to want a CJS
#model because our data is recapture data, and we can't tell whether you've
#actually died. (Although ultimately I may want to incorporate information about
#the birds that were killed for experiments and therefore will want to use a
#different type of model for that)
tsprocess <- process.data(datMark_F,model="CJS",begin.time=1975, groups= ("age"), initial.ages =c(1, 2))
#age vector is the initial ages, "initial.ages" assigns a dummy variable to
#count up from for each of those. So I could use AHY ASY and SY and assign c(1,
#2, 1) instead (it's done alphabetically) That condensed the data into that form
#where you have the different numbers of times you've seen a particular capture
#probability. Now I need to make the data in the right format for a CJS model
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(1, 2)),
                                                     p=list(age.bins=c(1,2)))) 
#may ultimately want to set age.bins but I don't really understand how to use
#that feature yet

#Now there are a number of different models that I would really like to run

#I will want to include Time (continuous), Age (discrete but based on the number
#of times we've captured the bird),Sex as those factors are
#what might be influencing both capture success and survival. Also make AgeAtFirstSight ()
#I'm going to simplify this even further so that you are either an adult or a neslting at first sighting. 

#I KNOW that we caught different numbers of birds each year so time should go in all the capture probabilities.
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)

#Survival probabilities might also vary by time but now they don't HAVE to, and that time function is continuous (Time not time)

phi.dot <- list(formula= ~1) #if survival is constant in all cases
phi.Time <- list(formula=~Time) #if survival rates change over the years
phi.Age <- list(formula=~Age) #If older birds (based on having seen them previously, not their known age) have lower survival
phi.Age.Time <- list(formula=~Time*Age) #if both age (based on sight history) have an effecct
phi.Age.plus.Time <- list(formula=~Time+Age)
#I'll pick the best one of these and then see if interactions are of any importance. 


#Let's build all our possible options for models, and then compare them all!


ts1 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.dot, p=p.dot), output=F, adjust=F)
ts2 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Time, p=p.dot), output=F, adjust=F)
ts3 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age, p=p.dot), output=F, adjust=F) 
ts4 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.Time, p=p.dot), output=F, adjust=F)
ts5 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.Time, p=p.dot), output = F, adjust=F)

ts6 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.dot, p=p.time), output=F, adjust=F)
#this is the model where all adult birds have constant survival across years, but time has different capture pobabilities
#phi = 0.3582621
ts7 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Time, p=p.time), output=F, adjust=F)
ts8 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age, p=p.time), output=F, adjust=F) 
#Based on the model that seperates ASY and SY return but averages over the years (second best model )
#logit link so we have to take exp
SYreturn8 <- exp(-1.1886984)
ASYreturn8 <- exp(-1.1886984+0.2324945)


ts9 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.Time, p=p.time), output=F, adjust=F)
ts10 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.Time, p=p.time), output = F, adjust=F)


#Use this to compare models For many of these models MARK underestimates the
#parameters so there is disagreement between the number of parameters RMark and
#Mark are giving. I am going with the more conservative RMark parameter count
#but should probably check to make sure there aren't just boundary meeting
#parameters which don't get counted....
ts.cjs.results <- collect.models()
#Now we should do model averaging on the models
modelaverage <- model.average(ts.cjs.results)

remove.mark(ts.cjs.results)
##################
#Now lets do an analysis of the nestlings

ndummy <- dummy %>% filter(ageAtFirstSight=="HY")

ndummy$Ch <- apply(ndummy[,1:42], 1, paste, collapse="")
datMark_nestling <- data.frame(ndummy$Ch, ndummy$ageAtFirstSight,  ndummy$sex)
colnames(datMark_nestling) <- c("ch", "age", "sex")
#Since we removed all the times we saw nestlings, I will put those birds in as SY at first sighting instead
datMark_nestling$ch <- as.character(datMark_nestling$ch)


nestlingprocess <- process.data(datMark_nestling,model="CJS",begin.time=1975, groups= c("age", "sex"), initial.ages =c(0))
#initial age for all the hatchlings is 0 because here I am only looking at those birds we first saw as nestlings
nestling.ddl<- make.design.data(nestlingprocess, parameters=list(Phi=list(age.bins=c(0,1, 2, 42)),
                                                     p=list(age.bins=c(0,1,2,42)))) 
#again we are binning the ages into HY, SY and ASY (AHY will be assigned 1 just like SY) ie age 0, 1, and 2-13


#Now we need to make the capture functions (p). Capture will also depend on sex
#but I'm a bit unsure how to take that into account since I don't know the sex
#for so many of the birds..... it's also super biased because I only know the sex for birds that survived.....
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)

#Now lets make the survival functions (phi)
phi.dot <- list(formula= ~1) #if survival is constant in all cases
phi.Time <- list(formula=~Time) #if survival rates change over the years
phi.age <- list(formula=~age) #If older birds (based on having seen them previously, not their known age) have lower survival
phi.age.Time <- list(formula=~Time*age) #if both age (based on sight history) have an effecct
phi.age.plus.Time <- list(formula=~Time+age)


n1 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.dot, p=p.time), output = F, adjust=F)
n2 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.Time, p=p.time), output = F, adjust=F)
n3 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age, p=p.time), output = F, adjust=F)
#this is the best model that doesn't include a time variable survival

n4 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.Time, p=p.time), output = F, adjust=F)
n5 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.Time, p=p.time), output = F, adjust=F)

n6 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.dot, p=p.dot), output = F, adjust=F)
n7 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.Time, p=p.dot), output = F, adjust=F)
n8 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age, p=p.dot), output = F, adjust=F)
n9 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.Time, p=p.dot), output = F, adjust=F)
n10 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.Time, p=p.dot), output = F, adjust=F)


nestling.cjs.results <- collect.models()
