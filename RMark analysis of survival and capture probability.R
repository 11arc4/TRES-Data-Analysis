#RMark analysis of birds

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
datMark <- data.frame(dummy$Ch, dummy$ageAtFirstSight, dummy$sex)
colnames(datMark) <- c("ch", "AgeAtFirstSight", "Sex")
datMark$ch <- as.character(datMark$ch)

datMark$FirstSighting[which(datMark$AgeAtFirstSight=="HY")] <- "Nestling"
datMark$FirstSighting[which(datMark$AgeAtFirstSight!="HY")] <- "Adult"
datMark$FirstSighting <- as.factor(datMark$AgeAtFirstSight)

datMark$Sex[which(datMark$Sex==7)] <- "U"

#OK let's start doing this!
library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975 as we should We're going to want a CJS
#model because our data is recapture data, and we can't tell whether you've
#actually died. (Although ultimately I may want to incorporate information about
#the birds that were killed for experiments and therefore will want to use a
#different type of model for that)
tsprocess <- process.data(datMark,model="CJS",begin.time=1975, groups=c("AgeAtFirstSight", "Sex", "FirstSighting"))
#Now I need to make the data in the right format for a CJS model
ts.ddl<- make.design.data(tsprocess)

#Now there are a number of different models that I would really like to run

#I will want to include Time (continuous), Age (discrete but based on the number
#of times we've captured the bird), AgeAtFirstSight, Sex as those factors are
#what might be influencing both capture success and survival.
#I'm going to simplify this even further so that you are either an adult or a neslting at first sighting. 

#Capture probabilities will ALWAY depend on whther you were a nestling first or not. 
p.dot <- list(formula= ~FirstSighting)
p.sex <- list(formula=~Sex*FirstSighting)
p.Age <- list(formula=~Age*FirstSighting)
p.Time <- list(formula=~Time*FirstSighting)
p.sex.Age <- list(formula=~Sex*Age*FirstSighting)
p.sex.Time <- list(formula=~Sex*Time*FirstSighting)
p.Age.Time <- list(formula=~Age*Time*FirstSighting)
p.sex.Age.Time <- list(formula=~Sex*Age*Time*FirstSighting)

#Survival probabilities will also ALWAY depend on whther you were a nestling first or not. 

phi.dot <- list(formula= ~FirstSighting)
phi.sex <- list(formula=~Sex*FirstSighting)
phi.Age <- list(formula=~Age*FirstSighting)
phi.Time <- list(formula=~Time*FirstSighting)
phi.sex.Age <- list(formula=~Sex*Age*FirstSighting)
phi.sex.Time <- list(formula=~Sex*Time*FirstSighting)
phi.Age.Time <- list(formula=~Age*Time*FirstSighting)
phi.sex.Age.Time <- list(formula=~Sex*Age*Time*FirstSighting)





#Let's build all our possible options for models, and then compare them all!

ts1 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.dot ), output=F)
ts2 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.dot ), output=F)
ts3 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.dot ), output=F)
ts4 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.dot ), output=F)
ts5 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.dot ), output=F)
ts64 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.dot ), output=F)
ts6 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.dot ), output=F)
ts7 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.dot ), output=F)

ts8 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.sex ), output=F)
ts9 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.sex ), output=F)
ts10 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.sex ), output=F)
ts11<-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.sex ), output=F)
ts12 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.sex ), output=F)
ts13 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.sex ), output=F)
ts14 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.sex ), output=F)
ts15 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.sex ), output=F)

ts16 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.Age ), output=F)
ts17 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.Age ), output=F)
ts18 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.Age ), output=F)
ts19 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.Age ), output=F)
ts20 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.Age ), output=F)
ts21 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.Age ), output=F)
ts22 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.Age ), output=F)
ts23 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.Age ), output=F)

ts24 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.Time ), output=F)
ts25 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.Time ), output=F)
ts26 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.Time ), output=F)
ts27 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.Time ), output=F)
ts28 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.Time ), output=F)
ts29 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.Time ), output=F)
ts30 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.Time ), output=F)
ts31 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.Time ), output=F)

ts32 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.sex.Age ), output=F)
ts33 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.sex.Age ), output=F)
ts34 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.sex.Age ), output=F)
ts35 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.sex.Age ), output=F)
ts36 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.sex.Age ), output=F)
ts37 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.sex.Age ), output=F)
ts38 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.sex.Age ), output=F)
ts39 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.sex.Age ), output=F)

ts40 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.sex.Time ), output=F)
ts41 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.sex.Time ), output=F)
ts32 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.sex.Time ), output=F)
ts43 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.sex.Time ), output=F)
ts44 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.sex.Time ), output=F)
ts45 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.sex.Time ), output=F)
ts46 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.sex.Time ), output=F)
ts47 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.sex.Time ), output=F)

ts48 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.Age.Time ), output=F)
ts49 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.Age.Time ), output=F)
ts50 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.Age.Time ), output=F)
ts51 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.Age.Time ), output=F)
ts52 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.Age.Time ), output=F)
ts53 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.Age.Time ), output=F)
ts54 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.Age.Time ), output=F)
ts55 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.Age.Time ), output=F)

ts56 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.dot, p=p.sex.Age.Time ), output=F)
ts57 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex, p=p.sex.Age.Time ), output=F)
ts58 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age, p=p.sex.Age.Time ), output=F)
ts59 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Time, p=p.sex.Age.Time ), output=F)
ts60 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age, p=p.sex.Age.Time ), output=F)
ts61 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Time, p=p.sex.Age.Time ), output=F)
ts62 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.Age.Time, p=p.sex.Age.Time ), output=F)
ts63 <-  mark(tsprocess, ts.ddl, model.parameters=list(Phi=phi.sex.Age.Time, p=p.sex.Age.Time ), output=F)
#Use this to compare models For many of these models MARK underestimates the
#parameters so there is disagreement between the number of parameters RMark and
#Mark are giving. I am going with the more conservative RMark parameter count
#but should probably check to make sure there aren't just boundary meeting
#parameters which don't get counted....
ts.cjs.results <- collect.models()
#Now we should do model averaging on the models
gc() #This is for garbage collection, to hopefully get me some more space!
modelaverage <- model.average(ts.cjs.results)
#Jesus christ it's a 1.8GB vector.... that's too big....
