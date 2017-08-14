#RMark analysis of birds
setwd("~/Masters Thesis Project/TRES Data Analysis/RMark Preliminary Survival Analysis")
#Make sure you set the working directory to something seperarate otherwise MARK
#will make 1000 files that you'll want to get rid of later.

datMark_F <- readRDS("Adult Female MARK Data.rda") 
#this data is only for females. All males have been removed as have observations
#of nestlings (if the bird was seen as a nestling the nestling observation is
#removed and it shows up first at age 1). I did this because I don't know which
#nestlings are female and which are not if they didn't recruit so I have to
#consider nestling recruitment independent of sex

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've #actually died. (Although ultimately I may want to
# incorporate information about #the birds that were killed for experiments and
# therefore will want to use a #different type of model for that) 
tsprocess <-process.data(datMark_F,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(1, 2))
  
  #age vector is the initial ages, "initial.ages" assigns
# a dummy variable to #count up from for each of those. So I could use AHY ASY
# and SY and assign c(1, #2, 1) instead (it's done alphabetically) That
# condensed the data into that form
#where you have the different numbers of times you've seen a particular capture
#probability.
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(1, 2, 43)),
                                                     p=list(age.bins=c(1,2, 43)))) 

#Settin age bins to look at birds from 1-2 (SY) and then over 2 (ASY)

#Now there are a number of different models that I would really like to run

#I will want to include Time (continuous), Age (discrete but based on the number
#of times we've captured the bird),Sex as those factors are
#what might be influencing both capture success and survival. Also make AgeAtFirstSight ()
#I'm going to simplify this even further so that you are either an adult or a neslting at first sighting. 

#I KNOW that we caught different numbers of birds each year so time should go in
#all the capture probabilities. We might also be more likely to catch a ASY bird
#than a SY birds (that's what the nestling records show) but since SY is the
#first time we'd catch these birds we can't look at that.
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)

#Survival probabilities might also vary by time but now they don't HAVE to, and
#that time function is continuous (Time not time). 

#I will use discrete time (time) in my RMark alysis for our vital rates anlaysis
#though since that will be a better estimator of variation in survival over the
#years

phi.dot <- list(formula= ~1) #if survival is constant in all cases
phi.Time <- list(formula=~Time) #if survival rates change over the years
phi.Age <- list(formula=~age) #If older birds (based on having seen them previously, not their known age) have lower survival
phi.Age.Time <- list(formula=~Time*age) #if both age (based on sight history) have an effecct
phi.Age.plus.Time <- list(formula=~Time+age)
#I'll pick the best one of these and then see if interactions are of any importance. 

#these are the options that I've built specifically for the vital rates analysis
phi.time <- list(formula=~time) #if survival rates change over the years
phi.Age.time <- list(formula=~time*age) #if both age (based on sight history) have an effecct
phi.Age.plus.time <- list(formula=~time+age)

#Let's build all our possible options for models, and then compare them all!
ts1 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.dot, p=p.dot), output=F, adjust=F)
ts2 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Time, p=p.dot), output=F, adjust=F)
ts3 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age, p=p.dot), output=F, adjust=F) 
ts4 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.Time, p=p.dot), output=F, adjust=F)
#second best model includes time age interactions in survival
ts5 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.Time, p=p.dot), output = F, adjust=F)


ts6 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.dot, p=p.time), output=F, adjust=F)
ts7 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Time, p=p.time), output=F, adjust=F)
ts8 <-  mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age, p=p.time), output=F, adjust=F) 
#Based on the model that seperates ASY and SY return but averages over the years-- best model that I will use for the vital rates analysis!
ts9 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.Time, p=p.time), output=F, adjust=F)
ts10 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.Time, p=p.time), output = F, adjust=F)


#Just for the Vital Rates Analayis I need to get yearly estimates of survival!
ts_vitalRates1 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.time, p=p.time), output = F, adjust=F)
ts_vitalRates2 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.plus.time, p=p.dot), output = F, adjust=F)
ts_vitalRates3 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.time, p=p.time), output = F, adjust=F)
ts_vitalRates4 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.Age.time, p=p.dot), output = F, adjust=F)
ts_vitalRates5 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.time, p=p.time), output = F, adjust=F)
ts_vitalRates6 <- mark(tsprocess, ts.ddl, model.parameters = list(Phi=phi.time, p=p.dot), output = F, adjust=F)

#also need to compare these to if we have no parameters, (ts1 and ts5), or if we
#have only age in sugriva, (ts3, and ts8) In that case by far the best most is
#model ts_vitalRates1, so I will pull numbers out of that for my vital rates
#correlation stuff.




#Use this to compare models For many of these models MARK underestimates the
#parameters so there is disagreement between the number of parameters RMark and
#Mark are giving. I am going with the more conservative RMark parameter count
#but should probably check to make sure there aren't just boundary meeting
#parameters which don't get counted....
ts.cjs.results <- collect.models()
#Would you look at that? There's only one best model and it's one of the ones
#that's good for the vital rates analysis! YAY. Let's use that one for the vital
#rates then
saveRDS(ts_vitalRates1, "Best MARK Results for Vital Rates Adults Analysis.rda")

#Now we could do model averaging on the models
modelaverage <- model.average(ts.cjs.results)

remove.mark(ts.cjs.results)




##################
#Now lets do an analysis of the nestlings. I will again bin the ages. 
datMark_nestling<- readRDS("Nestling MARK Data.rda")

nestlingprocess <- process.data(datMark_nestling,model="CJS",begin.time=1975, groups= c("age"), initial.ages =c(0))
#initial age for all the hatchlings is 0 because here I am only looking at those birds we first saw as nestlings
nestling.ddl<- make.design.data(nestlingprocess, parameters=list(Phi=list(age.bins=c(0,1, 2, 43)),
                                                     p=list(age.bins=c(0,1,2,43)))) 
#again we are binning the ages into HY, SY and ASY (AHY will be assigned 1 just like SY) ie age 0, 1, and 2-13


#Now we need to make the capture functions (p). Capture will also depend on sex 
#but I'm a bit unsure how to take that into account since I don't know the sex 
#for so many of the birds..... it's also super biased because I only know the
#sex for birds that survived..... If birds are likely to be kicked off the grid
#when they are SY because they're subordinant, then we might expect age to
#affect capture function as well
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)


phi.dot <- list(formula= ~1) #if survival is constant in all cases
phi.Time <- list(formula=~Time) #if survival rates change over the years
phi.age <- list(formula=~age) #If older birds (based on having seen them previously, not their known age) have lower survival
phi.age.Time <- list(formula=~Time*age) #if both age (based on sight history) have an effecct
phi.age.plus.Time <- list(formula=~Time+age)
#For the vital rates analysis we will want time to not be continuous
phi.time <- list(formula=~time)
phi.age.time <- list(formula=~time*age)
phi.age.plus.time <- list(formula=~time+age)

n1 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.dot, p=p.time), output = F, adjust=T)
n2 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.Time, p=p.time), output = F, adjust=T)
n3 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age, p=p.time), output = F, adjust=T)
n4 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.Time, p=p.time), output = F, adjust=T)
n5 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.Time, p=p.time), output = F, adjust=T)

n6 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.dot, p=p.dot), output = F, adjust=T)
n7 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.Time, p=p.dot), output = F, adjust=T)
n8 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age, p=p.dot), output = F, adjust=T)
n9 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.Time, p=p.dot), output = F, adjust=T)
n10 <- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.Time, p=p.dot), output = F, adjust=T)

n1_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.time, p=p.time), output = F, adjust=T)
n2_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.time, p=p.dot), output = F, adjust=T)

n3_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.time, p=p.time), output = F, adjust=T)
n4_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.time, p=p.dot), output = F, adjust=T)

n5_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.time, p=p.time), output = F, adjust=T)
n6_vr<- mark(nestlingprocess, nestling.ddl, model.parameters = list(Phi=phi.age.plus.time, p=p.dot), output = F, adjust=T)


nestling.cjs.results <- collect.models()
#Here there are multiple top models and model averaging will be more crucial but
#there is only one top model with a discrete time so that's the one we'll use for the vital rates
saveRDS(n4_vr, "Best MARK Results for Vital Rates Nestlings Analysis.rda")
summary(n4_vr)

#I feel VERY uncomfortable using this model though. This isn't by any means the
#best model and the parameterization is shite. I wonder if it might be better to
#use the best model  #################
