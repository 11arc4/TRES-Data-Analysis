#Packages Needed
library(lubridate)
library(lme4)
library(nlme)
library(car)
library(MuMIn)
library(glmmADMB)
library(dplyr)
#We need to a better analysis of adult mass using all the data. This means GLMM
#because I have multiple measurements of the same individual each year. 

#The data we are loading in is all the adult body measurements, and includes the
#data for first nest the bird had that year


inputdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis"
adult <- read.csv(paste(inputdir, "Adult Measurements with First nest data.csv", sep="/"), as.is = T, na.strings=c("", "NA"))
names(adult)
str(adult)
adult$band<- as.factor(adult$band)
adult$sex [which(adult$sex=="6" | adult$sex=="7")] <- "U"
adult$sex [which(adult$sex=="F ")] <- "F"
adult$sex<- as.factor(adult$sex)
adult$dateMeas <- as.Date(adult$dateMeas, format= "%m/%d/%Y")
adult$dateMeas2 <- yday(adult$dateMeas)
adult$year2 <- adult$year -1974 #rescales eveyrone so analysis works better
adult$age <- as.factor(adult$age) 
#I"m not sure if there's a better way to deal with this. There might be. Age is 
#not REALLY a factor because it has an order to it, but it's not numeric either 
#because their age estimates... I also need to remove all the birds that were
#polygynous because I still haven't really figured out a good way to deal with
#them in the dataset....
adult <- adult[which(adult$age!= "ASY/ASY" & adult$age != "AHY/AHY" & adult$age != "SY + ASY"),]

adult$hatchdate[which(adult$hatchdate==0)] <- NA
adult$mass[which(adult$mass<15 | adult$mass>30 )] <- NA

 

#Data exploration is the first thing I need to do
#I am following the procedures laid out in Zuur "A Beginner's guide to GLM" 
#They have some great freely available code that I'm using and have attached.
#Run that file before you try to run this code so you have all the right functions!
#Step 1: Check for outliers
MyVar <- c("year2", "dateMeas2", "mass", "clutchsize", "sex", "hatchdate", "age" )
Mydotplot(adult[,MyVar])

#sex is all good as long as I clear up everyont to be either male, female or unknown

#hatch date has some wonky outliers where hatch date is set to 0. Let's fix that because that's wrong
#same thing happened in the mass


#There are a couple of clutch sizes of 12 from experimental maniupulation. Let's get rid of those for the analysis


adult2 <- adult[which(adult$clutchsize<9),]

adult2$diff <- adult2$dateMeas2-adult2$hatchdate #THis will give positive values close if measured after hatching and negative before
MyVar2 <- c("year2", "mass", "clutchsize", "sex", "diff", "age" )



Mydotplot(adult2[,MyVar2])
#That looks a lot better but diff is still an issue. I will remove all the points where the diff is HUGe
adult3 <- adult2[which(adult2$diff<50 & adult2$diff >-90),]

plot(adult3$diff, adult3$year2)
Mydotplot(adult3[,MyVar2])
#OK that's finally WAYYYY better although I lost a lot of points


#Step 2: Check for colinearity
corvif(adult3[,MyVar])
#Yay! No one has variance inflation factor (VIF) greater than even 1.25 so
#colinearity is not going to be an issue

#Step 3: Check for relationships
Myxyplot(adult3, MyVar, "mass")
plot(adult3$mass~adult3$year2)
plot(adult3$mass~adult3$dateMeas2)
plot(adult3$mass~adult3$clutchsize) #variance will be an issue
plot(adult3$mass~adult3$sex)
plot(adult3$mass~adult3$hatchdate) #variance will be an issue
plot(adult3$mass~adult3$age)

#OK now we are ready to start the modeling

#I will start with the basic linear mixed model and build up from there
#I will include bird band as a random intercept
#Let me first just subset the data so I have only nice stuff without NAs
adult4 <- adult3[which(!is.na(adult3$clutchsize) &
                         !is.na(adult3$sex) &
                         !is.na(adult3$year2) &
                         !is.na(adult3$diff) &
                         !is.na(adult3$mass) &
                         !is.na(adult3$age)
                 ),]
                            
adult4$diff2 <- scale(adult4$diff)
#I'd like to add age to this model as well but I'm not sure how best to do this-- right now it's getting dropped
mod1 <- lmer(mass~ year2*clutchsize*diff*sex + (1 | band), data=adult4)
summary(mod1)
#the random intercept of band explains 1.9 variance (more than residual variance of 0.87 so I'm keeping it!)



Anova(mod1) #Cool. All sorts of things are significant



hist(resid(mod1)) #nice!
plot(resid(mod1)~ adult4$year2) 
plot(resid(mod1)~adult4$sex) #this is fine
plot(resid(mod1)~ adult4$clutchsize) #variance issues
plot(resid(mod1)~adult4$diff) #Data is clumpy but maybe not really so bad?

#There were errors about scaling variables-- why don't I try to fix that by using the scaled diff
mod2 <- lmer(mass~ year2*clutchsize*diff2*sex+ (1 | band), data=adult4)
summary(mod2)
plot(mod2)

#Take things out until the trends go away
#looks like it's the random effect that is causing this problem
hist(resid(mod2)) #nice!
plot(resid(mod2)~ adult4$year2) #good enough
plot(resid(mod2)~adult4$sex) #this is fine
plot(resid(mod2)~ adult4$clutchsize) #again, variance issues but that's probably oK because it's just due to having less data
plot(resid(mod2)~adult4$diff2) #Data is clumpy and looks like it might be trending upward very slightly  but it's actually totally ok

mod3 <- lm(mass~ year2*clutchsize*diff2*sex, data=adult4)
options(na.action="na.fail")
allmodels <- dredge(mod3, extra="R^2", beta="sd") #this will give you R^2
topmodels <- get.models(allmodels, subset=delta<3)

Anova(mod3)
plot(mod3)

#Better model would probably not include the four way interactions
mod4 <- lm(mass~ year2+clutchsize+diff2+sex+ #all the main effects
             year2:clutchsize + year2:diff2 + year2:sex + clutchsize:diff2 + clutchsize:sex + diff2:sex+ #2 way interactions
             year2:clutchsize:diff2 + year2:clutchsize:sex + year2:sex:diff2 + clutchsize:diff2:sex, data=adult4)
plot(mod4)
hist(resid(mod4, type="pearson"))
#check assumptions again on the top model
#Check up on whether you're meeting assumptions of likelihood (what are the assumptions of likelihood)
#likelihood assumes probablility distribution but need to double checck on that. 


#add small random error to see if the argyle pattern dissapates. 

#pick a random measurement of the bird from the list of measurements
#redo this over and over and over to get a good estimate of p values


