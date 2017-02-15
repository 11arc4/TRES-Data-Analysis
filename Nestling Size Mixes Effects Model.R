#Modeling Chick Mass
library(lme4)
library(glmmADMB)
library(ggplot2)
inputdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis"
ndata <- read.csv(paste(inputdir, "Nestling Measurements for Analysis.csv", sep="/"), as.is=TRUE, na.strings= c("", "NA"))


ndata2 <- ndata[which(ndata$mass <30 & ndata$mass >0), ]  #removed the points which are obvious clerical errors. 
str(ndata2)

plot(mass ~ age* year, data=ndata2) #I will likely have issues because variance in mass increases with age

#Here I've just included different intercepts for the nests and nestlings (that's why it has a 1)
#If I'd wanted different slopes I would have put 2
mod1 <- lmer( mass ~ age * year + (1|nestID) + (1|nestlingID), data=ndata2) 
#get a warning about predictor variables being on different scales. This is
#probably because year is in the 1000s and age is <20
#To deal with this I will rescale my numeric variables as per stack overflow suggestion
#http://stackoverflow.com/questions/26904580/error-messages-when-running-glmer-in-r

pvars <- c("age", "year")
datsc <- ndata2
datsc[pvars] <- lapply(datsc[pvars],scale)
#now my data is all scaled to be on the same scale so this error shouldn't show up



#Data exploration
#Code from Zuur et al. 2013

###1. Check for outliers

MyVar <- c("age", "year")
Mydotplot(datsc[, MyVar])
#OK there aren't any crazy outliers anymore

#### 2. Check for colinearity

corvif (datsc[, MyVar]) 
#Covariance is low for age and year! That makes sense it shoudn't be there at all so yay

#No need to drop anything


#3 Check relationships 
Myxyplot(datsc, MyVar, "mass") 
#Looks like age and mass are maybe logorithmically related and there's a weird
#hump in the middle with year (That's probably just because they only did day 15
#for those years and other years have multiple measurements for the same bird)


#OK now lets make a new model using this better data
mod2 <- glmer( mass ~ age * year + (1|nestID) + (1|nestlingID), data=datsc, na.action="na.omit") 
#and look at that! no error anymore
summary(mod2)
#NestID explains variance, NestlingID doesn't (soooo small), so you can probably
#remove NestlingID as a random effect

mod3 <- glmer( mass ~ age * year + (1|nestID) , data=datsc, na.action="na.omit") 
summary(mod3)

#To get p values we have to do this
Betas <- fixef(mod3)
SE <- sqrt(diag(vcov(mod3)))
pval <- 2*pnorm(-abs(Betas/SE))
output <- cbind (Betas, SE, pval)
print (output, digits=3)  #This shows that everythign is significant (like super significant)

plot(mod3)
hist(resid(mod3, type="pearson"))
shapiro.test(resid(mod3, type="pearson")) #n =16078 so shapiro test fails and won't calculate with my data
plot(resid(mod3, type="pearson")~age, data=datsc)
#there is a trend here--downward as age increases
plot(resid(mod3, type="pearson")~year, data=datsc)
#Still not great. There is more variation in early years when they are only looking at mass multiple times
plot(resid(mod3, type="pearson")~mass, data=datsc) 
#This is bad or at least I think it probably is but it looks like it's only
#really bad when mass is >30 so if I subset the data and only look at points
#where the mass is not greater than 30, then maybe it's ok. LIkely the mass
#never is actually greater than 30 anyway and if it is it's probably a clerical
#mistake
#If I re-run the code without those points it looks fine!!!!

