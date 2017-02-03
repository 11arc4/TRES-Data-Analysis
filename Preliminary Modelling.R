library(ggplot2)
library(lubridate)

AllNestdata<- createMasterNestdataFile()


AllNestdata$F.Mass..g.<-as.numeric(AllNestdata$F.Mass..g.)
#I am going to remove all the points there the mass is above 40, or below 10 because that's likely clerical error
AllNestdata$F.Mass..g.[which(AllNestdata$F.Mass..g.>40  | AllNestdata$F.Mass..g.<10)]<- NA



AllNestdata$F.Tarsus..mm.<-as.numeric(AllNestdata$F.Tarsus..mm.)
AllNestdata$F.Wing..mm.<-as.numeric(AllNestdata$F.Wing..mm.)
AllNestdata$F.Nineth.Primary..mm.<-as.numeric(AllNestdata$F.Nineth.Primary..mm.)
AllNestdata$J.F.Day.measured <- yday(AllNestdata$F.Day.measured)

AllNestdata$M.Mass..g.<-as.numeric(AllNestdata$M.Mass..g.)
AllNestdata$M.Tarsus..mm.<-as.numeric(AllNestdata$M.Tarsus..mm.)
AllNestdata$M.Wing..mm.<-as.numeric(AllNestdata$M.Wing..mm.)
AllNestdata$M.Nineth.Primary..mm.<-as.numeric(AllNestdata$M.Nineth.Primary..mm.)
AllNestdata$J.M.Day.measured <- yday(AllNestdata$M.Day.measured)


#I am going to remove all the points there the mass is above 40, or below 10 because that's likely clerical error
AllNestdata$F.Mass..g.[which(AllNestdata$F.Mass..g.>40  | AllNestdata$F.Mass..g.<10)]<- NA
AllNestdata$M.Mass..g.[which(AllNestdata$M.Mass..g.>40  | AllNestdata$M.Mass..g.<10)]<- NA




ggplot(AllNestdata, aes(x=F.Day.measured, y= F.Mass..g.))+
  geom_point(alpha = 1/10)+
geom_jitter()+
geom_smooth()


ggplot(AllNestdata, aes(x=M.Day.measured, y= M.Mass..g.))+
  geom_point(alpha = 1/10)+
  geom_jitter(alpha=1/10)+
  stat_smooth(method=lm)
plot(AllNestdata$F.Mass..g.~AllNestdata$F.Day.measured)
plot(AllNestdata$M.Mass..g.~AllNestdata$M.Day.measured)

#Preliminary Adult mass by year models
#I've modeled mass by year and the julian day within the year (those factors are
#seperate because might have different patterns and shouldn't be included as a
#simple date)
#Subset the data prior to doiing the female mass analysis
FData <- AllNestdata[which(!is.na(AllNestdata$F.Mass..g.) & !is.na(AllNestdata$F.Day.measured)), ]
Fmass_mod<- lm(as.numeric(FData$F.Mass..g.)~FData$Year*FData$J.F.Day.measured)
summary(aov(Fmass_mod))
summary(Fmass_mod)
plot(Fmass_mod) #thse plots look decent. Not as good as the males but I don't think 
shapiro.test(resid(Fmass_mod)) #Says that my residuals aren't normal! Weird because evverything else so far has looked good!
hist(resid(Fmass_mod)) #This histogram looks good!
plot(FData$Year, resid(Fmass_mod))
plot(FData$J.F.Day.measured, resid(Fmass_mod))

MData <- AllNestdata[which(!is.na(AllNestdata$M.Mass..g.) & !is.na(AllNestdata$M.Day.measured)), ]
Mmass_mod<- lm(MData$M.Mass..g.~MData$Year * MData$J.M.Day.measured)
summary(aov(Mmass_mod))
summary(Mmass_mod)
plot(Mmass_mod) #These plots look fine to me so I think a linear model is probably appropriate!
shapiro.test(resid(Mmass_mod)) #Says that my residuals aren't normal! Weird because evverything else so far has looked good!
hist(resid(Mmass_mod)) #This histogram looks good!
plot(MData$Year, resid(Mmass_mod))
plot(MData$J.M.Day.measured, resid(Mmass_mod)) #

#Preliminary malaria Breeding success models
#Will also want to include and interaction with growth rate probably when you have growth rate calculations for the nestlings

MasterMalaria<- AllNestdata [which(!is.na(AllNestdata$M.Malaria.Status) | !is.na(AllNestdata$F.Malaria.Status)),]
FledgeMalaria <- MasterMalaria[ which(!is.na (MasterMalaria$Fledge.Size) & 
                         !is.na(MasterMalaria$F.Malaria.Status) & 
                         !is.na(MasterMalaria$Year) & 
                         !is.na( MasterMalaria$M.Malaria.Status) &
                         !is.na(MasterMalaria$F.Mass..g.) &
                         !is.na(MasterMalaria$M.Mass..g.)), ]
FledgeMalaria$F.Malaria.Status <- as.factor(FledgeMalaria$F.Malaria.Status)
FledgeMalaria$M.Malaria.Status <- as.factor(FledgeMalaria$M.Malaria.Status)

ggplot(FledgeMalaria, aes(x=Year, y= Fledge.Size, color=F.Malaria.Status) )+
  geom_point()
ggplot(FledgeMalaria, aes(x=Year, y= Fledge.Size, color=M.Malaria.Status) )+
  geom_point()

Fledge_mod <- lm(FledgeMalaria$Fledge.Size~
                   FledgeMalaria$F.Malaria.Status *
                   FledgeMalaria$Year * 
                   FledgeMalaria$M.Malaria.Status *
                   FledgeMalaria$F.Mass..g.*
                   FledgeMalaria$M.Mass..g.)


summary(aov(Fledge_mod))
plot(Fledge_mod)
shapiro.test(resid(Fledge_mod)) #Says that my residuals aren't normal!
hist(resid(Fledge_mod)) #This histogram looks a bit right skewed
plot(FledgeMalaria$Year, resid(Fledge_mod))
plot(FledgeMalaria$F.Malaria.Status, resid(Fledge_mod)) #really unequal variances
plot(FledgeMalaria$M.Malaria.Status, resid(Fledge_mod)) #really unequal variances
plot(FledgeMalaria$M.Mass..g., resid(Fledge_mod))
plot(FledgeMalaria$M.Mass..g., resid(Fledge_mod))
#Hmmmm. This might not be as good. I think I might need to do something a bit
#different since these malaria status is binomial and fledge success is bounded
#at 0



ClutchMalaria <- MasterMalaria[ which(!is.na (MasterMalaria$Clutch.Size) & 
                                        !is.na(MasterMalaria$F.Malaria.Status) & 
                                        !is.na(MasterMalaria$Year) & 
                                        !is.na( MasterMalaria$M.Malaria.Status) &
                                        !is.na(MasterMalaria$F.Mass..g.) &
                                        !is.na(MasterMalaria$M.Mass..g.)), ]
ClutchMalaria$F.Malaria.Status <- as.factor(ClutchMalaria$F.Malaria.Status)
ClutchMalaria$M.Malaria.Status <- as.factor(ClutchMalaria$M.Malaria.Status)

Egg_mod <- lm(ClutchMalaria$Clutch.Size~ClutchMalaria$F.Malaria.Status * ClutchMalaria$Year * ClutchMalaria$M.Malaria.Status)
summary(Egg_mod)
summary(aov(Egg_mod))
plot(Egg_mod) #THis is not good. Probably needs fixing
shapiro.test(resid(Egg_mod)) #Says that my residuals are normal!
hist(resid(Egg_mod)) #This histogram looks a bit left skewed but not too bad
plot(ClutchMalaria$Year, resid(Egg_mod))
plot(ClutchMalaria$F.Malaria.Status, resid(Egg_mod))
plot(ClutchMalaria$M.Malaria.Status, resid(Egg_mod))
plot(ClutchMalaria$M.Mass..g., resid(Egg_mod))
plot(ClutchMalaria$M.Mass..g., resid(Egg_mod))
#Aside from the quartile plots, everything looks pretty decent so I'm not sure that I need to fuss with this.
#Nothing is significant




HatchMalaria <- MasterMalaria[ which(!is.na (MasterMalaria$Hatch.Size) & 
                                        !is.na(MasterMalaria$F.Malaria.Status) & 
                                        !is.na(MasterMalaria$Year) & 
                                        !is.na( MasterMalaria$M.Malaria.Status) &
                                        !is.na(MasterMalaria$F.Mass..g.) &
                                        !is.na(MasterMalaria$M.Mass..g.)), ]
HatchMalaria$F.Malaria.Status <- as.factor(HatchMalaria$F.Malaria.Status)
HatchMalaria$M.Malaria.Status <- as.factor(HatchMalaria$M.Malaria.Status)

Hatch_mod <- lm(HatchMalaria$Hatch.Size~HatchMalaria$F.Malaria.Status * HatchMalaria$Year * HatchMalaria$M.Malaria.Status)
summary(Hatch_mod)
summary(aov(Hatch_mod))
plot(Hatch_mod) #THis is not good. Probably needs fixing
shapiro.test(resid(Hatch_mod)) #Says that my residuals are normal!
hist(resid(Hatch_mod)) #This histogram looks a bit left skewed but not too bad
plot(HatchMalaria$Year, resid(Hatch_mod))
plot(HatchMalaria$F.Malaria.Status, resid(Hatch_mod)) #Variance is very unequal
plot(HatchMalaria$M.Malaria.Status, resid(Hatch_mod)) #Variance is very equal! Yay
plot(HatchMalaria$M.Mass..g., resid(Hatch_mod))
plot(HatchMalaria$M.Mass..g., resid(Hatch_mod))
#Might need to tweak this analysis. Appears to be violating some of the assumptions. 
