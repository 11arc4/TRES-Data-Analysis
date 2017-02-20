library(ggplot2)
library(lubridate)

AllNestdata<- createMasterNestdataFile()


AllNestdata$F.Mass..g.<-as.numeric(AllNestdata$F.Mass..g.)
#I am going to remove all the points there the mass is above 40, or below 10 because that's likely clerical error
AllNestdata$F.Mass..g.[which(AllNestdata$F.Mass..g.>40  | AllNestdata$F.Mass..g.<10)]<- NA
AllNestdata$M.Mass..g.[which(AllNestdata$M.Mass..g.>40  | AllNestdata$M.Mass..g.<10)]<- NA

#making sure everything is in the right data format
AllNestdata$F.Tarsus..mm.<-as.numeric(AllNestdata$F.Tarsus..mm.)
AllNestdata$F.Wing..mm.<-as.numeric(AllNestdata$F.Wing..mm.)
AllNestdata$F.Nineth.Primary..mm.<-as.numeric(AllNestdata$F.Nineth.Primary..mm.)
AllNestdata$J.F.Day.measured <- yday(AllNestdata$F.Day.measured)
AllNestdata$M.Mass..g.<-as.numeric(AllNestdata$M.Mass..g.)
AllNestdata$M.Tarsus..mm.<-as.numeric(AllNestdata$M.Tarsus..mm.)
AllNestdata$M.Wing..mm.<-as.numeric(AllNestdata$M.Wing..mm.)
AllNestdata$M.Nineth.Primary..mm.<-as.numeric(AllNestdata$M.Nineth.Primary..mm.)
AllNestdata$J.M.Day.measured <- yday(AllNestdata$M.Day.measured)



#Basic ggplot of FEMALE mass by day measureed
ggplot(AllNestdata, aes(x=F.Day.measured, y= F.Mass..g.))+
  geom_jitter(alpha=1/10)+
  stat_smooth()

#Basic ggplot of MALE mass by day measured
ggplot(AllNestdata, aes(x=M.Day.measured, y= M.Mass..g.))+
  geom_jitter(alpha=1/10)+
  stat_smooth()

#Preliminary Adult mass by year models
#I've modeled mass by year and the julian day within the year (those factors are
#seperate because might have different patterns and shouldn't be included as a
#simple date)
#Subset the data prior to doiing the female mass analysis
FData <- AllNestdata[which(!is.na(AllNestdata$F.Mass..g.) & !is.na(AllNestdata$F.Day.measured) & !is.na(AllNestdata$Hatch.Date)), ]
FData$Diff <- FData$J.F.Day.measured - FData$Hatch.Date
Fmass_mod<- glm(as.numeric(FData$F.Mass..g.)~FData$Year* FData$Diff *FData$Clutch.Size)
summary(aov(Fmass_mod))
summary(Fmass_mod)
plot(Fmass_mod) 
#thse plots look decent except for the leverage! Need to talk to Fran about how to deal with that
shapiro.test(resid(Fmass_mod)) 
#Says that my residuals aren't normal! Weird because evverything else so far has looked good!
hist(resid(Fmass_mod)) #This histogram looks good!
plot(FData$Year, resid(Fmass_mod))
plot(FData$Diff, resid(Fmass_mod))
plot(FData$Clutch.Size, resid(Fmass_mod)) #This might not be the best.....


#Female mass analysis Removing Leverage Points
FNLData <- FData[-c(234, 527, 1094, 1114, 1128, 1389, 1430, 1558 ),]
FNLmass_mod<- glm(as.numeric(FNLData$F.Mass..g.)~FNLData$Year* FNLData$Diff *FNLData$Clutch.Size)
summary(aov(FNLmass_mod))
summary(FNLmass_mod)
plot(FNLmass_mod) #removing those points did solve the leverage issue!
shapiro.test(resid(FNLmass_mod)) 
#Says that my residuals aren't normal
hist(resid(FNLmass_mod)) #This histogram looks good and normal to me!
plot(FNLData$Year, resid(FNLmass_mod))
plot(FNLData$Diff, resid(FNLmass_mod))
plot(FNLData$Clutch.Size, resid(FNLmass_mod)) #This might not be the best still.....

#Overall it looks like removing the leveraging points makes very little difference

FNLmass_mod<- glm(as.numeric(FNLData$F.Mass..g.)~FNLData$Year* FNLData$Diff *FNLData$Clutch.Size, na.action = "na.omit")

MData <- AllNestdata[which(!is.na(AllNestdata$M.Mass..g.) & 
                             !is.na(AllNestdata$M.Day.measured) & 
                             !is.na(AllNestdata$Hatch.Date) & 
                             !is.na(AllNestdata$Clutch.Size)), ]
MData$Diff <-(MData$J.M.Day.measured- MData$Hatch.Date)
Mmass_mod<- lm(MData$M.Mass..g. ~ MData$Year * MData$Diff * MData$Clutch.Size)
summary(aov(Mmass_mod))
summary(Mmass_mod)
plot(Mmass_mod) #These plots look OK but again, there are points with leaverage
shapiro.test(resid(Mmass_mod)) #Says that my residuals aren't n rmal! Weird because evverything else so far has looked good!
hist(resid(Mmass_mod)) #This histogram looks good!
plot(MData$Year, resid(Mmass_mod))
plot(MData$Diff, resid(Mmass_mod)) 
plot(MData$Clutch.Size, resid(Mmass_mod)) #variance isn't really super equal
#Maybe not as good as before....


#let's just remove the leverageing points. It's a preliminary analysis for my committee meeting so it'll be ok
MNLData <- MData[-c(144, 313, 545, 592, 630  ),]
MNLmass_mod<- lm(MNLData$M.Mass..g. ~ MNLData$Year * MNLData$Diff * MNLData$Clutch.Size)
summary(aov(Mmass_mod))
summary(Mmass_mod)


ggplot(data=MNLData, aes(x=Year, y=M.Mass..g.))+
  geom_jitter(alpha=.5, aes(color=as.factor(Clutch.Size)))+
  geom_smooth(method=lm)+
  xlab("Year")+
  ylab("Male mass (g)")+
  theme_classic()+
  scale_color_discrete(name="Clutch size")
                    