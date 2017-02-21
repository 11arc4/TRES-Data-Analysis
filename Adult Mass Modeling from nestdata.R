library(ggplot2)
library(lubridate)
library(car)
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
AllNestdata$Clutch.Size <- as.integer(AllNestdata$Clutch.Size)


#Basic ggplot of FEMALE mass by day measureed
ggplot(AllNestdata, aes(x=F.Day.measured, y= F.Mass..g.))+
  geom_jitter(alpha=1/10)+
  stat_smooth()

#Basic ggplot of MALE mass by day measured
ggplot(AllNestdata, aes(x=M.Day.measured, y= M.Mass..g.))+
  geom_jitter(alpha=1/10)+
  stat_smooth()
pvars <- c( "Year")
datsc <- AllNestdata
datsc[pvars] <- lapply(datsc[pvars],scale)
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

#Just look at the male masses

##I think I want to pull data out of the data structure to use instead-- then I can do a Mixed model and use birds that I checked multiple times!

#Get out data ready

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
AllNestdata$Clutch.Size <- as.integer(AllNestdata$Clutch.Size)

#WIll need to rescale year since it's on such a crazy scale
AllNestdata$Year <- AllNestdata$Year-1974



AllNestdata$MDiff <-(AllNestdata$J.M.Day.measured- AllNestdata$Hatch.Date)
AllNestdata$FDiff <-(AllNestdata$J.F.Day.measured- AllNestdata$Hatch.Date)

mdata <- AllNestdata[which(!is.na(AllNestdata$Year) & !is.na(AllNestdata$MDiff) & !is.na(AllNestdata$Clutch.Size) & !is.na(AllNestdata$M.Mass..g.)),]

#Step 1: Check for outliers


MyVar <- c("Year", "MDiff", "Clutch.Size")

Mydotplot(mdata[,MyVar])
#Why don't we remove clutch sizes >8--that will be due to experiments anyway

mdata <- mdata[which(mdata$Clutch.Size<8),]
#now those plots are a bit better


#Step 2: Check for colinearity
corvif(mdata[,MyVar])
#There doesn't really seem to be any bad colinearity actually-- all less than 1.5 and the suggested cutoff is 3

#Step 3: Check for relationships 
Myxyplot(mdata, MyVar, "M.Mass..g.") 
#MDiff appears to have a nonlinear relationship with mass--I will want to do something about that
#There are also most of the points right around MDiff=0. May be an issue


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
hist(resid(Mmass_mod)) #This histogram looks ok but not great
plot(MData$Year, resid(Mmass_mod))
plot(MData$Diff, resid(Mmass_mod)) 
plot(MData$Clutch.Size, resid(Mmass_mod)) #variance isn't really super equal
#Maybe not as good as before....


#let's just remove the leverageing points and see if that changes anything
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

ggplot(data=MNLData, aes(x=Year, y=M.Mass..g.))+
  geom_jitter(alpha=.5, aes(fill=Diff))+
  geom_smooth(method=lm)+
 
  theme_classic()+
  scale_fill_discrete(name="Time in breading season")


#I think it would be best to redo this analysis with scaled variables

pvars <- c( "Year")
datsc <- AllNestdata
datsc[pvars] <- lapply(datsc[pvars],scale)
FData2 <- datsc[which(!is.na(datsc$F.Mass..g.) & !is.na(datsc$F.Day.measured) & !is.na(datsc$Hatch.Date)), ]
FData2$Diff <- FData2$J.F.Day.measured - FData2$Hatch.Date
Fmass_mod2<- glm(as.numeric(FData2$F.Mass..g.)~FData2$Year* FData2$Diff *FData2$Clutch.Size)
summary(Fmass_mod2)
Anova(Fmass_mod2)
summary(aov(Fmass_mod2))
#OK so basically that shows that year is not really affecting female body mass



MData2 <- datsc[which(!is.na(datsc$M.Mass..g.) & !is.na(datsc$M.Day.measured) & !is.na(datsc$Hatch.Date)), ]
MData2$Diff <- MData2$J.M.Day.measured - MData2$Hatch.Date
Mmass_mod2<- glm(as.numeric(MData2$M.Mass..g.)~MData2$Year* MData2$Diff *MData2$Clutch.Size)
summary(Mmass_mod2)
Anova(Mmass_mod2)
summary(aov(Mmass_mod2))
plot(Mmass_mod2)
#Here year actually is affecting male mass!

#Since I scaled year though it's very difficult to interpret....



#What if I rescale the years in a more intuitive fashion, with year 0 as 1974
MData3 <- MData
MData3$Year <- MData3$Year-1974
Mmass_mod3<- glm(as.numeric(MData3$M.Mass..g.)~MData3$Year* MData3$Diff *MData3$Clutch.Size)
summary(Mmass_mod3)
summary(aov(Mmass_mod3))
Anova(Mmass_mod3)
plot(Mmass_mod3)

#Need to make a plot for the committee meeting presentation with this data

MaleMassPlot <- 
ggplot(data=MData3, aes(x=Year+1971, y=M.Mass..g.))+
  geom_jitter(alpha=0.5)+
  geom_smooth(method=lm)+
  xlab ("Year") +
  ylab ("Male mass (g)") +
  ylim(14, 29)+
  xlim(1983, 2016)+
  theme_classic()+
  theme(text = element_text(size=20))




FData3 <- FData
FData3$Year <- FData3$Year-1974
Fmass_mod3<- glm(as.numeric(FData3$F.Mass..g.)~FData3$Year* FData3$Diff *FData3$Clutch.Size)
plot(Fmass_mod3)
summary(Fmass_mod3)
Anova(Fmass_mod3)
summary(aov(Fmass_mod3))

FemaleMassPlot <- 
  ggplot(data=FData3, aes(x=Year+1974, y=F.Mass..g.))+
  geom_jitter(alpha=0.5)+
  geom_smooth(method=lm)+
  xlab ("Year") +
  ylab ("Female mass (g)") +
  ylim(14, 29)+
  xlim(1983, 2016)+
  theme_classic()+
  theme(text = element_text(size=20))
                    