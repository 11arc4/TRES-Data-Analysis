library(ggplot2)

AllNestdata<- createMasterNestdataFile()


AllNestdata$F.Mass..g.<-as.numeric(AllNestdata$F.Mass..g.)
AllNestdata$F.Tarsus..mm.<-as.numeric(AllNestdata$F.Tarsus..mm.)
AllNestdata$F.Wing..mm.<-as.numeric(AllNestdata$F.Wing..mm.)
AllNestdata$F.Nineth.Primary..mm.<-as.numeric(AllNestdata$F.Nineth.Primary..mm.)

AllNestdata$M.Mass..g.<-as.numeric(AllNestdata$M.Mass..g.)
AllNestdata$M.Tarsus..mm.<-as.numeric(AllNestdata$M.Tarsus..mm.)
AllNestdata$M.Wing..mm.<-as.numeric(AllNestdata$M.Wing..mm.)
AllNestdata$M.Nineth.Primary..mm.<-as.numeric(AllNestdata$M.Nineth.Primary..mm.)


ggplot(AllNestdata, aes(x=F.Day.measured, y= F.Mass..g.))+
  geom_point(alpha = 1/10)+
geom_jitter()+
geom_smooth()


ggplot(AllNestdata, aes(x=M.Day.measured, y= M.Mass..g.))+
  geom_point(alpha = 1/10)+
  geom_jitter(alpha=1/10)
plot(AllNestdata$F.Mass..g.~AllNestdata$F.Day.measured)
plot(AllNestdata$M.Mass..g.~AllNestdata$M.Day.measured, ylim=c(10, 40))

#Preliminary Adult mass by year models
FmassYear_mod<- lm(as.numeric(AllNestdata$F.Mass..g.)~AllNestdata$Year)
summary(aov(FmassYear_mod))

Fmass_mod<- lm(as.numeric(AllNestdata$F.Mass..g.)~AllNestdata$F.Day.measured)
summary(aov(Fmass_mod))



MmassYear_mod <- lm(as.numeric(AllNestdata$M.Mass..g.)~AllNestdata$Year)
summary(aov(MmassYear_mod))

Mmass_mod<- lm(AllNestdata$M.Mass..g.~AllNestdata$M.Day.measured)
summary(aov(Mmass_mod))


#Preliminary malaria Breeding success models
#Will also want to include and interaction with growth rate probably when you have growth rate calculations for the nestlings

MasterMalaria<- AllNestdata [which(!is.na(AllNestdata$M.Malaria.Status) | !is.na(AllNestdata$F.Malaria.Status)),]

Fledge_mod <- lm(AllNestdata$Fledge.Size~AllNestdata$F.Malaria.Status * AllNestdata$Year * AllNestdata$M.Malaria.Status *AllNestdata$F.Mass..g.*AllNestdata$M.Mass..g.)
summary(aov(Fledge_mod))

Egg_mod <- lm(AllNestdata$Clutch.Size~AllNestdata$F.Malaria.Status * AllNestdata$Year * AllNestdata$M.Malaria.Status)
summary(Egg_mod)

Hatch_mod <- lm(AllNestdata$Hatch.Size~AllNestdata$F.Malaria.Status * AllNestdata$Year * AllNestdata$M.Malaria.Status)
summary(Hatch_mod)


