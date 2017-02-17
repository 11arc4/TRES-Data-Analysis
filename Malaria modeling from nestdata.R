#Preliminary malaria Breeding success models
#Will also want to include and interaction with growth rate probably when you have growth rate calculations for the nestlings

MasterMalaria<- AllNestdata [which(!is.na(AllNestdata$M.Malaria.Status) | !is.na(AllNestdata$F.Malaria.Status)),]

#THis is too strict subsetting!
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
plot(Fledge_mod) #Getting lots of points where leverage =1 (Ie only their own point is predicting their position)
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
plot(Egg_mod) #THis is not good. Probably needs fixing, also have leverage= 1 issue
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
plot(Hatch_mod) #THis is not good. Probably needs fixing, and also has leverage =1 problems
shapiro.test(resid(Hatch_mod)) #Says that my residuals are normal!
hist(resid(Hatch_mod)) #This histogram looks a bit left skewed but not too bad
plot(HatchMalaria$Year, resid(Hatch_mod))
plot(HatchMalaria$F.Malaria.Status, resid(Hatch_mod)) #Variance is very unequal
plot(HatchMalaria$M.Malaria.Status, resid(Hatch_mod)) #Variance is  equal! Yay
plot(HatchMalaria$M.Mass..g., resid(Hatch_mod))
plot(HatchMalaria$M.Mass..g., resid(Hatch_mod))
#Might need to tweak this analysis. Appears to be violating some of the assumptions. 
