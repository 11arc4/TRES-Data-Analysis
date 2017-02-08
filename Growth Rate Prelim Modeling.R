# Nestling Growth Rate analysis
library(dplyr)
library(ggplot2)
library(scatterplot3d)

inputdir<- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis"
filename <- "Nestling Growth Rate Modeling Data.csv"
gdata <- read.csv(paste (inputdir, filename, sep="/"), as.is=TRUE, na.strings=c("", "NA"))

plot (growthrate_mass~ year, data=gdata)

#Here are a couple of points that just really don't make sense, so will need to
#be excluded--I bet they're calculating with the wrong numbers or something

gdata[which(gdata$growthrate_mass< (-5)), ]
gdata[which(gdata$growthrate_mass> (10)), ]


#removed the wonky points
gdata2 <- gdata [which(gdata$growthrate_mass> (-5) & gdata$growthrate_mass< (10)), ]

gmdata <- gdata2[which(!is.na(gdata2$hatchdate) & !is.na(gdata2$hatchsize)), ]
plot (growthrate_mass~ year, data=gmdata) #That's way better!! No super wonky points anymore

ggplot(gmdata, aes(x=year, y= growthrate_mass))+
  geom_point(alpha= .5, position="jitter", aes(color=hatchsize))+
  xlab("Year") +
  ylab("Growth Rate (g/day)")+
  stat_smooth()


#Model growth rate!
mod <- lm ( growthrate_mass~ (year^2) * year*hatchdate*hatchsize, data = gmdata)
summary(aov(mod))
summary(mod)
plot(mod) #Q-Q plot isn't that great
hist(resid(mod)) #little left skewed but not too bad
shapiro.test(resid(mod)) #significantly different from normal
plot(resid(mod)~gmdata$year)
plot(resid(mod)~gmdata$hatchdate)
plot(resid(mod)~gmdata$hatchsize)






#Model Fledge success based on growth rate averages per nest 
#Need to use averages per nest because otherwise it will be unclear which individuals fledges and which died
FGrData <- summarise(group_by(.data=gmdata, nest), mean(year), mean(growthrate_mass, na.rm = TRUE), mean(hatchdate), mean(hatchsize), mean(fledgesize))




colnames(FGrData) <- c("nest", "year", "growthrate", "hatchdate", "hatchsize", "fledgesize")

scatterplot3d(x= FGrData$year, y=FGrData$growthrate, z=FGrData$fledgesize)


fmod <- lm(fledgesize ~ year* growthrate * hatchdate * hatchsize, data= FGrData)
summary(aov(fmod))
plot(fmod) #there's a definite trend in the QQ plots
hist(resid(fmod)) #left skewed
shapiro.test(resid(fmod)) #not normal


fglmod <- glm(fledgesize ~ year* growthrate * hatchdate * hatchsize, data= FGrData, family=poisson(link="log"))
summary(aov(fglmod))
plot(fglmod) #This actually fits worse (I think)

