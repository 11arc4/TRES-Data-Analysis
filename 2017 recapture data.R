DoubleCaught <- as.data.frame(matrix(nrow=250, ncol=21))
colnames(DoubleCaught)<- c("BandID", 
                           "Sex",
                           "Age",
                           "NestID", 
                           "Date_1", 
                           "Mass_1", 
                           "Wing_1", 
                           "Tarus_1", 
                           "Fat_1", 
                           "Date_2", 
                           "Mass_2", 
                           "Wing_2", 
                           "Tarus_2", 
                           "Fat_2",
                           "FirstEggDate", 
                           "ClutchSize", 
                           "HatchDate", 
                           "HatchSize", 
                           "FledgeFailDate", 
                           "FledgeSize", 
                           "AverageNestlingGrowthRate")
i=0
for (bird in as.list(globalData$birds)){
  
  for(year in as.list(bird$yearsSeen$as.list())){
    if (year$year==2017 & year$observations$length>1 ){
      i=i+1
      DoubleCaught$BandID[i]<- bird$bandID
      DoubleCaught$Sex[i]<- bird$sex
      DoubleCaught$Age[i]<- year$age
      FirstObs <- year$observations$buffer[[1]] #take the first observation
      DoubleCaught$Date_1[i]<- FirstObs$date
      DoubleCaught$Mass_1[i]<- FirstObs$mass
      DoubleCaught$Wing_1[i]<- FirstObs$wingChord
      DoubleCaught$Tarus_1[i]<- FirstObs$tarsus
      if(year$observations$length>1){
        LastObs <- year$observations$buffer[[year$observations$length]] #Take the last observation. Ultimately will likely want to be more spcific but for now this will be good enough
        DoubleCaught$Date_2[i]<- LastObs$date
        DoubleCaught$Mass_2[i]<- LastObs$mass
        DoubleCaught$Wing_2[i]<- LastObs$wingChord
        DoubleCaught$Tarus_2[i]<- LastObs$tarsus
      }
     
#we also have fat scores but those havven't been inputted into the database yet because they are new(ish)
      
      # #note that we are pulling the birds first nest for this analysis
      if(year$nest$length>0){
        NestEnvPointer <- year$nest$buffer[[1]]
        Nest <- get(NestEnvPointer$m_key, globalData$nests)
        DoubleCaught$NestID[i] <- NestEnvPointer$m_key
        DoubleCaught$FirstEggDate[i] <- Nest$firstEggDate
        DoubleCaught$ClutchSize[i]<- Nest$clutchSize
        DoubleCaught$HatchDate[i]<- Nest$hatchDate
        DoubleCaught$HatchSize[i]<- Nest$hatchSize
        DoubleCaught$FledgeFailDate[i]<- Nest$fledgeDate
        DoubleCaught$FledgeSize[i]<- Nest$fledgeSize
      }

    
    }
  }
}

DoubleCaught<- DoubleCaught[1:i, ]


library(ggplot2)
ggplot(DoubleCaught, aes(x=Mass_1, y=Mass_2, color=HatchSize))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~Sex)+ #OH SHITTTT males are really much more interesting. 
  xlab("Initial Mass")+
  ylab("Mass after nesting")



FDoubleCaught<- DoubleCaught %>% filter(Sex=="F")
mod <- lm(Mass_2 ~ Mass_1 + ClutchSize+ HatchSize, data=FDoubleCaught) #Also want to include provisioning rate
plot(mod)
plot(FDoubleCaught$Mass_1 ~ resid(mod))
plot(FDoubleCaught$ClutchSize ~ resid(mod))
plot(FDoubleCaught$HatchSize ~ resid(mod))


hist(resid(mod)) #eh doesn't look too bad
shapiro.test(resid(mod)) #normal!


Anova(mod) #nothing is significant predictor. Huh that's kind of weird


MDoubleCaught<- DoubleCaught %>% filter(Sex=="M")
mod <- lm(Mass_2 ~ Mass_1 + ClutchSize+ HatchSize, data=MDoubleCaught) #Also want to include provisioning rate
plot(mod)
plot(MDoubleCaught$Mass_1 ~ resid(mod))
plot(MDoubleCaught$ClutchSize ~ resid(mod))
plot(MDoubleCaught$HatchSize ~ resid(mod))

hist(resid(mod)) #eh doesn't look too bad
shapiro.test(resid(mod)) #normal!
Anova(mod) #nothing is significant predictor. 
summary(mod)


#maybe we need to calculate condition


