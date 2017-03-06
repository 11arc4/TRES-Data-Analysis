#Bootstrapping adult mass analysis

#Including multiple measures of the same bird using a random effect ruins our 
#model We will deal with this by taking ony one random measurement of each bird 
#for the analysis (from a random year and a random time within the year), and
#bootstrapping the whole analysis to do it again and again and again
setwd("~/Masters Thesis Project/TRES Data Analysis")
library(car)
library(MuMIn)
library(beepr)
for (i in 1:25){
  band <- c()
  year <- c()
  sex <- c()
  age <- c()
  mass <- c()
  tarsus <- c()
  wingChord <- c()
  ninethPrim <- c()
  malaria <- c() 
  dateMeas <- c()
  nestID <- c()
  clutchsize <- c()
  laydate <-c()
  incdate <- c()
  hatchsize <- c()
  hatchdate <- c()
  fledgedate <- c()
  fledgesize <- c()
  reasonforfailure <- c()
  
  
  for (bird in as.list(globalData$birds)){
    firstyear<- bird$yearsSeen$buffer[[1]]
    if (is.na(firstyear$hatchNest$m_key)){
      x=1:bird$yearsSeen$length
      yearNumber <- sample(x, size=1)
    } else {
      if(bird$yearsSeen$length==1){
        next
      } else {
        x=2:bird$yearsSeen$length 
        yearNumber <- sample(x, size=1)
        
      }
      
    }
    Year <- bird$yearsSeen$buffer[[yearNumber]]
    
    #pick the nest to get breeding statistics from
    if(Year$nest$length==0){
      nID<- NA
      clsize <- NA
      ldate <-NA
      idate <- NA
      hsize <- NA
      hdate <- NA
      fdate <- NA
      fsize <- NA
      rffailure <- NA
    } else {
      #Sort nests by order
      l2 <- Year$nest$as.list()
      l3 <- l2[order(sapply(l2, function(v) { 
        n <- get(v$m_key, globalData$nests)
        n$firstEggDate} ))]
      Year$nest$replaceList(l3)
      #message("Sorted nests list for bird ", bird$bandID, "by first egg date")
      
      nestKey <-Year$nest$buffer[[1]]
      nest <-get(nestKey$m_key, nestKey$m_hash)
      
      nID<- nest$siteID
      clsize <- nest$clutchSize
      ldate <- nest$firstEggDate
      idate <- nest$lastEggDate
      hsize <- nest$hatchSize
      hdate <- nest$hatchDate
      fdate <- nest$fledgeDate
      fsize <- nest$fledgeSize
      rffailure <- nest$reasonforFailure
    }
    
    if(Year$observations$length>0){
      Observations <- Year$observations$as.list()
      lobs <- Observations[which(lapply(Observations, function(v) {v$type} )=="bodymeasurement")]
      if(length(lobs)>0){
        obs <- lobs[[sample(size=1, 1:length(lobs))]]
        l <- length(sex)+1
        dateMeas[l]<- obs$date
        mass[l] <- obs$mass
        tarsus[l] <- obs$tarsus
        wingChord[l] <- obs$wingChord
        ninethPrim[l] <- obs$ninthPrimary
        nestID[l]<- nID
        clutchsize[l] <- clsize
        laydate[l] <-ldate
        incdate[l] <-idate
        hatchsize[l] <-hsize
        hatchdate[l] <- hdate
        fledgedate[l] <- fdate
        fledgesize[l] <-fsize
        reasonforfailure[l] <- rffailure
        year[l]<- Year$year
        sex[l] <- bird$sex
        band[l] <- bird$bandID
        age[l]<- Year$age
        
        
      } else {
        dateMeas[l]<- NA
        mass[l] <- NA
        tarsus[l] <- NA
        wingChord[l] <- NA
        ninethPrim[l] <- NA
        nestID[l]<- nID
        clutchsize[l] <- clsize
        laydate[l] <-ldate
        incdate[l] <-idate
        hatchsize[l] <-hsize
        hatchdate[l] <- hdate
        fledgedate[l] <- fdate
        fledgesize[l] <-fsize
        reasonforfailure[l] <- rffailure
        year[l]<- Year$year
        sex[l] <- bird$sex
        band[l] <- bird$bandID
        age[l]<- Year$age
      } 
    }
  }  
  
  Adult <- data.frame(band ,
                      year ,
                      sex ,
                      age ,
                      mass ,
                      tarsus ,
                      wingChord ,
                      ninethPrim ,
                      dateMeas ,
                      nestID ,
                      clutchsize,
                      laydate ,
                      incdate ,
                      hatchsize ,
                      hatchdate ,
                      fledgedate ,
                      fledgesize ,
                      reasonforfailure)
  #Let's just do a quick clean up of those dates! (They're not all in the right
  #format which really means that I need to go into the globalData and fix
  #something but I haven't done that yet!)
  
  Adult$dateMeas[which(grepl("/", Adult$dateMeas))] <- as.character(as.Date(as.character(Adult$dateMeas[which(grepl("/", Adult$dateMeas))]), format= "%m/%d/%Y"))
  
  #OK Now we're ready to do the actual analysis!
  
  Adult$sex [which(Adult$sex=="6" | Adult$sex=="7")] <- "U"
  Adult$sex [which(Adult$sex=="F ")] <- "F"
  Adult$sex<- as.factor(Adult$sex)
  Adult$dateMeas <- as.Date(Adult$dateMeas, format= "%Y-%m-%d")
  Adult$dateMeas2 <- yday(Adult$dateMeas)
  Adult$year2 <- Adult$year -1974 #rescales eveyrone so analysis works better
  Adult$age <- as.factor(Adult$age) 
  #we don't really have enough data for most of the age classes to treat birds as
  #anyting other than ASY and SY for females and AHY for males and unknown so I've
  #made an "agesex" column
  Adult$agesex <- rep(NA, length(Adult$age))
  Adult$agesex [which(Adult$sex=="M")] <- "AHY-M"
  Adult$agesex [which(Adult$sex=="F" & Adult$sex=="SY")] <- "SY-F"
  Adult$agesex [which(Adult$sex=="F" & Adult$sex!="SY" & !is.na(Adult$sex))] <- "ASY-F"
  Adult$agesex[which(Adult$sex=="U")] <- "AHY-U"
  # I also need to remove all the birds that were
  #polygynous because I still haven't really figured out a good way to deal with
  #them in the dataset....
  
  #remove polygynous nests
  Adult <- Adult[which(Adult$age!= "ASY/ASY" & Adult$age != "AHY/AHY" & Adult$age != "SY + ASY"),]
  
  Adult$hatchdate[which(Adult$hatchdate==0)] <- NA
  Adult$mass[which(Adult$mass<15 | Adult$mass>30 )] <- NA
  
  Adult$diff <- Adult$dateMeas2-Adult$hatchdate 
  #THis will give positive values close if measured after hatching and negative before
  
  #remove outliers
  adult2 <- Adult[which(Adult$clutchsize<9 & Adult$diff<50 & Adult$diff >-90),]
  
  adult2$diff2 <- scale(adult2$diff)
  adult3 <- adult2[which(!is.na(adult2$clutchsize) &
                           !is.na(adult2$sex) &
                           !is.na(adult2$year2) &
                           !is.na(adult2$diff) &
                           !is.na(adult2$mass)
                         
  ),]
  mod <- lm(mass~ year2+clutchsize+diff2+sex+ #all the main effects
              year2:clutchsize + year2:diff2 + year2:sex + clutchsize:diff2 + clutchsize:sex + diff2:sex+ #2 way interactions
              year2:clutchsize:diff2 + year2:clutchsize:sex + year2:sex:diff2 + clutchsize:diff2:sex, data=adult3)
  #plot(mod)  #Aside from that problematic argyle deal this looks pretty decent!
  #plot(resid(mod)~ adult3$year2) #good enough
  #plot(resid(mod)~adult3$sex) #this is fine
  #plot(resid(mod)~ adult3$clutchsize) #again, variance issues but that's probably oK because it's just due to having less data
  #plot(resid(mod)~adult3$diff2)
  #hist(resid(mod, type="pearson"))
  
  bootAV <- Anova(mod)
  bootsumm <- summary(mod)
  #extract the F values from the model
  #Extract the estimates
  if(i==1){
    bootestimates <- data.frame(matrix(NA, ncol=22, nrow=1000))
    colnames(bootestimates)<-rownames(bootsumm$coefficients) 
    bootestimates$N <- rep(NA, length(bootestimates$diff2))
    
    bootfstats <- data.frame(matrix(NA, ncol=15, nrow=1000))
    colnames(bootfstats) <- rownames(bootAV)
    bootfstats$N <- rep(NA, length(bootfstats$year2))
    
  }
  bootfstats[i,1:15]<-  bootAV[["F value"]]
  bootfstats$N[i] <- length(adult3$band)
  bootestimates [i,1:22] <-bootsumm$coefficients[1:22]
  bootestimates$N[i] <- length(adult3$band)
  message ("done", i, "in 1000 iterations of analysis", sep=" ")
}
#IF I want to dredge the model instead
#options(na.action="na.fail")
#allmodels <- dredge(mod, extra="R^2", beta="sd") #this will give you R^2
#topmodels <- get.models(allmodels, subset=delta<3)
beep(1)

#That took FOREVER so I'll just write that into a 
outputdir <- "~/Masters Thesis Project/TRES Data Analysis/Extracted Statistics"
write.csv(bootfstats, file= paste(outputdir, "Adult Mass Bootstrapping F Statistics.csv", sep="/"), row.names=F, na="" )
write.csv(bootestimates, file= paste(outputdir, "Adult Mass Bootstrapping Estimates.csv", sep="/"), row.names=F, na="" )

#To actually calculated the F statistic, I've had to do this kind of convoluted 
#mess because the sample sizes are not the same each year. Unsure how to deal
#with this right now. Will come back


FStats <-data.frame(matrix(NA, nrow=length(bootfstats), ncol=3))
colnames(FStats)<- c("F", "sd", "pvalue")
rownames(FStats) <- colnames(bootfstats)


mean(bootfstats[,1])
