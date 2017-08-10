setwd("~/Masters Thesis Project/TRES Data Analysis/RMark Preliminary Survival Analysis")
#Now all the stupid mark files will get put in their own folder away from
#everything important.
library(dplyr)
library(tidyr)
#Want to take out capture history, age at first capture and sex
years<- (seq(from= 1975, to = 2017, by=1))
dummy <- data.frame(matrix(ncol = length(years), nrow = length(as.list(globalData$birds))))
colnames(dummy) <- years
dummy$sex <- c()
dummy$ageAtFirstSight <- c()

#For each bird, we need to make a row of their capture history 
#This takes a while so make sure you're all set and ready to look at stuff
b <- 0
for (bird in as.list(globalData$birds)){
  b <- b+1
  y<- 0
  #We'll also set their sex because sex might change survival and I know it
  #changes capture probabilities (males were caught less)
  dummy$sex[b] <- bird$sex
  for (year in bird$yearsSeen$as.list()){
    y=y+1 #years are sorted so if y=1 then it's the first time we saw the bird
    if(y==1){
      #Age when we first saw you might also change your survival
      dummy$ageAtFirstSight[b] <- year$age
    }
    
    #Put a 1 in every year where the bird was seen 
    dummy[b, which(years==year$year)] <- 1
  }
  #Fill in all the empty spaces with 0s
  dummy[b, which(is.na(dummy[b,]))] <- 0
}


#Now let's make this dummy dataset into a data set that's in the right format for RMark. 

dummy$Ch <- apply(dummy[,1:43], 1, paste, collapse="")

datMark <- data.frame(dummy$Ch, dummy$ageAtFirstSight, dummy$sex)
colnames(datMark) <- c("ch", "AgeAtFirstSight", "Sex")

levels(datMark$AgeAtFirstSight)
datMark$FirstSighting[which(datMark$AgeAtFirstSight=="HY")] <- "Nestling"
datMark$FirstSighting[which(datMark$AgeAtFirstSight!="HY")] <- "Adult"
datMark$FirstSighting <- as.factor(datMark$FirstSighting)
datMark$ch <- as.character(datMark$ch)





#I want a data set that is JUST adult bird sightings. I need to remove the first
#sighting of birds that were caught as nestlings, and if they have no other
#sighting they are removed from the dataset
adummy <- dummy %>% filter(ageAtFirstSight!="HY")

ndummy <- dummy %>% filter(ageAtFirstSight=="HY")

for (i in 1:nrow(ndummy)){
  ndummy[i, which(ndummy[i,]==1)[1]]<- 0
  ndummy$sightings[i] <- sum(ndummy[i,1:43])
}

ndummy2 <- ndummy %>% filter(sightings>0)
ndummy2 <- ndummy2[, 1:(ncol(ndummy2)-1)] #remove the sightings column

dummy_adult <- rbind(ndummy2, adummy)

#Now we just have to put the adut data into the right Mark format and we're good to go
dummy_adult$Ch <- apply(dummy_adult[,1:43], 1, paste, collapse="")
dummy_adult$sex[which(dummy_adult$sex==7)] <- "U"
datMark_adult <- data.frame(dummy_adult$Ch, dummy_adult$ageAtFirstSight, dummy_adult$sex)
colnames(datMark_adult) <- c("ch", "AgeAtFirstSight", "Sex")
#Since we removed all the times we saw nestlings, I will put those birds in as SY at first sighting instead
datMark_adult$AgeAtFirstSight[which(datMark_adult$AgeAtFirstSight=="HY")] <- "SY"
datMark_adult$ch <- as.character(datMark_adult$ch)
datMark_adult$Sex[which(datMark_adult$Sex=="F ")] <- "F"
datMark_adult$Sex[which(datMark_adult$Sex==6)] <- "U"

#Since we only need survival for female birds to put into the matrix, lets do that....

datMark_F<- datMark_adult %>% filter(Sex=="F")
datMark_F$age <- rep(NA, nrow(datMark_F))
datMark_F$age[which(datMark_F$AgeAtFirstSight=="ASY")]<- 2
datMark_F$age[which(datMark_F$AgeAtFirstSight!="ASY")]<- 1


datMark_F <- datMark_F %>% filter ("000000000000000000000000000000000000000000"!=ch)
write.csv(datMark_F, file= "Adult Female MARK Data.csv", row.names = F, na="")
saveRDS(datMark_F, file="Adult Female MARK Data.rda" )
Female <- readRDS("Adult Female MARK Data.rda") #This works well and will be how I send this to Brad Fedy




############# MAKING THE NESLTING DATA

ndummy <- dummy %>% filter(ageAtFirstSight=="HY")
#This is looking at all of the nestlings for each year that were banded at day
#12 and then fledged and then either recruited or did not. If we want to
#seperate out recruitment death prior to fledging we need to remove all the
#entries for each year where a bird was seen as a nestling and then did not
#fledge. IE we should not have more sightings of nestlings per year than birds fledged that year. 


latedeath <- as.data.frame(matrix(nrow=length(as.list(globalData$nests)), ncol=4))
colnames(latedeath)<- c("year", "nestID", "banded", "fledge")
r<- 0
for (nest in as.list(globalData$nests)){
  r<- r+1 
  latedeath$fledge[r] <- nest$fledgeSize
  latedeath$year[r] <- nest$year
  latedeath$nestID[r] <- nest$siteID
  banded <- 0
  if(nest$nestlings$length>0){
    for (nstgptr in nest$nestlings$as.list()){
      nestling <- get(nstgptr$m_key, globalData$nestlings)
      if(!is.na(nestling$nestlingTRES$m_key)){
        banded <- banded+1
      }
    }
  }
  latedeath$banded[r] <- banded 
}
latedeath$diedbeforefledge <- latedeath$banded-latedeath$fledge



ndummy

c <- 0
for (i in 1975:2017){
  c<- c+1 
  diedbeforefledge <- sum(latedeath$diedbeforefledge[which(latedeath$year==i & latedeath$diedbeforefledge>0)], na.rm=T) #number of nestlings that we banded in a nest and then know died before fledging
  fledgedwithoutbands <- -sum(latedeath$diedbeforefledge[which(latedeath$year==i & latedeath$diedbeforefledge<0)])
  if(diedbeforefledge>0){
    #pick out all the capture records that were seen ONLY as nestings and remove some of those that I know actually died before fledging. 
    ndummy <- ndummy[-c(which(ndummy[,c]==1 & rowSums(ndummy[1:43])==1)[1:diedbeforefledge]),]
  }
}



ndummy$Ch <- apply(ndummy[,1:43], 1, paste, collapse="")
datMark_nestling <- data.frame(ndummy$Ch, ndummy$ageAtFirstSight,  ndummy$sex)
colnames(datMark_nestling) <- c("ch", "age", "sex")
#Since we removed all the times we saw nestlings, I will put those birds in as SY at first sighting instead
datMark_nestling$ch <- as.character(datMark_nestling$ch)
datMark_nestling <- datMark_nestling %>% filter ("000000000000000000000000000000000000000000"!=ch)

saveRDS(datMark_nestling, file="Nestling MARK Data.rda" )



#I'd also like to have a mark file with all the data (excluding nestlings that
#died before fledging) to try to get recruitment from

datMarkAll

dummyAll <- dummy
c <- 0
for (i in 1975:2017){
  c<- c+1 
  diedbeforefledge <- sum(latedeath$diedbeforefledge[which(latedeath$year==i & latedeath$diedbeforefledge>0)], na.rm=T) #number of nestlings that we banded in a nest and then know died before fledging
  fledgedwithoutbands <- -sum(latedeath$diedbeforefledge[which(latedeath$year==i & latedeath$diedbeforefledge<0)])
  if(diedbeforefledge>0){
    #pick out all the capture records that were seen ONLY as nestings and remove some of those that I know actually died before fledging. 
    dummyAll <- dummyAll[-c(which(dummyAll[,c]==1 & rowSums(dummyAll[1:43])==1)[1:diedbeforefledge]),]
  }
}



datMarkAll <- data.frame(dummyAll$Ch, dummyAll$ageAtFirstSight, dummyAll$sex)
colnames(datMarkAll) <- c("ch", "AgeAtFirstSight", "Sex")

levels(datMarkAll$AgeAtFirstSight)

datMarkAll$age <- rep(NA, nrow(datMarkAll))
datMarkAll$age[which(datMarkAll$AgeAtFirstSight=="ASY")]<- 2
datMarkAll$age[which(datMarkAll$AgeAtFirstSight=="SY" | datMarkAll$AgeAtFirstSight=="AHY")]<- 1
datMarkAll$age[which(datMarkAll$AgeAtFirstSight=="HY")]<- 0


datMarkAll$ch<- as.character(datMarkAll$ch)
saveRDS(datMarkAll, file="All Birds MARK Data.rda" )

