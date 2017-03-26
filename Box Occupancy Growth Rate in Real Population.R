#Calculate the real population growth rates per year!


#Using box occupancy to get this surrogate population growth rate

BoxOccupancy <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Box Occupancy using nestdata renest function.csv", as.is=T, na.strings = c("", "NA"))
colnames(BoxOccupancy)[1]<- "Year"

#NES had boxes but wasn't surveyed in 1994 so I'll turn that to an NA not a 0
BoxOccupancy$BoxOccNES[20]<- NA

BoxOccupancy$BoxOccOtherLongTermGrid<- (BoxOccupancy$HUBirdTotal+BoxOccupancy$NBBirdTotal + BoxOccupancy$SPBirdTotal+BoxOccupancy$SRBBirdTotal+BoxOccupancy$BGBirdTotal)/
  (BoxOccupancy$HUBoxes+BoxOccupancy$NBBoxes+BoxOccupancy$SPBoxes+BoxOccupancy$SRBBoxes+ BoxOccupancy$BGBoxes)/2

BoxOccupancy$BoxOccSolitary<- (BoxOccupancy$BirdTotal- (BoxOccupancy$NESBirdTotal +BoxOccupancy$HUBirdTotal+BoxOccupancy$NBBirdTotal + BoxOccupancy$SPBirdTotal+BoxOccupancy$SRBBirdTotal+BoxOccupancy$BGBirdTotal))/
  (BoxOccupancy$YearlyBoxes-(BoxOccupancy$NESBoxes+BoxOccupancy$HUBoxes+BoxOccupancy$NBBoxes+BoxOccupancy$SPBoxes+BoxOccupancy$SRBBoxes+ BoxOccupancy$BGBoxes))/2


#let's calculate population growth rate for all boxes, NES, Grids, and Solitary
for(y in 2: length(BoxOccupancy$Year)){
  BoxOccupancy$lambdaAll[y]<- BoxOccupancy$BoxOccTotal[y]/BoxOccupancy$BoxOccTotal[y-1]
  BoxOccupancy$lambdaNES[y]<- BoxOccupancy$BoxOccNES[y]/BoxOccupancy$BoxOccNES[y-1]
  BoxOccupancy$lambdaOtherLongGrids[y]<- BoxOccupancy$BoxOccOtherLongTermGrid[y]/BoxOccupancy$BoxOccOtherLongTermGrid[y-1]
  BoxOccupancy$lambdaSolitary[y]<- BoxOccupancy$BoxOccSolitary[y]/BoxOccupancy$BoxOccSolitary[y-1]
  
}
#Need to pick a time period before and during decline-- 1993 seems to be the split point!
plot(BoxOccupancy$BoxOccTotal ~BoxOccupancy$Year)
abline(v=1993)

mean(BoxOccupancy$lambdaAll, na.rm=T) #mean is 1.055783 overall

#Mean Growth rate for years 1975-1993 (when population is growing) is 1.12181
mean(BoxOccupancy$lambdaAll[1:19], na.rm=T)
#Mean Growth rate for years 1994-2016 (when population is shrinking) #1.00411

mean(BoxOccupancy$lambdaAll[20:42], na.rm=T)
#What if we take out 2015 and 2016? Now lambda is 0.943785-- makes more sense. 
mean(BoxOccupancy$lambdaAll[20:40], na.rm=T)





plot(BoxOccupancy$NESBirdTotal~BoxOccupancy$Year)
abline(v=1993)
abline(v=1999, col="red")
#Looks like NES takes a bit longer to start crashing but when it does maybe it even crashes harder (arround 1998)

#What if we do just NES?
mean(BoxOccupancy$lambdaNES, na.rm=T) #mean is 1.074229 overall

#Mean Growth rate for years 1975-1993 (when population is growing) is 1.049355
mean(BoxOccupancy$lambdaNES[1:19], na.rm=T)
#Mean Growth rate for years 1994-2016 (when overall population is shrinking) #1.133926--- NES wasn't shrinking, actually it was growing faster than ever!
mean(BoxOccupancy$lambdaNES[20:42], na.rm=T)
#Mean growth rate for years 1999-2016 when NES is also starting to shrink!
mean(BoxOccupancy$lambdaNES[26:42], na.rm=T)
#Not actualy informative because only 2 years in this period of time....


#What about if we do the other long term grids?
plot(BoxOccOtherLongTermGrid ~ Year, data=BoxOccupancy)
abline(v=1993)

mean(BoxOccupancy$lambdaOtherLongGrids, na.rm=T) #1.014182

#Mean Growth rate for years 1975-1993 (when population is growing) is 1.037689
mean(BoxOccupancy$lambdaOtherLongGrids[1:19], na.rm=T)

#Mean Growth rate for years 1975-1993 (when population is declining!) is 0.9968062
mean(BoxOccupancy$lambdaOtherLongGrids[20:42], na.rm=T)

#again we can get rid of 2015 and 2016 but I think that's quite unnecessary not because they look like they fit in ok
# if we do then growth rate is 0.9606213
mean(BoxOccupancy$lambdaOtherLongGrids[20:40], na.rm=T)



###What about for the solitary boxes and grids that aren't set up long term (may
###want to ultimately seperate the two of these but for now we'll keep it
###together)
#This is doing a kind of different thing
plot(BoxOccSolitary~years, data=BoxOccupancy)
abline(v=1993)
abline(v=1999)

mean(BoxOccupancy$lambdaSolitary, na.rm=T) #1.240325

#Mean Growth rate for years 1975-1993 (when population is growing) is 1.380793-- growin faster than the rest of everyone
mean(BoxOccupancy$lambdaSolitary[1:19], na.rm=T)
#Mean Growth rate for years 1994-2016 (when overall population is shrinking) is 1.130393
mean(BoxOccupancy$lambdaSolitary[20:42], na.rm=T)
#Mean growth rate for years 1999-2016 when NES is also starting to shrink--
#solitaries seem to be following this pattern too but is't a lot less clear and
#again they still look like they're increasing so I don't know how helpful that
#is lambda = 1.168016
mean(BoxOccupancy$lambdaSolitary[26:42], na.rm=T)



