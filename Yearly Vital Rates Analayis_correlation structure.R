
setwd("~/Masters Thesis Project/TRES Data Analysis/RMark Preliminary Survival Analysis")
library(RMark)
library(dplyr)
library(popbio)
#Function to estimate the parameters for a beta distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
#this is just the original eigen function from base R that has been modified so 
#it doesn't order the eigen values and vectors by size of eigen value (this is
#what matlab does and therefore what I need to do in order to match the Morris
#and Doak matlab code properly)
eigenUnOrdered <- function (x, symmetric, only.values = FALSE, EISPACK = FALSE) 
{
  x <- unname(as.matrix(x))
  n <- nrow(x)
  if (!n) 
    stop("0 x 0 matrix")
  if (n != ncol(x)) 
    stop("non-square matrix in 'eigen'")
  n <- as.integer(n)
  if (is.na(n)) 
    stop("invalid nrow(x)")
  complex.x <- is.complex(x)
  if (!all(is.finite(x))) 
    stop("infinite or missing values in 'x'")
  if (missing(symmetric)) 
    symmetric <- isSymmetric.matrix(x)
  if (symmetric) {
    z <- if (!complex.x) 
      .Internal(La_rs(x, only.values))
    else .Internal(La_rs_cmplx(x, only.values))
    ord <- rev(seq_along(z$values))
  }
  else {
    z <- if (!complex.x) 
      .Internal(La_rg(x, only.values))
    else .Internal(La_rg_cmplx(x, only.values))
    ord <- sort.list(Mod(z$values), decreasing = TRUE)
  }
  return(list(values = z$values, vectors = if (!only.values) z$vectors))
}
#Read in the data, including all the different types of data from RMark
PopData<- read.csv(file= "file:///C:/Users/Amelia/Documents/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Yearly Vital Rates.csv", na.strings=c(""), as.is=T)
nestlingMark <- readRDS( "Best MARK Results for Vital Rates Nestlings Analysis.rda")
summary <- summary(nestlingMark)
PopData$recruitment <- c(summary$reals$Phi$`Group:ageHY.sexU`[[1]][1,], NA) #doesn't matter which sex you choose because that wasn't allowed to change the survival (not enough data)
#there are a number of years that weren't parameterized properly-- you can tell because it's set to 1
#remove those
PopData$recruitment[which(PopData$recruitment>0.98)]<- NA

FemaleMark <- readRDS("Best MARK Results for Vital Rates Adults Analysis.rda")
summaryF <- summary(FemaleMark)
# I need to pull the number out on the diagonal for the SY birds
for(i in 1:42){
  PopData$SYReturn[i] <- summaryF$reals$Phi$`Group:age1`[[1]][i,i]
}
#remove 1975 from both of these columns-- it's set to 1 for both the SY and ASY
#but so few birds were banded it's a terrible estimate
PopData$SYReturn[1]<- NA
PopData$ASYReturn <- c(summaryF$reals$Phi$`Group:age2`[[1]][1,], NA)
PopData$ASYReturn[1]<- NA

#Calculate parameters necessary later
recruitmentParameters <- estBetaParams(mu=mean(PopData$recruitment, na.rm=T), var=var(PopData$recruitment, na.rm=T))
SYReturnParameters<- estBetaParams(mu=mean(PopData$SYReturn, na.rm=T ), var=var(PopData$SYReturn, na.rm=T))
ASYReturnParameters<- estBetaParams(mu=mean(PopData$ASYReturn, na.rm=T), var=var(PopData$ASYReturn, na.rm=T))
fledgeParameters <- estBetaParams(mu=mean(PopData$fledgeRate), var=var(PopData$fledgeRate))
hatchrateParameters <- estBetaParams(mu=mean(PopData$hatchRate), var=var(PopData$hatchRate))
averageNestParameters<- estBetaParams(mu=mean(PopData$averageNests-1), var=var(PopData$averageNests-1))
#clutch size works using a normal distribution


#Follow directions in Morris and Doak 2002 Quantitative Conservation Biology pg 284 

#1. get the correlation matrix of all the vital rates (not inluding the year!)
c <- cor(PopData[,2:ncol(PopData)], method = "spearman", use="complete.obs")

#2. make a matrix (W) who's columns are all the possible right eigenvectors of C
eigC <- eigenUnOrdered(c)
W<- eigC$vectors

#3. make a matrix (D) who's has the eigen values along the diagonal and all else 0
#check to make sure that all eigen values are >=0 first, if less than they are likely artificial and should be changed to 0
#All of my eigen values are >0 so life is good and we can carry on
sqrt(abs(eigC$vectors))
D<- matrix(data=0, nrow=7, ncol=7)
for (i in 1:7){
  D[i,i]<- eigC$values[i]
  
}


#This is where we need to start looping! and collecting data for the vital rates analysis!
vrdat <- as.data.frame(matrix(nrow=10000, ncol=9)) 
#Doing 10,000 because that's what Taylor et al 2012 did)
colnames(vrdat)<- c("hatchrate", "fledgerate", "recruitrate", "eggRecruitRate", "clutchSize", "averageNests", "SYReturn", "ASYReturn", "lambda" )
stages <- c("egg", "SY", "ASY")
A <- matrix(0, nrow=3, ncol=3, dimnames = list(stages, stages))
N0 <- c(0, 16 , 38)

for (i in 1:nrow(vrdat)){
  
  
  #3. Generate a vector (m) of uncorrelation standard normal variables. 
  m <- rnorm(n=7, mean=0, sd=1)
  
  #4. Multiply m by C 1/2 (ie W * D 1/2 * W') to get a set of correlated standard normal variables (y)
  c12<- W%*%sqrt(D)%*% t(W)
  
  y <- m%*%c12
  
  #5 None of our actual distributions were standard normal though so now we have
  #to find the F stat for the standard normal value given, and then find the value
  #for the equivalent F stat in the appropriate distribution.
  colnames(c)
  
  #averageNests goes first
  FaverageNests <- pnorm(y[1], mean=0, sd=1)
  vrdat$averageNests[i] <- 1+qbeta(FaverageNests, shape1= averageNestParameters$alpha , shape2=averageNestParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  #clutchSize 
  FClutchSize <- pnorm(y[2], mean=0, sd=1)
  vrdat$clutchSize[i] <- qnorm(FClutchSize, mean=mean(PopData$clutchSize), sd=sd(PopData$clutchSize))
  
  #hatchRate 
  FHatchRate <- pnorm(y[3], mean=0, sd=1)
  vrdat$hatchrate[i] <- qbeta(FHatchRate, shape1= hatchrateParameters$alpha , shape2=hatchrateParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  
  #fledgeRate 
  FFledgeRate <- pnorm(y[4], mean=0, sd=1)
  vrdat$fledgerate[i] <- qbeta(FFledgeRate, shape1= fledgeParameters$alpha , shape2=fledgeParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  #SYReturn
  FSYReturn <- pnorm(y[6], mean=0, sd=1)
  vrdat$SYReturn[i] <- qbeta(FSYReturn, shape1= SYReturnParameters$alpha , shape2=SYReturnParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  #ASYReturn
  FASYReturn <- pnorm(y[7], mean=0, sd=1)
  vrdat$ASYReturn[i] <- qbeta(FASYReturn, shape1= ASYReturnParameters$alpha , shape2=ASYReturnParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  
  #Recruitment
  FRecruitment <- pnorm(y[5], mean=0, sd=1)
  vrdat$recruitrate[i] <- qbeta(FRecruitment, shape1= recruitmentParameters$alpha , shape2=recruitmentParameters$beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  #Phew I think we've now generated correlated data!! That's so great. 
  
  
  vrdat$layrate[i]<- vrdat$clutchSize[i]*vrdat$averageNests[i]
  vrdat$eggRecruitRate[i] <- vrdat$hatchrate[i] * vrdat$fledgerate[i] * vrdat$recruitrate[i]
  
  
  #Put those vital rates into the matrix
  #Rows= what stage the birds are in now
  #Columns=what stage the birds are going to
  A[1, 2] <- vrdat$layrate[i] 
  A[1, 3] <- vrdat$layrate [i]
  A[2, 1] <- vrdat$eggRecruitRate[i]
  A[3, 2] <- vrdat$SYReturn[i]
  A[3, 3] <- vrdat$ASYReturn[i]
  p<- pop.projection(A=A, n=N0, iterations=42)
  
  vrdat$lambda[i] <- p$lambda
  
}


cor(vrdat[,1:8], method="spearman", use="complete.obs"
    )
cor(PopData, use="complete.obs", method="spearman")

#Fantastic! The correlation structure is right and the numbers look fine. I'm stoked!
plot(lambda~ASYReturn, data=vrdat)
plot(lambda~SYReturn, data=vrdat)
plot(lambda~layrate, data=vrdat)
plot(lambda~eggRecruitRate, data=vrdat) #Ooooo we have a winner!!
#What within the eggRecruitRate is important? \
plot(lambda~recruitrate, data=vrdat) #recruitment looks super important but I'm still not sure I really like those numbers very much so I want to do some more thinking about that. 
plot(lambda~hatchrate, data=vrdat) #hatch rate looks much less important now. 
plot(lambda~fledgerate, data=vrdat)  #fledge rate is important


############Now lets do a deterministic sensitivity analysis on this data
#Calculate sensitivity



