# CALCULATING DISTANCES FROM PARKS TO CRIMES===================================

# WORKING DIRECTORY------------------------------------------------------------
getwd()
setwd("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory")

# READ IN ASSAULT DATA---------------------------------------------------------
assaultdat <- read.csv("CSV/ChicagoAssaults.csv", stringsAsFactors=FALSE)
save(assaultdat, file="RDataFiles/assaultdat.RData")

# READ IN PARKS SHAPEFILES-----------------------------------------------------
library(rgdal)
library(broom)
library(maptools)
park <- readOGR(dsn="./Shapefiles/Parks_Aug2012", layer="Parks_Aug2012")
parkdat <- tidy(park, region="PARK")
parkdat$parknumber <- as.integer(factor(parkdat$id))
save(parkdat, file="RData/parkdat.RData")

# MATRIX OF LATITUDES AND LONGITUDES OF ASSAULTS-------------------------------
assaultlatlong <- na.omit(cbind(assaultdat$Longitude, assaultdat$Latitude))
colnames(assaultlatlong) <- c("longitude", "latitude")

# CONVERT ASSAULTS TO SPATIAL POINTS FORMAT------------------------------------
llproj <- '+init=epsg:4269'
assaultsp <- SpatialPoints(na.omit(assaultlatlong), proj4string=CRS(llproj))

# CONVERT ASSAULTS TO MERCATOR PROJECTION--------------------------------------
assaultmerc <- spTransform(assaultsp, CRS(proj4string(park)))

# CONVERT ASSAULTS TO COORDINATES----------------------------------------------
assaultxy <- coordinates(assaultmerc)

# SAMPLE ASSAULTS--------------------------------------------------------------
sampleassaultxy <- assaultxy[sample(1:nrow(assaultxy), 1000),]

# NUMBER OF SIDES OF EACH POLYGON IN MATRIX------------------------------------
parksize <- sort(table(parkdat$id))

# SOURCE IN R FILES------------------------------------------------------------
source('~/Documents/R/Chicago/WorkingDirectory/RFiles/DIST.R')
source('~/Documents/R/Chicago/WorkingDirectory/RFiles/DISTppoly.R')

# MATRIX OF DISTANCES FROM 10000 ASSAULT SAMPLE TO ALL PARKS-------------------
distancematrix <- matrix(NA, 1000, 556)
for (i in 1:1000) {
	distancematrix[,i+1] <- DISTppoly(sampleassaultxy, parkdat[parkdat$id==i, 1:2],
					  method="Euclidean")
}

# SAVE DISTANCEMATRIX TO RDATA OBJECT------------------------------------------
save(distancematrix, file="distancematrix.RData")

# DENSITY PLOTS OF DISTANCE MATRIX---------------------------------------------
plot(density(distancematrix[,1]), xlim=c(0, 75000), ylim=c(0, 1e-4),
     main="Density of Distances for Each Park",
     xlab="Distance",
     ylab="Density")
for (i in 2:556) {
	lines(density(distancematrix[,i]))
}
plot(density(distancematrix), xlim=c(0, 75000), ylim=c(0, 1e-4),
     main="Density of Distances for All Parks",
     xlab="Distance",
     ylab="Density")

# IDEAS------------------------------------------------------------------------
# Now that we have a density plot of the sample crime distances to park
# we need to create a density plot of random locations in Chicago
# to see if the distribution of crime distances differs from
# the distribtion of random distances

# BUILD RANDOM LOCATIONS-------------------------------------------------------
minlong <- min(assaultlatlong[,1])
maxlong <- max(assaultlatlong[,1])
minlat <- min(assaultlatlong[,2])
maxlat <- max(assaultlatlong[,2])
set.seed(777)
rlong <- runif(1000, minlong, maxlong)
rlat <- runif(1000, minlat, maxlat)

# MATRIX OF RANDOM LOCATIONS---------------------------------------------------
randomlatlong <- matrix(c(rlong,rlat), nrow=1000, ncol=2)
colnames(randomlatlong) <- c("randomLong", "randomLat")

# CONVERT RANDOM TO SPATIAL POINTS---------------------------------------------
randomsp <- SpatialPoints(randomlatlong, proj4string=CRS(llproj))

# CONVERT RANDOM TO MERCATOR---------------------------------------------------
randommerc <- spTransform(randomsp, CRS(proj4string(park)))

# CONVERT RANDOM TO COORDINATES------------------------------------------------
randomxy <- coordinates(randommerc)

# SOURCE IN R FILES------------------------------------------------------------
source('~/Documents/R/Chicago/WorkingDirectory/RFiles/DIST.R')
source('~/Documents/R/Chicago/WorkingDirectory/RFiles/DISTppoly.R')

# RUN DISTPPOLY ON RANDOMXY----------------------------------------------------
randomDistanceMatrix <- matrix(NA, 1000, 556)
for (i in 0:555) {
	randomDistanceMatrix[,i+1] <- DISTppoly(randomxy, parkdat[parkdat$id==i, 1:2],
					  method="Euclidean")
}

# DENSITY PLOTS OF RANDOM DISTANCE MATRIX--------------------------------------
plot(density(randomDistanceMatrix[,1]),
     main="Density of Random Distances for Each Park",
     xlab="Distance",
     ylab="Density")
for (i in 2:556) {
	lines(density(randomDistanceMatrix[,i]))
}
plot(density(randomDistanceMatrix),
     main="Density of Random Distances for All Parks",
     xlab="Distance",
     ylab="Density")

# WEIGHT-----------------------------------------------------------------------
weight <- 1/distancematrix
parkweight <- t(apply(weight, 1, function(x){x/sum(x)})) # contributions of each park to each assault
assaultweight <- apply(weight, 2, function(x){x/sum(x)}) # contributions of each assault to each park

# SKEWNESS OF DISTANCES--------------------------------------------------------
library(moments)
park_skew <- apply(distancematrix, 2, skewness)
plot(density(park_skew))
unique(parkdat$id)[which(park_skew<0)]

# LOAD-----------------------------------------------------------------
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/assaultdat.RData")
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/parkdat.RData")
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/distancematrix.RData")

# FROM DISTANCEMATRIX FIND NEAREST PARK----------------------------------------
nearestpark <- data.frame()
for (i in 1:10000) {
	nearestpark[i,1] <- which.min(distancematrix[i,])-1
	nearestpark[i,2] <- min(distancematrix[i,])
}
colnames(nearestpark) <- c("parkID", "distanceToCrime")
save(nearestpark, file="/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/nearestpark.RData")

# IDEAS------------------------------------------------------------------------
# now we can model this data with a parametric curve like a gamma distribution
# we'll need quite a few data points for this to work

# PLOT-------------------------------------------------------------------------
attach(nearestpark)
plot(density(distanceToCrime[parkID==219]))
summary(distanceToCrime[parkID==2])

# SKEWNESS OF DISTANCES--------------------------------------------------------
for (i in 1:556) {
	park_skew[i,] <- skewness(distancematrix[which(closest_park==i),i])
}




