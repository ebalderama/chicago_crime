# POINT PROCESS MODELLING======================================================

# LOAD-------------------------------------------------------------------------
load("RData/assaultxy.RData")
load("RData/region.RData")
load("RData/park.RData")
load("RData/parkdat.RData")

# KERNEL SMOOTHING------------------------------------------------------------

poly <- region@polygons[[1]]@Polygons[[3]]@coords # isolates coordinates from polygon
library(splancs)

# sensitivity analysis to choose a bandwidth (optional)
library(MASS)
bw.nrd(assaultxy[,2])

# smoothing
smooth <- kernel2d(assaultxy, poly, 1320, 500, 500) # hi res
# smooth <- kernel2d(assaultxy, poly, 5000, 5, 10) # lo res


# save
save(smooth, file="RData/smooth.RData")

# plot (optional)
library(fields)
image.plot(smooth); plot(region, add=TRUE)
# image(smooth); plot(region, add=TRUE)
# image.plot(smooth); plot(park, add=TRUE)

# UPPER THRESHOLD ------------------------------------------------------------------

# isolate matrix of crime density at each pixel
zval <- smooth$z

# set upper threshold for isolating clusters
# e.g. 0.75 isolates the pixels that have the top 25% of crime densities
threshold <- quantile(zval[zval>0], .95, na.rm=TRUE)

# make matrix indicating pixels where crime density exceeds threshold
crimepixels <- zval > threshold

# optional: flips matrix horizontally to match map for visual inspection
# bool <- apply(bool, 1, rev)

# LOWER THRESHOLD ------------------------------------------------------------------

# isolate matrix of crime density at each pixel
zval <- smooth$z

# set lower threshold for isolating clusters
# e.g. .15 isolates the pixels that have the bottom 15% of crime densities
threshold <- quantile(zval, .15, na.rm=TRUE)

# make matrix indicating pixels where crime density is below threshold
crimepixels <- zval < threshold

# optional: flips matrix horizontally to match map for visual inspection
# bool <- apply(bool, 1, rev)

# GROUP ----------------------------------------------------------------------
# source: http://stackoverflow.com/questions/35772846/obtaining-connected-components-in-r

# pixels that are nearby and exceed the threshold are clumped together
# each clump represents a hotspot
# directions=8 includes diagonals, directions=4 does not
# gaps=FALSE numbers and counts clumps without skips
library(raster)
crimeraster <- raster(crimepixels)
plot((crimeraster))
hotspots <- as.matrix(clump(crimeraster, directions=8, gaps=FALSE))

# make list that returns indices of each pixel in each hotspot
numhotspots <- max(hotspots, na.rm=TRUE)
hotspotlist <- vector("list", numhotspots)
for (i in 1:numhotspots){
	hotspotlist[i] <- list(which(hotspots == i, arr.ind = TRUE))
}

# FIND DENSITY MODE OF HOTSPOT-------------------------------------------------
centers <- matrix(NA, length(hotspotlist), 2)
for(i in 1:length(hotspotlist)) {
	zval <- vector()
	for(j in 1:nrow(hotspotlist[[i]])){
		zval[j] <- smooth$z[hotspotlist[[i]][j,1], hotspotlist[[i]][j,2]]
	}
	thismode <- which.max(zval)
	centers[i,1] <- smooth$x[hotspotlist[[i]][thismode,1]]
	centers[i,2] <- smooth$y[hotspotlist[[i]][thismode,2]]
}
centers <- data.frame(centers)
names(centers) <- c("x", "y")

# PLOTTING---------------------------------------------------------------------
# build kde data frame to pass to ggmap
# don't use the raster object of the hotspots for visualization
# instead, use ggmap to directly isolate hotspots from the kde
smoothgrid <- expand.grid(smooth$x, smooth$y)
densvec <- c(smooth$z)
smoothgrid <- data.frame(smoothgrid, densvec)
names(smoothgrid) <- c("x", "y", "z")

library(ggmap)
regionsp <- spTransform(region, CRS("+proj=longlat +datum=WGS84"))
chitown <- get_map(center="Chicago",
		   scale=2,
		   zoom=10,
		   maptype="terrain",
		   source="google")
chimap <- ggmap(chitown, extent="panel") + 
	geom_polygon(aes(x=long, y=lat, group=group),
		     fill="grey",
		     size=0.5,
		     color="black",
		     data=regionsp,
		     alpha=0)
chimap

# DISTANCE FROM HOTSPOT TO PARK------------------------------------------------
source("RScript/DIST.R")
source("RScript/DISTppoly.R")
# each park is a row and each hotspot is a column
# distancematrix <- matrix(data=NA, nrow=length(unique(parkdat$number)), ncol=nrow(centers))
# loop through every park calculating distances to every hotspot
# DISTppoly should place a vector of distances in each row

# DISTPPOLY
distances <- matrix(data=NA, nrow=length(unique(parkdat$parknumber)), ncol=nrow(centers))
for (i in 1:nrow(distances)) {
	distances[i,] <- DISTppoly(pts=as.matrix(centers), poly=as.matrix(parkdat[parkdat$parknumber==i, 1:2]),
						method="Euclidean")
}

# for (i in 573:583) {
#	hist(distances[i,], breaks=25) # distribution of distances to crime hotspots for park number i
# }
# many look bivariate

# MEDIAN DISTANCE TO HOTSPOT---------------------------------------------------
# for each park, take the median of the distances to the hotspots
medians <- data.frame(matrix(data=NA, nrow=nrow(distances), ncol=2))
medians[,1] <- 1:nrow(distances)
names(medians) <- c("parknumber", "medspotdist")
for (i in 1:nrow(distances)) {
	medians[i,2] <- median(distances[i,]) 
}
medians <- medians[order(medians[,2], decreasing=FALSE),] # order parks from closest median distance to hotspot to furthest

# set threshold for distance for within and without then count number of hotspots in and out for each park
# add to poster to see layout

# NEARBY HOTSPOTS--------------------------------------------------------------
# for each park, how many hotspots are within a 5 mile radius
numnearspots <- data.frame(matrix(data=NA, nrow=nrow(distances), ncol=2))
numnearspots[,1] <- 1:nrow(distances)
names(numnearspots) <- c("parknumber", "numnearspots")
radius <- 26400 # 5 mile radius
for (i in 1:nrow(distances)) {
	numnearspots[i,2] <- sum(distances[i,] < radius)
}
numnearspots <- numnearspots[order(numnearspots[,2], decreasing=TRUE),] # order parks from largest number of nearby hotspots to least

parkdat <- merge(parkdat, numnearspots, by="parknumber")
parkdat <- merge(parkdat, medians, by="parknumber")


























