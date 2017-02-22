# POINT PROCESS MODELLING======================================================

# LOAD FILES
	# LOAD-------------------------------------------------------------------------
load("RData/assaultxy.RData")
load("RData/region.RData")
load("RData/park.RData")

	# KERNEL SMOOTHING-------------------------------------------------------------
poly <- region@polygons[[1]]@Polygons[[3]]@coords # isolates the coordinates from the polygon
library(splancs)
# eventually do a sensitivity analysis to choose a bandwidth
smooth <- kernel2d(assaultxy, poly, 1000, 200, 200)
library(fields)
image.plot(smooth); plot(park, add=TRUE)

	# POINT PROCESS (NOT USED)-----------------------------------------------------
#library(spatstat)
#glass <- data.frame(x=rev(poly[,1]), y=rev(poly[,2]))
#windowpane <- owin(poly=glass)
#assaultpp <- as.ppp(assaultxy, windowpane)

	# CONGLOMERATE Z VALUES (NOT USED)---------------------------------------------
#zval <- vector()
#for (i in 1:100000) {
#	xcoord <- which(smoothfine$x > assaultxy[i,1])[1]
#	ycoord <- which(smoothfine$y > assaultxy[i,2])[1]
#	zval[i] <- smoothfine$z[xcoord,ycoord]
#}

# CALCULATE RETENTION PROBABILITY
	# RETENTION PROBABILITY--------------------------------------------------------
totalassaults <- nrow(assaultxy) # total number of assaults
library(rgeos)
chicagoarea <- gArea(region, byid=FALSE) # area of Chicago in sq ft
pixelarea <- (smooth$x[2] - smooth$x[1]) * (smooth$y[2] - smooth$y[1]) # area of one pixel
chicagopixels <- chicagoarea/pixelarea # number of pixels that make up Chicago
lambda = smooth$z*totalassaults/chicagopixels # estimated true intensity from density for each pixel
rho = matrix(nrow=nrow(lambda), ncol=ncol(lambda)) # desired intensity for near-CSR after thinning
for(i in 1:200) {
	for (j in 1:200) {
		rho[i,j] <- 10
	}
}

	# ASSOCIATE EACH CRIME WITH ITS LAMBDA-----------------------------------------
lambdacol <- matrix(ncol=1, nrow=nrow(assaultxy)) # temporary column to hold lambda values
assaultlambda <- cbind(assaultxy, lambdacol) # cbind assaultxy and lambdacol
colnames(assaultlambda)[3] <- "lambda" # rename lambdacol
for (i in 1:nrow(assaultlambda)) {
	xcoord <- which(smooth$x > assaultlambda[i,1])[1] # temp holds first xcoord in smooth$x that's > assault's xcoord
	ycoord <- which(smooth$y > assaultlambda[i,2])[1] # temp holds first ycoord in smooth$y that's > assault's ycoord
	assaultlambda[i,3] <- lambda[xcoord, ycoord] # insert associated lambda value into matrix
}
save(assaultlambda, file="RData/assaultlambda.RData")

	# THINNING OF POINT PROCESS (IN PROGRESS)--------------------------------------
library(spatstat)
thin <- rthin(assaultpp, retprob)
plot(thin)
p <- (1/smoothfine$z)
p <- ifelse(p==Inf, 1, p)
smoothfine2 <- smoothfine
smoothfine2$z <- p
image.plot(smoothfine2)