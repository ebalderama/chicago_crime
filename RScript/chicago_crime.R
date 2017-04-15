# CHICAGO CRIME################################################################

	# PARKS SHAPEFILES=============================================================
	library(rgdal)
	library(broom)
	library(maptools)
	park <- readOGR(dsn="./Shapefiles/Parks_Aug2012", layer="Parks_Aug2012")
	save(park, file="RData/park.RData")
	parkdat <- tidy(park, region="PARK")
	parkdat$parknumber <- as.integer(factor(parkdat$id))
	# save(parkdat, file="RData/parkdat.RData")

	# CRIME DATA PROJECTION========================================================
	load("RData/violentdat.RData")
	load("RData/park.RData")
	violentll <- na.omit(cbind(violentdat$Longitude, violentdat$Latitude))
	colnames(violentll) <- c("longitude", "latitude")
	llproj <- '+init=epsg:4269'
	violentsp <- SpatialPoints(na.omit(violentll), proj4string=CRS(llproj))
	library(sp)
	violentmerc <- spTransform(violentsp, CRS(proj4string(park)))
	violentxy <- coordinates(violentmerc)
	# save(violentxy, file="RData/violentxy.RData")

	# REGION POLYGON=============================================
	# modified from: https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/

		# READ AND MODIFY SHAPEFILES---------------------------------------------------
		library(rgdal)
		region <- readOGR(dsn="Shapefiles/CommAreas", layer="CommAreas")
		plot(region)
		lookup <- data.frame()
		lookup <- rbind(lookup, region@data)
		lookup$AREA_NUMBE <- as.character(lookup$AREA_NUMBE)
		lookup$COMMUNITY <- as.character(lookup$COMMUNITY)
		lookup$CITY <- NA
		lookup$CITY <- "Chicago"

		# MERGE------------------------------------------------------------------------
		region@data$AREA_NUMBE <- as.character(region@data$AREA_NUMBE)
		library(dplyr)
		region@data <- full_join(region@data, lookup, by="AREA_NUMBE")
		row.names(region) <- row.names(region@data)
		region <- spChFIDs(region, row.names(region))

		# DISSOLVE---------------------------------------------------------------------
		library(rgeos)
		region <- gUnaryUnion(region, id=region@data$country)

		# MODIFY DATAFRAME-------------------------------------------------------------
		row.names(region) <- as.character(1:length(region))
		lookup <- unique(lookup$CITY)
		lookup <- as.data.frame(lookup)
		colnames(lookup) <- "CITY"
		region <- SpatialPolygonsDataFrame(region, lookup)
		save(region, file="RData/region.RData")

	# ANALYSIS=====================================================================

		# LOAD-------------------------------------------------------------------------
		load("RData/violentxy.RData")
		load("RData/region.RData")
		load("RData/parkdat.RData")

		# KERNEL SMOOTHING------------------------------------------------------------
		poly <- region@polygons[[1]]@Polygons[[3]]@coords # isolates coordinates
		library(splancs)
		library(fields)
		# perform with 1/8 1/4 1/2 1 mile (660, 1320, 2640, 5280 feet) bandwidths
		load(file="RData/violentxy.RData")
		
		# EIGHTH MILE
		eighthsmooth <- kernel2d(violentxy, poly, 660, 2000, 2000)
		png(file="Graphics/eighthsmooth.png", width=2000, height=2000)
		image(eighthsmooth, col=tim.colors(), axes=FALSE, bty="n")
		plot(region, add=TRUE, lwd=10)
		box(which="plot", lty="solid", lwd=10)
		dev.off()
		
		# QUARTER MILE
		quartersmooth <- kernel2d(violentxy, poly, 1320, 200, 200)
		#png(file="Graphics/quartersmooth.png", width=2000, height=2000)
		image(quartersmooth, col=tim.colors(), axes=FALSE, bty="n")
		plot(region, add=TRUE, lwd=10)
		box(which="plot", lty="solid", lwd=10)
		#dev.off()
		
		# HALF MILE
		halfsmooth <- kernel2d(violentxy, poly, 2640, 2000, 2000)
		png(file="Graphics/halfsmooth.png", width=2000, height=2000)
		image(halfsmooth, col=tim.colors(), axes=FALSE, bty="n")
		plot(region, add=TRUE, lwd=10)
		box(which="plot", lty="solid", lwd=10)
		dev.off()
		
		# WHOLE MILE
		wholesmooth <- kernel2d(violentxy, poly, 5280, 2000, 2000)
		png(file="Graphics/wholesmooth.png", width=2000, height=2000)
		image(wholesmooth, col=tim.colors(), axes=FALSE, bty="n")
		plot(region, add=TRUE, lwd=10)
		box(which="plot", lty="solid", lwd=10)
		dev.off()

		# UPPER THRESHOLD ------------------------------------------------------------------
		# isolate matrix of crime density at each pixel
		# zval <- smooth$z
		# set upper threshold for isolating clusters
		# e.g. 0.95 isolates the pixels that have the top 5% of crime densities
		# threshold <- quantile(zval[zval>0], .95, na.rm=TRUE)
		# make matrix indicating pixels where crime density exceeds threshold
		# crimepixels <- zval > threshold
		
		# EIGHTH MILE
		eighthzval <- c(eighthsmooth$z)
		eighththreshold <- quantile(eighthzval[eighthzval>0], .95, na.rm=TRUE)
		eighthpixels <- eighthzval > eighththreshold
		
		# QUARTER MILE
		quarterzval <- c(quartersmooth$z)
		quarterthreshold <- quantile(quarterzval[quarterzval>0], .95, na.rm=TRUE)
		quarterpixels <- quarterzval > quarterthreshold
		
		# HALF MILE
		halfzval <- c(halfsmooth$z)
		halfthreshold <- quantile(halfzval[halfzval>0], .95, na.rm=TRUE)
		halfpixels <- halfzval > halfthreshold
		
		# WHOLE MILE
		wholezval <- c(wholesmooth$z)
		wholethreshold <- quantile(wholezval[wholezval>0], .95, na.rm=TRUE)
		wholepixels <- wholezval > wholethreshold

		# GROUP ----------------------------------------------------------------------
		# source: http://stackoverflow.com/questions/35772846/obtaining-connected-components-in-r
		# pixels that are nearby and exceed the threshold are clumped together
		# each clump represents a hotspot
		# directions=8 includes diagonals, directions=4 does not
		# gaps=FALSE numbers and counts clumps without skips
		library(raster)
		# crimeraster <- raster(crimepixels)
		# image(t(flip(crimeraster,1)), col=c("gray", "red"),
		#       axes=FALSE, xlab="", ylab="")
		# box(which = "plot", lty = "solid")
		
		# EIGHTH MILE
		eighthraster <- raster(eighthpixels)
		png(file="Graphics/eighthraster.png", width=2000, height=2000)
		image(t(flip(eighthraster,1)), col=c("gray20", "firebrick1"),
		      axes=FALSE, xlab="", ylab="", bty="n")
		box(which = "plot", lty = "solid", lwd=10)
		dev.off()
		
		# QUARTER MILE
		quarterraster <- raster(quarterpixels)
		png(file="Graphics/quarterraster.png", width=2000, height=2000)
		image(t(flip(quarterraster,1)), col=c("gray20", "firebrick1"),
		      axes=FALSE, xlab="", ylab="", bty="n")
		box(which = "plot", lty = "solid", lwd=10)
		dev.off()
		
		# HALF MILE
		halfraster <- raster(halfpixels)
		png(file="Graphics/halfraster.png", width=2000, height=2000)
		image(t(flip(halfraster,1)), col=c("gray20", "firebrick1"),
		      axes=FALSE, xlab="", ylab="", bty="n")
		box(which = "plot", lty = "solid", lwd=10)
		dev.off()
		
		# WHOLE MILE
		wholeraster <- raster(wholepixels)
		png(file="Graphics/wholeraster.png", width=2000, height=2000)
		image(t(flip(wholeraster,1)), col=c("gray20", "firebrick1"),
		      axes=FALSE, xlab="", ylab="", bty="n")
		box(which = "plot", lty = "solid", lwd=10)
		dev.off()
		
		# image(t(flip(x, 1)))
		# image(t(flip(x, 2)))
		# plotRGB(t(flip(x, 1)))
		# plotRGB(t(flip(x, 2)))
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

		# DISTANCE FROM PARK TO HOTSPOTS-----------------------------------------------
		source("RScript/DIST.R")
		source("RScript/DISTppoly.R")
		# each park is a row and each hotspot is a column
		# loop through every park calculating distances to every hotspot
		# DISTppoly should place a vector of distances in each row
		distances <- matrix(data=NA,
				    nrow=length(unique(parkdat$parknumber)),
				    ncol=nrow(centers))
		for (i in 1:nrow(distances)) {
			distances[i,] <- DISTppoly(pts=as.matrix(centers),
						   poly=as.matrix(parkdat[parkdat$parknumber==i, 1:2]),
						   method="Euclidean")
		}

		# MINIMUM DISTANCE TO HOTSPOT------------------------------------------------
		mymins <- data.frame(parknumber=1:nrow(distances))
		mymins$mins <- apply(distances, 1, min)
		mymins <- mymins[order(mymins[,2], decreasing=FALSE),] # order parks from closest minimum distance to hotspot to furthest
		parkdat <- merge(parkdat, mymins, by="parknumber")
		parksort <- parkdat[order(parkdat[,9], decreasing=TRUE),]
		parkmins <- unique(parksort[,c(1,8,9)])
		parkmins$mins <- (parkmins$mins/5280)
		
		# PLOTTING GRAPHS OF PARKS---------------------------------------------------
		quartersmooth <- kernel2d(violentxy, poly, 1320, 100, 100)
		quarterzval <- c(quartersmooth$z)
		quarterframe <- data.frame(expand.grid(quartersmooth$x, quartersmooth$y))
		names(quarterframe) <- c("x", "y")
		quarterframe$z <- quarterzval
		quarterframe$hotspot <- ifelse(quarterframe$z > quarterthreshold, TRUE, FALSE)
		library(broom)
		library(ggplot2)
		ggplot() +
			geom_raster(aes(x, y),
				    fill="gray",
				    data=subset(quarterframe, !quarterframe$hotspot)) +
			geom_polygon(aes(long, lat, group=group),
				     fill="green",
				     colour=1,
				     size=.01,
				     data=parkdat) +
			geom_raster(aes(x, y),
				    fill="red",
				    alpha=.3,
				    data=subset(quarterframe, quarterframe$hotspot)) +
			labs(x="", y="") +
			theme(legend.position="none",
			      axis.text=element_blank(),
			      panel.background=element_blank(),
			      axis.ticks=element_blank())
			
		
		
		
		#ggsave("Graphics/")
		
	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
