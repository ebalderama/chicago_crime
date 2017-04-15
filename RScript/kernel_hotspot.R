


#===========================================================
# Distances to violent crimes
#1 meter is 3.28084 feet
#1 meter is 0.00062137119223733386 miles
#===========================================================
dist1 <- distHaversine(church1[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church2[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church3[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church4[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church5[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church6[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)




#===========================================================
# Kernel Density Estimation
#===========================================================
disc.area <- dist1 < 1  #disc radius
pixels <- 50  #resolution for kde2d



getKde <- function(in_df, n=100, lims=c(range(in_df[,1]),range(in_df[,2]))){
  require(MASS)
  #pts <- as.matrix(in_df[,c('lon','lat')])
  pts <- as.matrix(in_df[,1:2])
  dens <- kde2d(pts[,1],pts[,2],n=n,lims=lims)
  #dens$z <- dens$z#/(xyarea/degarea)
  dens_df <- data.frame(expand.grid(dens$x,dens$y),z=c(dens$z))#*length(pts[,1]))
  colnames(dens_df) <- c('x','y','z')
  return(dens_df)
}

library(splancs)
xrange = c(sbox(as.matrix(vcxy[disc.area,]))[1,1], sbox(as.matrix(vcxy[disc.area,]))[2,1])
yrange = c(sbox(as.matrix(vcxy[disc.area,]))[1,2], sbox(as.matrix(vcxy[disc.area,]))[4,2])

londpm <- diff(range(vcll$lon))/diff(range(vcxy$x))
latdpm <- diff(range(vcll$lat))/diff(range(vcxy$y)) #degrees per meter
lonrange <- c(min(vcll$lon[disc.area]) - (min(vcxy$x[disc.area])-xrange[1])*londpm,
              max(vcll$lon[disc.area]) + (xrange[2]-max(vcxy$x[disc.area]))*londpm)
latrange <- c(min(vcll$lat[disc.area]) - (min(vcxy$y[disc.area])-yrange[1])*latdpm,
              max(vcll$lat[disc.area]) + (yrange[2]-max(vcxy$y[disc.area]))*latdpm)





#===========================================================
# data frame for mapping
#===========================================================
densxy.df <- getKde(vcxy[disc.area,], n=pixels, lims=c(xrange,yrange))
densll.df <- getKde(vcll[disc.area,], n=pixels, lims=c(lonrange,latrange))

dens.df <- data.frame(lon=densll.df$x, lat=densll.df$y)
dens.df$density <- densxy.df$z
dens.df$density[densxy.df$z < 1e-9] <- NA
dens.df$rate <- densxy.df$z*sum(disc.area)
dens.df$rate[densxy.df$z < 1e-9] <- NA
#===========================================================



degarea <- diff(lonrange)*diff(latrange)
xyarea <- diff(xrange)*diff(yrange)

sum(disc.area) #total crimes in disc

mean(dens.df$density*sum(disc.area)*xyarea*pi/4, na.rm=T) #should be about n
mean(dens.df$density, na.rm=T)*xyarea*pi/4 #should be about 1
sum(dens.df$density, na.rm=T)*xyarea/pixels^2 #should be about 1









#===========================================================
# Maps
#===========================================================


## Church 1
ggmap(Sabina) + 
  geom_point(aes(lon, lat), data = church1, col=2, size=2) +
  geom_point(aes(lon, lat), data = church1, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Sabina", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Sabina_kern.png")

## Church 2
ggmap(Benedict) + 
  geom_point(aes(lon, lat), data = church2, col=2, size=2) +
  geom_point(aes(lon, lat), data = church2, alpha=.4, col=2, size=7) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","","high")) +
  ggtitle("St. Benedict the African", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Benedict_kern.png")

## Church 3
ggmap(Immaculate) + 
  geom_point(aes(lon, lat), data = church3, col=2, size=2) +
  geom_point(aes(lon, lat), data = church3, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","","low","","high","")) +
  ggtitle("Immaculate Conception", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Immaculate_kern.png")

## Church 4
ggmap(Michael) + 
  geom_point(aes(lon, lat), data = church4, col=2, size=2) +
  geom_point(aes(lon, lat), data = church4, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Michael the Archangel", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Michael_kern.png")


## Church 5
ggmap(Agatha) + 
  geom_point(aes(lon, lat), data = church5, col=2, size=2) +
  geom_point(aes(lon, lat), data = church5, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Agatha", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Agatha_kern.png")

## Church 6
ggmap(Martin) + 
  geom_point(aes(lon, lat), data = church6, col=2, size=2) +
  geom_point(aes(lon, lat), data = church6, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","","","high","")) +
  ggtitle("St. Martin de Porres", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Martin_kern.png")





