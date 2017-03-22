##Distance between 2 sets of points

DIST <- function(a,b,radius=6378.135,method=c("Haversine","Euclidean"),squared=F){
	a <- matrix(a,ncol=2)
	b <- matrix(b,ncol=2)
	method <- match.arg(method)
    switch(method,Haversine={
    	d <- radius * 2*asin(sqrt( sin(outer(a[,2],b[,2],"-")*pi/360)^2 + outer(cos(a[,2]*pi/180),cos(b[,2]*pi/180))*sin(outer(a[,1],b[,1],"-")*pi/360)^2 )) #kilometers
    	if(squared) d <- d^2
    },Euclidean={
    	d <- outer(a[,1],b[,1],"-")^2 + outer(a[,2],b[,2],"-")^2
    	if(!squared) d <- sqrt(d)
    })
    return(d)
}

##Shortest distance to a polygon from a set of points
##using Haversine distance or Euclidean distance
DISTppoly <- function(pts,poly,method=c("Haversine","Euclidean")){

	require(splancs)
	p <- matrix(pts,ncol=2)
	l <- cbind(poly,rbind(poly[-1,],poly[1,]))
	l <- as.matrix(l)
	
	np <- nrow(p)
    nl <- nrow(l)
    xp <- p[,1]
    yp <- p[,2]

    #distance to 1st vertex
    d1 <- DIST(p,l[,1:2],method=method)
    
    #distance to 2nd vertex
    d2 <- DIST(p,l[,3:4],method=method)
    
    dx <- l[,3]-l[,1]
    dy <- l[,4]-l[,2]
    v <- cbind(dx,dy)
    #projection points
    q <- matrix(NA,nl,2)
    #d3 <- matrix(NA,np,nl)
    for(i in 1:np){
    	for(j in 1:nl){
    		V <- (v[j,]%*%t(v[j,]))/sum(v[j,]^2)
    		q[j,] <- c(V%*%(p[i,]-l[j,1:2]) + l[j,1:2])
    	}
    	
    	#distance to projection
    	#d3[i,] <- DIST(t(p[i,]),q,method=method)
    }
    	d3 <- DIST(p,q,method=method)
    
    leng <- sqrt(dx^2 + dy^2) #DIST(l[,1:2],l[,3:4],method="Euclidean")
    co <- dx/leng
    si <- dy/leng
    co <- matrix(co, nrow = np, ncol = nl, byrow = TRUE)
    si <- matrix(si, nrow = np, ncol = nl, byrow = TRUE)
    xp.x1 <- outer(xp, l[, 1], "-")
    yp.y1 <- outer(yp, l[, 2], "-")
    xpr <- xp.x1 * co + yp.y1 * si
    lenf <- matrix(leng, nrow = np, ncol = nl, byrow = TRUE)
    zero <- (lenf < .Machine$double.eps)
    outside <- (zero | xpr < 0 | xpr > lenf)
    if(any(outside)) d3[outside] <- Inf
    
    #d <- matrix(pmin(d1,d2,d3),np,nl)
    d <- apply(pmin(d1,d2,d3),1,min)
    d[inout(p,poly)] <- 0
    return(d)
}


if(F){
	
	p <- cbind(1:3,rep(3,3))

	l <- rbind(c(2,2,5,2),c(5,2,5,4),c(5,4,2,4),c(2,4,2,2))
	DISTppoly(p,l[,1:2],method="Eucl")
	
	#l <- as.matrix(expand.grid(c(2,5),c(2,4)))
	l <- rbind(c(2,2),c(5,2),c(5,4),c(2,4))
	DISTppoly(p,l,method="Eucl")
	
	point.in.polygon(p[,1],p[,2],l[,1],l[,2])
	inout(p,l)

	ls <- SpatialPoints(l)
	DISTppoly(p,coordinates(ls),method="Eucl")


	plot(p,xlim=c(0,6),ylim=c(0,6))
	lines(l)
	lines(rbind(l[,3:4],l[1:2,1:2]))
	
	
	DISTppoly(p,l,method="Have")
	DISTppoly(p,l,method="Eucl")

}
