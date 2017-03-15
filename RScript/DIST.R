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
