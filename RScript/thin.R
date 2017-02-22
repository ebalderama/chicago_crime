#Maybe ignore that last email. Here's the other code I use, where first I
#pass the points that need to get thinned to the thin.c function. And then
#I pass the function for lambda to the super.f function to do the
#superposition. And then after that you would just combine the two results.
#Here, "min" and "max" is just equal to k (or b or whatever your cutoff
#point for super-thinning is). "fun" is the conditional intensity function.
#"data" for thin.c is just the points that need to be thinned. "data" for
#super.f are just the points that are automatically kept. Hope this helps.


thin.c <- function(data, min, fun)
{
  prob <- min/fun(data[,1], data[,2])
  u <- runif(length(prob))
  retain <- (u<=prob)
  thinres <- data[retain,]
  return(thinres)
}


super.f <- function(data, max, fun)
{
	temp <- rpoispp(max)
	temp.x <- temp$x; temp.y<-temp$y; temp.l<-fun(temp.x, temp.y)
	temp.data <- data.frame(cbind(temp.x, temp.y, temp.l))
	names(temp.data) <- c("x","y","lambda")
	prob <- (max-temp.l)/temp.l
	u <- runif(length(prob))
	retain <- (u <= prob)
	thinres <- temp.data[retain,]
	total.pts <- data.frame(rbind(thinres, data))
	return(total.pts)
}


