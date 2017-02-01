#==============================================================================
# POINT PROCESS MODELLING
#==============================================================================

# LOAD FILES
load("RData/assaultxy.RData")
load("RData/region.RData")
load("RData/park.RData")

# SMOOTHING
poly <- region@polygons[[1]]@Polygons[[3]]@coords
library(splancs)
smoothcoarse <- kernel2d(assaultxy, poly, 10000, 200, 200)
smoothfine <- kernel2d(assaultxy, poly, 1000, 200, 200)
library(fields)
image.plot(smoothcoarse)
image.plot(smoothfine)
image.plot(smoothcoarse); plot(park, add=TRUE)
image.plot(smoothfine); plot(park, add=TRUE)






