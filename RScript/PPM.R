#==============================================================================
# POINT PROCESS MODELLING
#==============================================================================

# WORKING DIRECTORY
getwd()
setwd("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory")

# LOAD CHECKPOINT
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/assaultxy.RData")
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/region.RData")
load("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/park.RData")

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






