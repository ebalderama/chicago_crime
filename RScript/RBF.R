# RADIAL BASIS FUNCTION===============================================================

	# LOAD------------------------------------------------------------------------
#library(RSNNS)
library(plotly)
load("RData/assaultxy.RData")
load("RData/park.RData")
summary(park)

	# PLOTS-----------------------------------------------------------------------
#p <- plot_ly(z = ~volcano) %>% add_surface()
#kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
#p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()











	# IDEAS-------------------------------------------------------------------------------

# Parks may be centers (input layer?), a function (gaussian?) may be the
# hidden layer, and the crime locations may be the expected output layer
# The weight from the input to the hidden layer may be given by some function
# The weight from the hidden layer to the output may be given by some linear
# function.



# OUTPUT: density of crime
# INPUT: distance from park
# CENTER: park



# Using plotly to 3D hist the data
# Looking at the hist of distances from park to crimes