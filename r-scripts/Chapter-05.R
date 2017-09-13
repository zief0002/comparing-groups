###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 03, 2010
### Chapter 05: Exploration of Multivariate Data: Comparing Many Groups
###################################################

###################################################
### Command Snippet 5.1: Read in and Examine the VLSS per capita data.
###################################################
household <- read.table(file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(household)
tail(household)
str(household)
summary(household)
plot(density(household$Dollars), xlab = "Dollars", main = " ")


###################################################
### Command Snippet 5.2: Indexing to Create Region Subsets 
###################################################
central.coast <- household$Dollars[household$Region == 1]
central.highlands <- household$Dollars[household$Region == 2]
mekong.delta <- household$Dollars[household$Region == 3]
north.coast <- household$Dollars[household$Region == 4]
northern.uplands <- household$Dollars[household$Region == 5]
red.river.delta <- household$Dollars[household$Region == 6]
south.east <- household$Dollars[household$Region == 7]


###################################################
### Command Snippet 5.3: Plotting Densities 
###################################################
library(RColorBrewer)
brewer.pal(n = 7, name = "Set1")
plot(density(central.coast), main = " ", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), col = "#E41A1C")
lines(density(central.highlands), col = "#377EB8")
lines(density(mekong.delta), col = "#4DAF4A")
lines(density(north.coast), col = "#984EA3")
lines(density(northern.uplands), col = "#FF7F00")
lines(density(red.river.delta), col = "#FFFF33")
lines(density(south.east), col = "#A65628")
legend(x = 2000, y = 0.0075, legend = c("Central Coast", "Central Highlands", "Mekong Delta", "North Coast", "Northern Uplands", "Red River Delta", "South East"), lty="solid", col = c("#E41A1C", "#FFFF33", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#377EB8"))


###################################################
### Command Snippet 5.4: Panel Plots 
###################################################
par(mfrow=c(4, 2))
my.bty <- "l"
my.xlab <- "Household Per Capita Expenditures (in U.S. Dollars)"
my.xlim <- c(0, 3100)
my.ylim <- c(0, 0.008)
plot(density(central.coast), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(central.highlands), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(mekong.delta), main = "Mekong Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(north.coast), main = "North Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(northern.uplands), main = "Northern Uplands", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(red.river.delta), main = "Red River Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
plot(density(south.east), main = "South East", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
par(mfrow = c(1, 1))


###################################################
### Command Snippet 5.5: Side-by-Side Box and Whiskers Plot
###################################################
boxplot(central.coast, central.highlands, mekong.delta, north.coast, northern.uplands, red.river.delta, south.east, names = c("Central Coast", "Central Highlands", "Mekong Delta", "North Coast", "Northern Uplands", "Red River Delta", "South East"))


###################################################
### Command Snippet 5.6: Robust Estimates
###################################################
tapply(X = household$Dollars, INDEX = household$Region, FUN = mean, tr = 0.2)
library(WRS)
sqrt(tapply(X = household$Dollars, INDEX = household$Region, FUN = winvar, tr = 0.2))
table(household$Region)


###################################################
### Command Snippet 5.7: Adding a Line to Each Panel
###################################################
par(mfrow=c(4, 2))
plot(density(central.coast), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(central.highlands), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(mekong.delta), main = "Mekong Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(north.coast), main = "North Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(northern.uplands), main = "Northern Uplands", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(red.river.delta), main = "Red River Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
plot(density(south.east), main = "South East", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
abline(v = 119, col = "red", lty = "dashed")
par(mfrow = c(1, 1))


###################################################
### Command Snippet 5.8: More Indexing to Create Subsets
###################################################
central.coast.urban <- household$Dollars[household$Region == 1 & household$Area == "Urban"]
central.coast.rural <- household$Dollars[household$Region == 1 & household$Area == "Rural"]
central.highlands.urban <- household$Dollars[household$Region == 2 & household$Area == "Urban"]
central.highlands.rural <- household$Dollars[household$Region == 2 & household$Area == "Rural"]
mekong.delta.urban <- household$Dollars[household$Region == 3 & household$Area == "Urban"]
mekong.delta.rural <- household$Dollars[household$Region == 3 & household$Area == "Rural"]
north.coast.urban <- household$Dollars[household$Region == 4 & household$Area == "Urban"]
north.coast.rural <- household$Dollars[household$Region == 4 & household$Area == "Rural"]
northern.uplands.urban <- household$Dollars[household$Region == 5 & household$Area == "Urban"]
northern.uplands.rural <- household$Dollars[household$Region == 5 & household$Area == "Rural"]
red.river.delta.urban <- household$Dollars[household$Region == 6 & household$Area == "Urban"]
red.river.delta.rural <- household$Dollars[household$Region == 6 & household$Area == "Rural"]
south.east.urban <- household$Dollars[household$Region == 7 & household$Area == "Urban"]
south.east.rural <- household$Dollars[household$Region == 7 & household$Area == "Rural"]


###################################################
### Command Snippet 5.9: Creating Density Plots for the Subset
###################################################
par(mfrow=c(4, 2))
plot(density(central.coast.rural), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(central.coast.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(central.highlands.rural), main = "Central Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(central.highlands.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(mekong.delta.urban), main = "Mekong Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(mekong.delta.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(north.coast.rural), main = "North Coast", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(north.coast.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(northern.uplands.rural), main = "Northern Uplands", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(northern.uplands.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(red.river.delta.rural), main = "Red River Delta", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(red.river.delta.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
plot(density(south.east.rural), main = "South East", xlab = my.xlab,  bty = my.bty, xlim = my.xlim, ylim = my.ylim)
lines(density(south.east.urban), lty = "dotted")
abline(v = 119, col = "red", lty = "dashed")
par(mfrow = c(1, 1))

###################################################
### Command Snippet 5.10: Numerical Summaries Conditioned on Region and Area
###################################################
tapply(X = household$Dollars, INDEX = list(household$Region, household$Area), FUN = mean, tr = 0.2)
sqrt(tapply(X = household$Dollars, INDEX = list(household$Region, household$Area), FUN = winvar, tr = 0.2))
table(household$Region, household$Area)


###################################################
### Command Snippet 5.11: Read in and Examine the NELS Data
###################################################
nels <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(nels)
tail(nels)
str(nels)
summary(nels)
plot(density(nels$Achieve), main = " ", xlab = "Mathematics Achievement")


###################################################
### Command Snippet 5.12: Determine Levels of Homework, Create Subsets for Each Level, and Plot the Side-by-Side Box and Whiskers Plots
###################################################
table(nels$Homework)
zero <- nels$Achievement[nels$Homework == 0]
one <- nels$Achievement[nels$Homework == 1]
two <- nels$Achievement[nels$Homework == 2]
three <- nels$Achievement[nels$Homework == 3]
four <- nels$Achievement[nels$Homework == 4]
five <- nels$Achievement[nels$Homework == 5]
six <- nels$Achievement[nels$Homework == 6]
seven <- nels$Achievement[nels$Homework == 7]
ten <- nels$Achievement[nels$Homework == 10]
boxplot(zero, one, two, three, four, five, six, seven, ten, at = c(0:7, 10), xlim = c(-0.4, 10.4))


###################################################
### Command Snippet 5.13: Scatterplot of the Conditional Distributions
###################################################
plot(x = nels$Homework, y = nels$Achievement, xlab = "Average Weekly Time Spent on Mathematics Homework (in Hours)", ylab = "Mathematics Achievement")


###################################################
### Command Snippet 5.14: Scatterplot and Side-by-Side Box and Whiskers Plots of the Conditional Distributions
###################################################
plot(x = nels$Homework, y = nels$Achievement, xlab = "Average Weekly Time Spent on Mathematics Homework (in Hours)", ylab = "Mathematics Achievement", xlim = c(-0.4, 10.4), pch = 20)
boxplot(zero, one, two, three, four, five, six, seven, ten, at = c(0:7, 10), add = TRUE, axes = FALSE, boxwex = 0.4, col = rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.3))


