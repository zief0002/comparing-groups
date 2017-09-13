###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 03, 2010
### Chapter 03: Data Exploration: One Variable
###################################################





###################################################
### Command Snippet 3.1: Read in and Examine the VLSS Age Data
###################################################

vlss <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(vlss)
tail(vlss)
str(vlss)
summary(vlss)





###################################################
### Command Snippet 3.2: Estimate, Print and Plot the Density for the Age Distribution
###################################################

d <- density(vlss$Age)
d
plot(d)





###################################################
### Command Snippet 3.3: Changing the Smoothing Parameter
###################################################

d.SJ <- density(vlss$Age, bw = "SJ")
plot(d.SJ)

library(quantreg)  # This package needs to be installed
d.akd <- akj(x = vlss$Age, z = d$x)
plot(x = d$x, y = d.akd$dens, type = "l")





###################################################
### Command Snippet 3.4: Plot for Publication 
###################################################

plot(x = d$x, y = d.akd$dens, type="l", 
	xlab = "Age (in years)", ylab = "Density")

plot(x = d$x, y = d.akd$dens,  
	xlab = "Age (in years)", ylab = "Density", 
	main = " ", type = "l")

plot(x = d$x, y = d.akd$dens,  
	xlab = "Age (in years)", ylab = "Density", 
	main = " ", type = "l", bty = "l")





###################################################
### Command Snippet 3.5: Producing Variability Bands
###################################################

set.seed(100)
age.sample <- sample(vlss$Age, n = 1000, replace = FALSE)
library(sm)
sm.density(age.sample, h = 4, 
	xlab = "Age", ylab = "Density", 
	xlim=c(-7,106), ylim=c(0, 0.03), 
	display="se", rugplot = FALSE)



