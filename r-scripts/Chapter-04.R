###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 03, 2010
### Chapter 04: Exploration of Multivariate Data: Comparing Two Groups
###################################################



###################################################
### Command Snippet 4.1: Read in the VLSS per capita data
###################################################

household <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(household)
tail(household)
str(household)
summary(household)



###################################################
### Command Snippet 4.2: Plot the Density for Expenditures
###################################################

plot(density(household$Dollars, 
	xlab = "U.S. Dollars", 
	main = " ")





###################################################
### Command Snippet 4.3: Indexing 
###################################################

household[1, 1]
household[1, ]
household[ , 1]
household$Dollars[1]





###################################################
### Command Snippet 4.4: Indexing Using a Logical Expression 
###################################################

rural.households <- household$Dollars[household$Area == "Rural"]
urban.households <- household$Dollars[household$Area == "Urban"]





###################################################
### Command Snippet 4.5: Plotting the Estimated Density for the Conditional Distributions 
###################################################

d.rural <- density(rural.households)
plot(d.rural, lty = "solid",  
	xlab = "Household Per Capita Expenditures (in U.S. Dollars)", 
	main = " ", bty = "l")
d.urban <- density(urban.households)
lines(d.urban, lty = "dotted")





###################################################
### Command Snippet 4.6: Side-by-Side Box and Whiskers Plots
###################################################

boxplot(rural.households, urban.households, 
	names = c("Rural","Urban") ) 





###################################################
### Command Snippet 4.7: Numerical Summaries of Central Tendency
###################################################

median(household$Dollars)
mean(household$Dollars)



###################################################
### Command Snippet 4.8: Conditional Numerical Summaries of Central Tendency
###################################################

tapply(X = household$Dollars, INDEX = household$Area, FUN = mean)





###################################################
### Command Snippet 4.9: Numerical Summaries of Variation
###################################################

var(household$Dollars)
sd(household$Dollars)
tapply(X = household$Dollars, INDEX = household$Area, FUN = var)
tapply(X = household$Dollars, INDEX = household$Area, FUN = sd)





###################################################
### Command Snippet 4.10: Standard Error of the Mean
###################################################

numerator <- tapply(X = household$Dollars, INDEX = household$Area, FUN = sd)
denominator <- sqrt(table(household$Area))
numerator / denominator





###################################################
### Command Snippet 4.11: Measuring Skewness
###################################################

library(e1071) #Needs to be installed
skewness(household$Dollars, type = 2)
tapply(X = household$Dollars, INDEX = household$Area, FUN = skewness, type = 2)





###################################################
### Command Snippet 4.12: Measuring Kurtosis
###################################################

kurtosis(household$Dollars, type = 2)
tapply(X = household$Dollars, INDEX = household$Area, FUN = kurtosis, type = 2)





###################################################
### Command Snippet 4.13: Plot for Publication
###################################################

plot(d.rural, lty = "solid", 
	xlab = "Household Per Capita Expenditures (in U. S. Dollars)", 
	main = " ", bty = "l")
lines(d.urban, lty = "dotted")
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4)





###################################################
### Command Snippet 4.14: Plot for Publication Using Color
###################################################

plot(d.rural, lty = "solid"
col = rgb(red = 139, green = 0, blue = 0, maxColorValue = 255),
	xlab = "Household Per Capita Expenditures (in U.S. Dollars)", 
	main = " ", bty = "l")
lines(d.urban, lty = "solid",
	col = rgb(red = 0, green = 0, blue = 139, maxColorValue = 255) )
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4)




###################################################
### Command Snippet 4.15: Plot for Publication Using Color and Shading
###################################################

plot(d.rural,  
	xlab = "Household Per Capita Expenditures (in U.S. Dollars)", 
	main = " ", bty = "l", type = "n")
polygon(d.rural, lty = "solid",
	col = rgb(red = 139, green = 0, blue = 0, alpha = 100, maxColorValue = 255) )
polygon(d.urban, lty = "dotted",
	col = rgb(red = 0, green = 0, blue = 139, alpha = 100, maxColorValue = 255) )
text(x = 182, y = 0.0059, labels = "Urban Households", pos = 4)
text(x = 495, y = 0.0012, labels = "Rural Households", pos = 4)





###################################################
### Command Snippet 4.16: Locator( ) Function
###################################################

text(locator(1), labels = "Rural Households", pos = 4)





###################################################
### Command Snippet 4.17: Color Palettes
###################################################

library(RColorBrewer) #Needs to be installed
brewer.pal(n = 2, name = "Set1")
library(colorspace) #Needs to be installed
hex2RGB(brewer.pal(n = 3, name = "Set1"))





###################################################
### Command Snippet 4.18: Robust Estimation - The Trimmed Mean 
###################################################

mean(household$Dollars, tr = 0.2)
tapply(X = household$Dollars, INDEX = household$Area, FUN = mean, tr=.2)
median(household$Dollars)
tapply(X = household$Dollars, INDEX = household$Area, FUN = median)





###################################################
### Command Snippet 4.19: Robust Estimation - Winsorized Variance and Standard Deviation
###################################################

library(WRS)
winvar(household$Dollars, tr = 0.2)
tapply(X = household$Dollars, INDEX = household$Area, FUN = winvar, tr=.2)
sqrt(tapply(X = household$Dollars, INDEX = household$Area, FUN = winvar, tr=.2))



