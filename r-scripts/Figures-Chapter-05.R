##################################################
### Read in All Data Sets 
###################################################
household <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
nels <- read.table(file.choose(), header = TRUE, sep = ",", row.names = "ID")


###################################################
### Figure 5-1 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-1.pdf")
plot(density(household$Dollars[household$Region == 1]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), col = "#E41A1C", las = 1, cex.axis = 0.7, cex.lab = 0.7)  ## Central Coast
lines(density(household$Dollars[household$Region == 2]), col = "#377EB8")  ## Central Highlands
lines(density(household$Dollars[household$Region == 3]), col = "#4DAF4A")  ## Mekong Delta
lines(density(household$Dollars[household$Region == 4]), col = "#984EA3")  ## North Coast
lines(density(household$Dollars[household$Region == 5]), col = "#FF7F00")  ## Northern Uplands
lines(density(household$Dollars[household$Region == 6]), col = "#FFFF33")  ## Red River Delta
lines(density(household$Dollars[household$Region == 7]), col = "#A65628")  ## South East
legend(x = 2000, y = 0.0075, legend = c("Central Coast", "Central Highlands", "Mekong Delta", "North Coast", "Northern Uplands", "Red River Delta", "South East"), lty="solid", col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628"), cex = 0.7)
dev.off()



###################################################
### Figure 5-2  NOT INCLUDED
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-2.pdf")
plot(density(household$Dollars[household$Region == 1]), main = "", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), type = "n", xaxt = "n", yaxt = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x = 0.5, y = 0.5, label = "Plot 1", cex = 2)
dev.off()


###################################################
### Figure 5-3 NOT INCLUDED
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-3.pdf")
par(mfrow=c(2,3))
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 1", cex=2)
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 2", cex=2)
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 3", cex=2)
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 4", cex=2)
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 5", cex=2)
plot(density(household$Dollars[household$Region == 1]), main = " ", xlab = " ", ylab= " ", xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
text(x=0.5, y=0.5, label="Plot 6", cex=2)
par(mfrow=c(1,1))
dev.off()


###################################################
### Figure 5-4 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-4.pdf", height = 12, width = 6)
par(mfrow=c(4,2), pty="m")
plot(density(household$Dollars[household$Region == 1]), main = "Central Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 2]),main = "Central Highlands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 3]),main = "Mekong Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 4]),main = "North Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 5]),main = "Northern Uplands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 6]),main = "Red River Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(household$Dollars[household$Region == 7]), main = "South East", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
par(mfrow=c(1,1))
dev.off()


###################################################
### Figure 5-5 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-5.pdf", width=15, height=10)
boxplot(household$Dollars[household$Region == 1], household$Dollars[household$Region == 2], household$Dollars[household$Region == 3], household$Dollars[household$Region == 4], household$Dollars[household$Region == 5], household$Dollars[household$Region == 6], household$Dollars[household$Region == 7], names=c("Central Coast", "Central Highlands", "Mekong Delta", "North Coast", "Northern Uplands", "Red River Delta", "South East"), ylab = "Household Per Capita Expenditures (in U.S. Dollars)", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


###################################################
### Figure 5-6 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-6.pdf", height = 12, width = 6)
par(mfrow=c(4,2))
plot(density(household$Dollars[household$Region == 1]), main = "Central Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 2]),main = "Central Highlands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 3]),main = "Mekong Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 4]),main = "North Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 5]),main = "Northern Uplands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 6]),main = "Red River Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 7]),main = "South East", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.008), las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 119, lty = "dashed")
par(mfrow=c(1,1))
dev.off()



###################################################
### Figure 5-7 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-7.pdf", height = 12, width = 6)
par(mfrow=c(4,2))
plot(density(household$Dollars[household$Region == 1 & household$Area == "Rural"]), main = "Central Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 1 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 2 & household$Area == "Rural"]), main = "Central Highlands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 2 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 3 & household$Area == "Rural"]), main = "Mekong Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 3 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 4 & household$Area == "Rural"]), main = "North Coast", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 4 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 5 & household$Area == "Rural"]), main = "Northern Uplands", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 5 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 6 & household$Area == "Rural"]), main = "Red River Delta", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 6 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
plot(density(household$Dollars[household$Region == 7 & household$Area == "Rural"]), main = "South East", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",  bty = "l", xlim = c(0, 3100), ylim = c(0, 0.01), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Region == 7 & household$Area == "Urban"]), lty="dotted")
abline(v = 119, lty = "dashed")
par(mfrow=c(1,1))
dev.off()


###################################################
### Figure 5-8
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-8.pdf")
plot(density(nels$Achievement), main="", xlab="Mathematics Achievement", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


###################################################
### Figure 5-9 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-9.pdf")
plot(x = 1, xlab = "Average Weekly Time Spent on Mathematics Homework (in Hours)", ylab = "Mathematics Achievement", xlim=c(-0.4, 10.4), ylim = c(0, 100), bty = "l", type = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
boxplot(nels$Achievement[nels$Homework == 0], nels$Achievement[nels$Homework == 1], nels$Achievement[nels$Homework == 2], nels$Achievement[nels$Homework == 3], nels$Achievement[nels$Homework == 4], nels$Achievement[nels$Homework == 5], nels$Achievement[nels$Homework == 6], nels$Achievement[nels$Homework == 7], nels$Achievement[nels$Homework == 10], at = c(0:7, 10), add = TRUE, axes = FALSE, boxwex = 0.4)
dev.off()


###################################################
### Figure 5-10 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-10.pdf")
plot(x = nels$Homework, y = nels$Achievement, xlab = "Average Weekly Time Spent on Mathematics Homework (in Hours)", ylab = "Mathematics Achievement", bty="l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()

		
###################################################
### Figure 5-11 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-5-11.pdf")
plot(x = nels$Homework, y = nels$Achievement, xlab = "Average Weekly Time Spent on Mathematics Homework (in Hours)", ylab = "Mathematics Achievement", xlim=c(-0.4, 10.4), pch = 20, bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
boxplot(nels$Achievement[nels$Homework == 0], nels$Achievement[nels$Homework == 1], nels$Achievement[nels$Homework == 2], nels$Achievement[nels$Homework == 3], nels$Achievement[nels$Homework == 4], nels$Achievement[nels$Homework == 5], nels$Achievement[nels$Homework == 6], nels$Achievement[nels$Homework == 7], nels$Achievement[nels$Homework == 10], at = c(0:7, 10), add = TRUE, axes=FALSE, boxwex = 0.4, col = rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.3))
dev.off()


