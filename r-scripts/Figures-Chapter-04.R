##################################################
### Read in All Data Sets 
###################################################
household <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")



###################################################
### Figure 4-1 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-1.pdf")
plot(density(household$Dollars),  xlab="U.S. Dollars", main="", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


###################################################
### Figure 4-2 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-2.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = " ", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", lty = "solid", las = 1, cex.axis = 0.7, cex.lab = 0.7, bty = "l")
lines(density(household$Dollars[household$Area == "Urban"]), lty = "dotted")
dev.off()


###################################################
### Figure 4-3 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-3.pdf")
boxplot(household$Dollars[household$Area == "Rural"], household$Dollars[household$Area == "Urban"], names= c("Rural", "Urban"), ylab = "Household Per Capita Expenditures (in U.S. Dollars)", las = 1, cex.axis = 0.7, cex.lab = 0.7, bty = "l")
dev.off()


###################################################
### Figure 4-4 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-4.pdf")
b <- seq(from = 60, to = 150, length = 10000)
d <- seq(from = 90, to = 135, length=10000)
plot(x = b, y = 0.9 * dnorm(b,mean = 106, sd = 12) * 0.35 * dnorm(d, mean = 115, sd = 6), lty = "solid", type = "n", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", ylab = "Density", xlim = c(50, 150), ylim=c(0, 0.061), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(x = b, y = 0.9 * dnorm(b, mean = 106, sd = 12), lty = "solid")
lines(x = d, y = 0.35 * dnorm(d, mean = 115, sd = 6), lty = "dotted")
legend(x = 130, y = 0.060, legend = c("Rural", "Urban"), lty = c("solid", "dotted"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 4-5 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-5.pdf")
b <- seq(from = 102, to = 110, length = 10000)
d <- seq(from = 111, to = 119, length = 10000)
plot(x = b, y = 0.15 * dnorm(b, mean = 106, sd = 1) * 0.06*dnorm(d, mean = 115, sd = 1), lty = "solid", type = "n", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", ylab = "Density", xlim = c(50, 150), ylim = c(0, 0.061), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(x = b, y = 0.15 * dnorm(b, mean = 106, sd = 1), lty = "solid")
lines(x = d, y = (0.06) * dnorm(d, mean = 115, sd = 1), lty = "dotted")
legend(x = 130, y = 0.060, legend = c("Rural", "Urban"), lty = c("solid", "dotted"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 4-6 
###################################################
library(VGAM)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-6.pdf")
b <- seq(from = -4, to = 4, length = 10000)
plot(x = b, y = dnorm(b, mean = 0, sd = 1), lty = "solid", type = "l", xlab = "", ylab = "Density", xlim = c(-4, 4), ylim = c(0, 0.55), lwd = 2, bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(x = b, y = dlaplace(b), lty = "dotted")
legend(x = 0.4, y = 0.54, legend = c("Mesokurtic (G2=0)", "Leptokurtic Distribution (G2=3)"), lty = c("solid", "dotted"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 4-7 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-7.pdf")
b <- seq(from = -4, to = 4, length = 10000)
plot(x = b, y = dnorm(b, mean = 0, sd = 1), lty = "solid", type = "l", xlab = "", ylab = "Density", xlim = c(-4, 4), ylim=c(0, 0.45), lwd = 2,  bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(x = b, y = dunif(b, min=-3.9, max=3.9), lty = "dotted")
legend(x = 0.25, y = 0.44, legend = c("Mesokurtic (G2=0)", "Platykurtic Distribution (G2=-1.2)"), lty = c("solid", "dotted"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 4-8 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-8.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", lty = "solid", las = 1, cex.axis = 0.7, cex.lab = 0.7, bty = "l")
lines(density(household$Dollars[household$Area == "Urban"]), lty = "dotted")
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4, cex = 0.7)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4, cex = 0.7)
dev.off()



###################################################
### Figure 4-9 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-9.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", lty = "solid", col= rgb(red = 139, green = 0, blue = 0, maxColorValue = 255), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(household$Dollars[household$Area == "Urban"]), lty = "solid", col= rgb(red = 0, green = 0, blue = 139, maxColorValue = 255))
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4, cex = 0.7)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4, cex = 0.7)
dev.off()


###################################################
### Figure 4-10 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-10.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", bty = "l", type = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
polygon(density(household$Dollars[household$Area == "Rural"]), col = rgb(red = 139, green = 0, blue = 0, alpha = 100, maxColorValue = 255), lty = "solid", border = rgb(red = 139, green = 0, blue = 0, maxColorValue = 255))
polygon(density(household$Dollars[household$Area == "Urban"]), col = rgb(red = 0, green = 0, blue = 139, alpha = 100, maxColorValue = 255), lty = "solid", border = rgb(red = 0, green = 0, blue = 139, maxColorValue = 255)) 
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4, cex = 0.7)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4, cex = 0.7)
dev.off()


###################################################
### Figure 4-11 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-11.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", bty = "l", type = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
polygon(density(household$Dollars[household$Area == "Rural"]), col = rgb(red = 0.2890237, green = 0, blue = 0, alpha = 0.4), lty = "solid")
polygon(density(household$Dollars[household$Area == "Urban"]), col = rgb(red = 0, green = 0, blue = 0.2890237, alpha = 0.4), lty = "solid") 
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4, cex = 0.7)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4, cex = 0.7)
dev.off()


###################################################
### Figure 4-12 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-4-12.pdf")
plot(density(household$Dollars[household$Area == "Rural"]), main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)", bty = "l", type = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
polygon(density(household$Dollars[household$Area == "Rural"]), col = rgb(red = 0.1262700, green = 0.1262700, blue = 0.03423021, alpha = 0.4), lty = "solid")
polygon(density(household$Dollars[household$Area == "Urban"]), col = rgb(red = 0.2123779, green = 0.2123779, blue = 0.49835797, alpha = 0.4), lty = "solid") 
text(x = 182, y = 0.0059, labels = "Rural Households", pos = 4, cex = 0.7)
text(x = 495, y = 0.0012, labels = "Urban Households", pos = 4, cex = 0.7)
dev.off()


