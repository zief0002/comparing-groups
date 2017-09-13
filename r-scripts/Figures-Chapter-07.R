##################################################
### Read in All Data Sets 
###################################################
latino <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")



###################################################
### Figure 7-1
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-1.pdf")
plot(density(latino$Achieve[latino$Mex == 0], bw = 5.5), main= "", xlab = "Educational Achievement", bty = "l", lty = "dashed", xlim = c(0, 100), las=1, cex.axis=.7, cex.lab=.7)
lines(density(latino$Achieve[latino$Mex == 1], bw = 5.5), lty = "solid")
legend(x = 5, y = 0.030, legend = c("Non-Mexican Immigrants", "Mexican Immigrants"), lty = c("dashed", "solid"), bty = "n", cex=.7)
dev.off()


###################################################
### Figure 7-2 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-2.pdf", width=6, height=4)
plot(x = 1, main= "", xlab= "Educational Achievement", ylab = "", bty = "n", xlim = c(0, 100), ylim = c (0.8, 1.7), yaxt = "n", type = "n", las=1, cex.axis=.7, cex.lab=.7)
boxplot(latino$Achieve[latino$Mex == 0], latino$Achieve[latino$Mex == 1], horizontal = TRUE, add = TRUE, at = c(1.0, 1.5), boxwex = 0.1, frame.plot = FALSE, yaxt = "n", xaxt = "n")
text(x = 50, y = 1.65, label = "Mexican Immigrants", cex = 0.8, font = 3)
text(x = 50, y = 1.15, label = "Non-Mexican Immigrants", cex = 0.8, font = 3)
dev.off()


###################################################
### Figure 7-3 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-3.pdf")
pcoins <- dbinom(x = 0:10, size = 10, prob = 0.5)
plot(x = 0:10, y = pcoins, type = "h", xlab = bquote(italic("k")), main = "Number of Heads", ylab = "Probability", ylim = c(0, 0.3), las=1, bty="l", cex.axis=.7, cex.lab=.7)
points(x = 0:10, y = pcoins, pch = 16, cex = 1.5, lwd = 2)
dev.off()



###################################################
### Figure 7-4
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-4.pdf")
latino$z.achieve <- scale(latino$Achieve)
plot(density(latino$z.achieve[latino$Mex == 0], bw = 0.4), main= "", xlab = "Standardized Educational Achievement", bty = "l", lty = "dashed", xlim = c(-4, 4), las=1, cex.axis=.7, cex.lab=.7)
lines(density(latino$z.achieve[latino$Mex == 1], bw = 0.35), lty = "solid")
legend(x = -4, y = 0.45, legend = c("Non-Mexican Immigrants", "Mexican Immigrants"), lty = c("dashed", "solid"), bty = "n", cex=.85)
dev.off()


###################################################
### Figure 7-5 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-5.pdf", width=6, height=4)
latino$z.achieve <- scale(latino$Achieve)
plot(x = 1, main= "", xlab= "Standardized Educational Achievement", ylab = "",bty = "n", xlim = c(-4, 4), ylim = c (0.8, 1.7), yaxt = "n", type = "n", las=1, cex.axis=.7, cex.lab=.7)
boxplot(latino$z.achieve[latino$Mex == 0], latino$z.achieve[latino$Mex == 1], horizontal = TRUE, add = TRUE, at = c(1.0, 1.5), boxwex = 0.1, frame.plot = FALSE, yaxt = "n", xaxt = "n")
text(x = 0, y = 1.65, label = "Mexican Immigrants", cex = 0.8, font = 3)
text(x = 0, y = 1.15, label = "Non-Mexican Immigrants", cex = 0.8, font = 3)
dev.off()


###################################################
### Figure 7-6 Tikz
###################################################


###################################################
### Figure 7-7
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-7.pdf")
lat.gen <- function(data, mle){
     rnorm(n = length(data), mean = 0, sd = 1)
     } 
mean.diff <- function(data, indices) {
	mean(data[1:34]) - mean(data[35:150])
	} 
library(boot)
set.seed(100)
par.boot <- boot(data = latino$z.achieve, statistic = mean.diff, R = 4999, sim = "parametric", ran.gen = lat.gen)
plot(density(par.boot$t), xlab = "Standardized Mean Difference", main = "", bty = "l", xlim = c(-0.8, 0.8), las=1, cex.axis=.7, cex.lab=.7)
abline(v = 0, lty = 2)
dev.off()



###################################################
### Figure 7-8
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-8.pdf")
library(sm)
new <- sm.density(latino$Achieve[latino$Mex == 1], model = "normal", rugplot = FALSE, band = TRUE, col.band = "red")
new <- sm.density(latino$Achieve[latino$Mex == 1], se = TRUE, rugplot = FALSE)
plot(x = new$eval.points, y = new$estimate, type = "l", xlim = c(0, 100), ylim = c(0, 0.035), xlab = "Standardized Achievement", ylab = "Density", bty = "l", lwd = 1, las=1, cex.axis=.7, cex.lab=.7)
polygon(x = c(new$eval.points, rev(new$eval.points)), y = c(new$lower, rev(new$upper)), col = rgb(red = 100, green = 100, blue = 100, alpha = 100, maxColorValue = 255), border = NA)
#lines(x = new$eval.points, y = new$estimate, lwd = 3)
lines(x = new$eval.points, y = new$lower, lty = "dotted")
lines(x = new$eval.points, y = new$upper, lty = "dotted")
X <- seq(from = 0, to = 100, by = 0.001)
lines(x = X, dnorm(x = X, mean = mean(latino$Achieve[latino$Mex == 1]), sd = sd(latino$Achieve[latino$Mex == 1])), lty = "solid", lwd = 3)
dev.off()



###################################################
### Figure 7-9
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-9.pdf")
library(sm)
new <- sm.density(latino$Achieve[latino$Mex == 0], se = TRUE, rugplot = FALSE)
plot(x = new$eval.points, y = new$estimate, type = "n", xlim = c(0, 100), ylim = c(0, 0.05), xlab = "Standardized Achievement", ylab = "Density", bty = "l", las=1, cex.axis=.7, cex.lab=.7)
polygon(x = c(new$eval.points, rev(new$eval.points)), y = c(new$lower, rev(new$upper)), col = rgb(red = 100, green = 100, blue = 100, alpha = 100, maxColorValue = 255), border = NA)
lines(x = new$eval.points, y = new$estimate, lwd = 1)
lines(x = new$eval.points, y = new$lower, lty = "dotted")
lines(x = new$eval.points, y = new$upper, lty = "dotted")
X <- seq(from = 20, to = 100, by = 0.001)
lines(x = X, dnorm(x = X, mean = mean(latino$Achieve[latino$Mex == 0]), sd = sd(latino$Achieve[latino$Mex == 0])), lty = "solid", lwd = 3)
dev.off()


###################################################
### Figure 7-10 Tikz
###################################################


###################################################
### Figure 7-11
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-7-11.pdf")
mean.diff.np <- function(data, indices) {
	d <- data[indices, ]
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150])
	}
nonpar.boot <- boot(data = latino, statistic = mean.diff.np, R = 4999)	
plot(density(nonpar.boot$t), xlab="Mean Difference", main="", bty = "l", las=1, cex.axis=.7, cex.lab=.7)
abline(v=0, lty=2)
dev.off()


