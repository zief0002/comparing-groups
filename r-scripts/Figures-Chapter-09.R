##################################################
### Read in All Data Sets 
###################################################
latino <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")


##################################################
### Load Packages 
###################################################
library(boot)
library(MBESS)
library(WRS)
library(sm)


###################################################
### Figure 9-1 
###################################################
point.estimate <- function(data, indices) {
	d <- data[indices,];
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150]) + 3;
	} 
set.seed(100)	
boot.1 <- boot(data = latino, statistic = point.estimate, R = 4999)
point.estimate.2 <- function(data, indices) {
	d <- data[indices,];
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150]);
	} 
set.seed(100)	
boot.2 <- boot(data = latino, statistic = point.estimate.2, R = 4999)
point.estimate.3 <- function(data, indices) {
	d <- data[indices,];
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150]) + 10;
	} 
set.seed(100)	
boot.3 <- boot(data = latino, statistic = point.estimate.3, R = 4999)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-1.pdf")
plot(density(boot.2$t), lty = "dotted", xaxt = "n", bty = "n", xlim = c(-12, 20), xlab = "", main = "", las=1, cex.axis=.7, cex.lab=.7)
lines(density(boot.1$t), lty = "dashed")
lines(density(boot.3$t), lty = 6)
axis(side = 1, at = seq(from = -12, to = 20, by = 5), pos = 0, cex.axis=.7)
mtext(side = 1, text = "Mean Difference", line = 2, cex = 0.7)
points(x = 5.9, y = 0.004, col = "black", pch = 20, cex = 4)
equ1 <- expression(mu[Difference] == 0)
text(x = -3, y = 0.124, label = equ1, pos = 3, cex = 0.7)
equ2 <- expression(mu[Difference] == 3)
text(x = 3.7, y = 0.1264826, label = equ2, pos = 4, cex = 0.7)
equ3 <- expression(mu[Difference] == 10)
text(x = 11, y = 0.1264826, label = equ3, pos = 4, cex = 0.7)
dev.off()


###################################################
### Figure 9-2
###################################################
model.1 <- function(data, indices) {
	d <- data[indices,];
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150]) + 3;
	} 
set.seed(100)	
boot.1 <- boot(data = latino, statistic = model.1, R = 4999)

pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-2.pdf")
plot(density(boot.1$t), xaxt = "n", bty = "n", xlim = c(-10, 15), xlab = "", main = "", las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = seq(from = -10, to = 15, by = 5), pos = 0, cex.axis = 0.7)
mtext(side = 1, text = "Mean Difference", line = 2, cex = 0.7)
new <- density(boot.1$t)
x <- new$x[306:512]
y <- new$y[306:512]
polygon(x = c(x, rev(x)), y = c(rep(0, 207), rev(y)), border = NA, col = rgb(red = 200, green = 200, blue = 200, alpha = 200, maxColorValue = 255))
points(x = 5.9, y = 0.004, col = "black", pch = 20, cex = 4)
segments(x0 = 3, y0 = 0, x1 = 3, y1 = 0.13, lty = "dashed")
dev.off()


###################################################
### Figure 9-3: Tikz Graphic
###################################################


###################################################
### Figure 9-4
###################################################
mean.diff.alt <- function(data, indices) {
	d <- data[indices,];
	mean(d$Achieve[d$Mex==0]) - mean(d$Achieve[d$Mex==1]);
	}
set.seed(100)
altmodel.boot <- boot(data = latino, statistic = mean.diff.alt, R = 4999, strata = latino$Mex)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-4.pdf")
plot(density(altmodel.boot$t), xlab = "Standardized Mean Difference", main = "", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 5.92, lty = "dashed")
dev.off()


###################################################
### Figure 9-5 
###################################################
robust.std.effect <- function(data, indices) {
	d <- data[indices,];
	.642*smd(
		Mean.1 = mean(d$Achieve[d$Mex == 0], tr = 0.2), 
		Mean.2 = mean(d$Achieve[d$Mex == 1], tr = 0.2), 
		s.1 = sqrt(winvar(d$Achieve[d$Mex == 0], tr = 0.2)), 
		s.2 = sqrt(winvar(d$Achieve[d$Mex == 1], tr = 0.2)), 
		n.1 = length(d$Achieve[d$Mex == 0]), 
		n.2 = length(d$Achieve[d$Mex == 1])
		);
	}
set.seed(100)
robust.boot <- boot(data = latino, statistic = robust.std.effect, R = 4999, strata = latino$Mex)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-5.pdf")
plot(density(robust.boot$t), main = "", xlab = "Robust Standardized Effect Size", ylab = "Density", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 0.38, lty = "dashed")
dev.off()


###################################################
### Figure 9-6
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-6.pdf")
mex.z <- scale(latino$Achieve[latino$Mex == 1])
qqnorm(mex.z, ylim = c(-3.5, 3.5), xlab = "Normal Quantiles", ylab = "Empirical Quantiles", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(a = 0, b = 1, col = "black")
dev.off()


###################################################
### Figure 9-7
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-7.pdf")
plot(density(mex.z), xlab = "Standardized Achievement", main = "", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
X <- seq(from = -4, to = 4, length = 100)
d1 <- dnorm(X, mean = mean(mex.z), sd = sd(mex.z))
lines(x = X, y = d1, lty = "dotted")
lines(density(mex.z))
dev.off()


###################################################
### Figure 9-8
###################################################
mex.gen <- function(data, mle){
  rnorm(n = length(data), mean = 0, sd = 1);
  }
set.seed(100)  
mex.qqboot <- boot(data = mex.z, statistic = sort, R = 4999, sim = "parametric", ran.gen = mex.gen)  
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-8.pdf")
qqnorm(mex.qqboot$t[1,], ylim = c(-3.5, 3.5), xlab = "Normal Quantiles", ylab = "Empirical Quantiles", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(a = 0, b = 1, col = "black")
dev.off()


###################################################
### Figure 9-9
###################################################
my.qq <- qqnorm(sort(mex.z), plot = FALSE)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-9.pdf")
plot(x = my.qq$x, y = my.qq$y, xlab="Normal Quantiles", ylab="Empirical Quantiles", ylim = c(-3.5, 3.5), type = "n", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
for(i in 1:25){
	points(x = sort(my.qq$x), y = mex.qqboot$t[i, ], type = "l", lty = "dotted", col = rgb(red = 100, green = 100, blue = 100, alpha = 200, maxColorValue = 255));
	}
abline(a = 0, b = 1, col = "black", lwd = 2)
dev.off()


###################################################
### Figure 9-10
###################################################
mex.env <- envelope(boot.out = mex.qqboot)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-10.pdf")
qqnorm(mex.z, xlab = "Normal Quantiles", ylab = "Empirical Quantiles", ylim = c(-3.5, 3.5), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(x = sort(my.qq$x), y = mex.env$overall[1, ])
lines(x = sort(my.qq$x), y = mex.env$overall[2, ])
dev.off()


###################################################
### Figure 9-11:  
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-9-11.pdf")
new <- sm.density(mex.z,   model = "normal", rugplot = FALSE, sm.options = list(add = TRUE))

plot(x = new$eval.points, y = new$estimate, type = "n", ylim = c(0, 0.035), xlab = "Standardized Achievement", ylab = "Density", bty = "l", lwd = 1, las=1, cex.axis=.7, cex.lab=.7)


dev.off()


