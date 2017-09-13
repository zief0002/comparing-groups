##################################################
### Read in All Data Sets 
###################################################
vlss <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")


##################################################
### Figure 3.2 
###################################################
pdf(file = "/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-3-2.pdf", height = 4, width = 8)
par(mfrow = c(1, 3), pty = "s")
plot(density(vlss$Age, bw = 0.5), main = "", xlab = "Age", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(vlss$Age, bw = 2), main = "", xlab = "Age", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(vlss$Age, bw = 10), main = "", xlab = "Age", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
par(mfrow=c(1, 1))
dev.off()


###################################################
### Figure 3.3 
###################################################
library(quantreg)
d.akd <- akj(vlss$Age, z = density(vlss$Age)$x)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-3-3.pdf", width = 7, height = 2.5)
par(mfrow=c(1, 3))
plot(density(vlss$Age), main = "", xlab = "Age", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(density(vlss$Age, bw = "SJ"), main = "", xlab = "Age", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
plot(x = density(vlss$Age)$x, y = d.akd$dens, type = "l", main = "", xlab = "Age",  ylab = "Density", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
par(mfrow=c(1, 1))
dev.off()


###################################################
### Figure 3.4 
###################################################
d <- density(vlss$Age)
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-3-4.pdf")
plot(x = d$x, y = d.akd$dens, type = "l", xlab = "Age (in years)", ylab = "Density", main = "", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


###################################################
### Figure 3.5 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-3-5.pdf")
set.seed(100)
age.sample <- sample(vlss$Age, size = 1000, replace = FALSE)
library(sm)
new <- sm.density(age.sample, h = 4, xlab = "Age", ylab = "Density", xlim = c(-7,106), ylim = c(0, 0.03), display = "se", rugplot = FALSE, las = 1, cex = 0.7)
plot(age.sample, xlab = "Age", ylab = "Density", xlim = c(-7,106), ylim = c(0, 0.03), las = 1, cex.axis = 0.7, cex.lab = 0.7, type = "n", bty = "l")
lines(new$estimate)
lines(new$lower, lty="dashed")
lines(new$upper, lty="dashed")

dev.off()

