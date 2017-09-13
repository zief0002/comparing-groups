##################################################
### Read in All Data Sets 
###################################################
diet <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")



###################################################
### Figure 12-1
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-1.pdf")
plot(x = 1, main = "", ylab = "Weight Change (in kg)", xlab = "Diet", bty = "l", type = "n", xlim = c(0.5, 4.5), ylim = c(-70, 30), xaxt = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
boxplot(WeightChange ~ Diet, data = diet, col = rgb(0.3,0.3,0.3,0.3), add = TRUE, boxwex = 0.4, axes = FALSE)
axis(side=1, at=1:4, labels=c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
dev.off()


###################################################
### Figure 12-2
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-2.pdf")
set.seed(100)
diet.jitter <- jitter(as.integer(diet$Diet), factor = 0.7)
plot(WeightChange ~ diet.jitter, data = diet, xlim = c(0.5, 4.5), ylim = c(-65, 25), main = "", ylab = "Weight Change (in kg)", xlab = "Diet", xaxt = "n", bty = "l", col = rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.7), las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:4, labels = c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
abline(h = mean(diet$WeightChange), col = "black", lty = "solid", lwd = 2)

for (i in c(5, 100, 163, 230)) {
  arrows(x0 = diet.jitter[i], y0 = diet[i, 2], x1 = diet.jitter[i], y1 = mean(diet$WeightChange), col = "black", length = 0, lwd = 2)
  points(x = diet.jitter[i], y = diet[i, 2], pch = 16)
	}
dev.off()


###################################################
### Figure 12-3
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-3.pdf")
set.seed(100)
diet.jitter <- jitter(as.integer(diet$Diet), factor = 0.7)
plot(WeightChange ~ diet.jitter, data = diet, xlim = c(0.5, 4.5), ylim = c(-65, 25), main = "", ylab = "Weight Change (in kg)", xlab = "Diet", xaxt = "n", bty = "l", col = rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.7), las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:4, labels = c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
abline(h = mean(diet$WeightChange), col = "black", lty = "solid", lwd = 2)
segments(x0 = 0.5, y0 = mean(diet$WeightChange[diet$Diet == "Atkins"]), x1 = 1.5, y1 = mean(diet$WeightChange[diet$Diet == "Atkins"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 1.5, y0 = mean(diet$WeightChange[diet$Diet == "LEARN"]), x1 = 2.5, y1 = mean(diet$WeightChange[diet$Diet == "LEARN"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 2.5, y0 = mean(diet$WeightChange[diet$Diet == "Ornish"]), x1 = 3.5, y1 = mean(diet$WeightChange[diet$Diet == "Ornish"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 3.5, y0 = mean(diet$WeightChange[diet$Diet == "Zone"]), x1 = 4.5, y1 = mean(diet$WeightChange[diet$Diet == "Zone"]), col = "black", lty = "dotted", lwd = 2)
for (i in c(5, 100, 163, 230)) {
  arrows(x0 = diet.jitter[i], y0 = mean(diet$WeightChange), x1 = diet.jitter[i], y1 = mean(diet$WeightChange[diet$Diet == diet[i, 1]]), col = "black", length = 0, lwd = 2, lty = "solid")
  arrows(x0 = diet.jitter[i], y0 = mean(diet$WeightChange[diet$Diet == diet[i, 1]]), x1 = diet.jitter[i], y1 = diet[i, 2], col = "black", length = 0, lty = "dashed", lwd = 2)
  points(x = diet.jitter[i], y = diet[i, 2], pch = 16)
	}
dev.off()


###################################################
### Figure 12-4
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-4.pdf")
set.seed(100)
diet.jitter <- jitter(as.integer(diet$Diet), factor = 0.7)
plot(WeightChange ~ diet.jitter, data = diet, xlim = c(0.5, 4.5), ylim = c(-65, 25), main = "", ylab = "Weight Change (in kg)", xlab = "Diet", xaxt = "n", bty = "l", col = rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.7), las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:4, labels = c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
abline(h = mean(diet$WeightChange), col = "black", lty = "solid", lwd = 2)

for (i in c(5, 100, 163, 230)) {
  arrows(x0 = diet.jitter[i], y0 = diet[i, 2], x1 = diet.jitter[i], y1 = mean(diet$WeightChange), col = "darkgreen", length = 0)
  arrows(x0 = diet.jitter[i], y0 = diet[i, 2], x1 = diet.jitter[i] + abs(diet[i, 2] - mean(diet[ ,2])) / 22.5, y1 = diet[i, 2], col = "black", length = 0)
  arrows(x0 = diet.jitter[i] + abs(diet$WeightChange[i] - mean(diet$WeightChange)) / 22.5, y0 = diet$WeightChange[i], x1 = diet.jitter[i] + abs(diet$WeightChange[i] - mean(diet$WeightChange)) / 22.5, y1 = mean(diet$WeightChange), col = "black", length = 0)
  arrows(x0 = diet.jitter[i], y0 = mean(diet$WeightChange), x1 = diet.jitter[i] + abs(diet$WeightChange[i] - mean(diet$WeightChange)) / 22.5, y1 = mean(diet$WeightChange), col = "black", length = 0) 
  X <- c(diet.jitter[i], diet.jitter[i], diet.jitter[i] + abs(diet$WeightChange[i] - mean(diet$WeightChange)) / 22.5, diet.jitter[i] + abs(diet$WeightChange[i] - mean(diet$WeightChange)) / 22.5)
  Y <- c(mean(diet$WeightChange), diet[i, 2], diet[i, 2], mean(diet$WeightChange))
  polygon(x = X, y = Y, col = rgb(red = 100, green = 100, blue = 100, alpha = 100, maxColorValue = 255))
  points(x = diet.jitter[i], y = diet[i, 2], pch = 16)
	}
dev.off()


###################################################
### Figure 12-5
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-5.pdf")
plot(WeightChange ~ diet.jitter, data = diet, xlim = c(0.5, 4.5), ylim = c(-65, 25), main="", ylab = "Weight Change (in kg)", xlab = "Diet", xaxt = "n", bty = "l", col = rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.7), las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:4, labels = c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
abline(h = mean(diet$WeightChange), col = "black", lty = "solid", lwd = 2)
model <- lm(WeightChange ~ Diet, data = diet)
segments(x0 = 0.5, y0 = mean(diet$WeightChange[diet$Diet == "Atkins"]), x1 = 1.5, y1 = mean(diet$WeightChange[diet$Diet == "Atkins"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 1.5, y0 = mean(diet$WeightChange[diet$Diet == "LEARN"]), x1 = 2.5, y1 = mean(diet$WeightChange[diet$Diet == "LEARN"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 2.5, y0 = mean(diet$WeightChange[diet$Diet == "Ornish"]), x1 = 3.5, y1 = mean(diet$WeightChange[diet$Diet == "Ornish"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 3.5, y0 = mean(diet$WeightChange[diet$Diet == "Zone"]), x1 = 4.5, y1 = mean(diet$WeightChange[diet$Diet == "Zone"]), col = "black", lty = "dotted", lwd = 2)
for (i in c(5, 100, 163, 230)) {
  arrows(x0 = diet.jitter[i], y0 = model$fitted.values[i], x1 = diet.jitter[i], y1=mean(diet[ , 2]), col = "black", length = 0)
  arrows(x0 = diet.jitter[i], y0 = model$fitted.values[i], x1 = diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5, y1 = model$fitted.values[i], col = "black", length = 0)
  arrows(x0 = diet.jitter[i], y0 = mean(diet[ , 2]), x1 = diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5, y1 = mean(diet[ , 2]), col = "black", length = 0)
  arrows(diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5, mean(diet[ , 2]), diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5, model$fitted.values[i], col = "black", length = 0)
  X <- c(diet.jitter[i], diet.jitter[i], diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5, diet.jitter[i] + abs(model$fitted.values[i] - mean(diet[ , 2])) / 22.5)
  Y <- c(model$fitted.values[i], mean(diet[ , 2]), mean(diet[ , 2]), model$fitted.values[i])
  polygon(x = X, y = Y, col = rgb(red = 100, green = 100, blue = 100, alpha = 100, maxColorValue = 255))
  points(x = diet.jitter[i], y = diet[i, 2], pch = 16)
  }
dev.off()


###################################################
### Figure 12-6
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-6.pdf")
plot(WeightChange ~ diet.jitter, data = diet, xlim = c(0.5, 4.5), ylim = c(-65, 25), main = "", ylab = "Weight Change (in kg)", xlab = "Diet", xaxt = "n", bty = "l", col = rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.7), las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:4, labels = c("Atkins", "LEARN", "Ornish", "Zone"), cex.axis = 0.7)
abline(h = mean(diet$WeightChange), col = "black", lty = "solid", lwd = 2)
segments(x0 = 0.5, y0 = mean(diet$WeightChange[diet$Diet == "Atkins"]), x1 = 1.5, y1 = mean(diet$WeightChange[diet$Diet == "Atkins"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 1.5, y0 = mean(diet$WeightChange[diet$Diet == "LEARN"]), x1 = 2.5, y1 = mean(diet$WeightChange[diet$Diet == "LEARN"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 2.5, y0 = mean(diet$WeightChange[diet$Diet == "Ornish"]), x1 = 3.5, y1 = mean(diet$WeightChange[diet$Diet == "Ornish"]), col = "black", lty = "dotted", lwd = 2)
segments(x0 = 3.5, y0 = mean(diet$WeightChange[diet$Diet == "Zone"]), x1 = 4.5, y1 = mean(diet$WeightChange[diet$Diet == "Zone"]), col = "black", lty = "dotted", lwd = 2)
for (i in c(5, 100, 163, 230)) {
  arrows(x0 = diet.jitter[i], y0 = model$fitted.values[i], x1 = diet.jitter[i], y1 = diet[i, 2], col = "black", length = 0)
  arrows(x0 = diet.jitter[i], y0 = model$fitted.values[i], x1 = diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5, y1 = model$fitted.values[i], col = "black", length = 0)
  arrows(x0 = diet.jitter[i], y0 = diet[i, 2], x1 = diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5, y1 = diet[i, 2], col = "black", length = 0)
  arrows(diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5, diet[i, 2], diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5, model$fitted.values[i], col = "black", length = 0)
  X <- c(diet.jitter[i], diet.jitter[i], diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5, diet.jitter[i] + abs(model$fitted.values[i] - diet[i, 2]) / 22.5)
  Y <- c(model$fitted.values[i], diet[i, 2], diet[i, 2], model$fitted.values[i])
  polygon(x = X, y = Y, col = rgb(red = 100, green = 100, blue = 100, alpha = 100, maxColorValue = 255))
  points(x = diet.jitter[i], y = diet[i,2], pch = 16)
  }
dev.off()



###################################################
### Figure 12-7
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-7.pdf")
library(gplots)
xbar <- -1 * tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean)
se <- tapply(diet$WeightChange, INDEX = diet$Diet, FUN = sd) / sqrt(table(diet$Diet))
barplot2(height = xbar, names.arg = c("Atkins", "LEARN", "Ornish", "Zone"), main = "", xlab = "Diet", ylab = "Weight Loss (in kg)", ylim = c(0, 20), plot.ci = TRUE, ci.u = xbar + se, ci.l = xbar, las = 1, cex.axis = 0.7, cex.lab = 0.7, cex = 0.7)
dev.off()


###################################################
### Figure 12-8
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-8.pdf")
point.estimate <- c(-9.2, -8.5, -7.0, -2.3, -1.6, -0.7)
lower.bound <- c(-14.2, -13.9, -12.2, -6.9, -6.3, -5.8)
upper.bound <- c(-4.3, -3.1, -2.1, 2.6, 3.8, 4.1)
plot(x = point.estimate, xlab = "Weight Change (in kg)", ylab = " ", xlim = c(-30, 10), ylim = c(1, 6), type = "n", bty = "n", yaxt = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
segments(x0 = lower.bound, y0 = 1:6, x1 = upper.bound, y1 = 1:6, lty=c(1, 1, 1, 3, 3, 3), col=c("black", "black", "black", "black", "black", "black"))
points(x = point.estimate, y = 1:6, pch = 20)
text(x = -30, y = 1:6, label = c("Atkins-Zone", "Atkins-Ornish", "Atkins-LEARN", "LEARN-Zone", "LEARN-Ornish", "Ornish-Zone"), cex = 0.7, pos = 4)
abline(v = 0, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3))
dev.off()


###################################################
### Figure 12-9
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-12-9.pdf")
point.estimate <- c(-9.2, -8.5, -7.0, -2.3, -1.6, -0.7)
lower.bound <- c(-16.2, -15.1, -13.2, -7.3, -6.5, -5.8)
upper.bound <- c(-2.7, -1.8, -1.2, 3.0, 4.0, 4.1)
plot(x = point.estimate, xlab = "Weight Change (in kg)", ylab="", xlim = c(-30, 10), ylim = c(0, 6.5), type = "n", bty = "n", yaxt = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
segments(x0 = lower.bound, y0 = 1:6, x1 = upper.bound, y1 = 1:6, lty=c(1, 1, 1, 3, 3, 3), col=c("black", "black", "black", "black", "black", "black"))
points(x = point.estimate, y = 1:6, pch = 20)
text(x = -30, y = 1:6, label = c("Atkins-Zone", "Atkins-Ornish", "Atkins-LEARN", "LEARN-Zone", "LEARN-Ornish", "Ornish-Zone"), cex = 0.7, pos = 4)
abline(v = 0, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3))
dev.off()






