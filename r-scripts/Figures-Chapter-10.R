##################################################
### Read in All Data Sets 
###################################################
math <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")  # PSAT.csv
math2 <- read.table(file = file.choose(), header = TRUE, sep = ",")  # Ordered PSAT.csv
math3 <- read.table(file = file.choose(), header = TRUE, sep = ",")  # BlockedPSAT.csv


##################################################
### Load Libraries 
###################################################
library(boot)



###################################################
### Figure 10-1
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-10-1.pdf")
plot(density(math2$PSAT[math2$Condition == "Control"], bw = 45), lty = "dashed", xlab = "PSAT Score", main = "", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(math2$PSAT[math2$Condition == "Treatment"], bw = 45))
legend(x = 600, y = 0.004, legend = c("Control", "Treatment"), lty = c("dashed", "solid"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 10-2
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-10-2.pdf")
plot(density(math3$Achievement[math3$Condition == "Control"], bw = 4), lty = "dashed", main = "", xlab = "Mathematics Achievement", ylab = "Density", bty= "l", xlim = c(-6, 50), ylim = c(0, 0.064), las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(math3$Achievement[math3$Condition == "Treatment"], bw = 4), lty = "solid" )
legend(x = 0, y = 0.055, legend = c("Control", "Treatment"), lty = c("dashed", "solid"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 10-3
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-10-3.pdf")
plot(x = math3$Achievement[math3$Condition == "Control"], main = "", xlab = "Mathematics Achievement", ylab= "", bty = "l", xlim = c(0, 50), ylim = c(0.8, 1.7), type = "n", yaxt = "n", las = 1, cex.axis = 0.7, cex.lab = 0.7)
boxplot(math3$Achievement[math3$Condition == "Control"], math3$Achievement[math3$Condition == "Treatment"],  horizontal = TRUE, boxwex = 0.1, add = TRUE, frame.plot = FALSE, at = c(1.0, 1.5), axes = FALSE, cex = 0.7)
text(x = 0, y = 1.5, label = "Control", pos = 4, cex = 0.7, font = 3)
text(x = 0, y = 1.0, label = "Treatment", pos = 4, cex = 0.7, font = 3)
dev.off()


###################################################
### Figure 10-4
###################################################
math.wide <- reshape(data = math3, direction = "wide", idvar = "Block", timevar =  "Condition", v.names = c("PSAT", "Achievement"))
math4 <- math.wide[ ,c(3, 5)]
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-10-4.pdf")
set.seed(100)
permuted <- replicate(n = 4999, expr = diff(apply(X = apply(X = math4, MARGIN = 1, FUN = sample), MARGIN = 1, FUN = mean)))
plot(density(permuted), main = "", xlab = "Mean Difference", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = 0)
dev.off()


###################################################
### Figure 10-5
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-10-5.pdf")
all.math <- c(math4$Achievement.Control, math4$Achievement.Treatment)
set.seed(100)
permuted.ind <- replicate (n = 4999, expr = sample(all.math))
mean.diff <- function (data) {
  mean(data[1:20]) - mean(data[21:40])
  }
diffs <- apply(X = permuted.ind , MARGIN = 2, mean.diff)
plot(density(diffs), main = "", xlab = "Mean Difference", ylim = c(0, 0.5), bty = "l", lty = "dashed", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(permuted), lty = "solid")
legend(x = -6.5, y = 0.48, legend = c("Dependence", "Independence"), lty = c("solid", "dashed"), bty = "n", cex = 0.7)
abline(v = 0, lty = "dotted")
dev.off()


###################################################
### Figure 10-6
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-10-6.pdf")
ts.math <- as.ts(math4)
glass.delta <- function(data){
	numerator <- abs(mean(data[ ,2]) - mean(data[ ,1]))
	denominator <- sd(data[ ,1])
	numerator / denominator
	} 
set.seed(100)
match.boot <- tsboot(tseries = ts.math, statistic = glass.delta, R = 4999, sim = "fixed", l = 2)
plot(density(match.boot$t), xlab = "Glass' Delta", main = "", bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
abline(v = glass.delta(ts.math), lty = "dotted")
dev.off()


