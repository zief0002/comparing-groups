##################################################
### Read in All Data Sets 
###################################################
asp <- read.table(file.choose(), header = TRUE, sep = ",", row.names = "ID")


###################################################
### Figure 6-1 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-6-1.pdf")
plot(density(asp$Delinq[asp$Treatment == 1], bw = 3), main= " ", xlab= "T-Scaled Delinquency Measure", bty= "l", lty = "solid", las=1, cex.axis=.7, cex.lab=.85)
lines(density(asp$Delinq[asp$Treatment == 0], bw = 3), lty="dashed")
legend(x = 70, y = 0.085, legend = c("Control Group", "Treatment Group"), lty = c("dashed", "solid"), bty = "n", cex=0.85)
dev.off()



###################################################
### Figure 6-2 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-FIles/Graphics/fig-6-2.pdf")
set.seed(100)
d <- density(apply(X = replicate(n = 4999, expr = sample(asp$Delinq)), MARGIN = 2, FUN = function(data) mean(data[1:187]) - mean(data[188:356])))
plot(d, main= " ", xlab= "Mean Difference", bty= "n", xaxt = "n", las=1, cex.axis=.7, cex.lab=.85)
axis(side = 1, at = seq(from = -4, to = 4, by = 1), pos = 0, cex.axis=.7)
X <- d$x[363:512]
Y <- d$y[363:512]
polygon(x = c(X, rev(X)), y = c(rep(0, 150), rev(Y)), col = rgb(red = 0.7, green = 0.7, blue = 0.7, alpha = 0.4), border = NA)
X <- d$x[1:146]
Y <- d$y[1:146]
polygon(x = c(X, rev(X)), y = c(rep(0, 146), rev(Y)), col = rgb(red = 0.7, green = 0.7, blue = 0.7, alpha = 0.4), border = NA)
segments(x0 = d$x[146], y0 = 0, x1 = d$x[146], y1 = d$y[146], lty = "dashed")
segments(x0 = d$x[363], y0 = 0, x1 = d$x[363], y1 = d$y[363], lty = "dashed")
points(x = d$x[363], y=0.01, pch=20, cex=4)
dev.off()



