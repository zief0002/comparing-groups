##################################################
### Read in All Data Sets 
###################################################
diet <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
word <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")


###################################################
### Figure 11-1
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-11-1.pdf")
plot(density(diet$WeightChange[diet$Diet == "Ornish"]), main = "", xlab = "12-Month Weight Change", xlim = c(-80, 40), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(diet$WeightChange[diet$Diet == "Atkins"]), lty = "dashed")
legend(x = -75, y = 0.025, legend = c("Atkins", "Ornish"), lty = c("dashed", "solid"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 11-2
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-11-2.pdf")
plot(density(diet$WeightChange[diet$Diet != "Atkins"]), main = "", xlab = "12-Month Weight Change", xlim = c(-80, 40), bty = "l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(diet$WeightChange[diet$Diet == "Atkins"]), lty = "dashed")
legend(x = -75, y = 0.030, legend = c("Atkins", "Others"), lty = c("dashed", "solid"), bty = "n", cex = 0.7)
dev.off()



###################################################
### Figure 11-3
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-11-3.pdf")
plot(density(diet$WeightChange[diet$Diet == "LEARN" | diet$Diet == "Ornish"]), main="", xlab="12-Month Weight Change", xlim=c(-80, 45),bty="l", las = 1, cex.axis = 0.7, cex.lab = 0.7)
lines(density(diet$WeightChange[diet$Diet == "Atkins" | diet$Diet == "Zone"]), lty="dashed")
legend(x = -75, y = 0.030, legend = c("Carbohydrate Restrictive", "Behavior Modification"), lty = c("dashed", "solid"), bty = "n", cex = 0.7)
dev.off()


###################################################
### Figure 11-4 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-11-4.pdf")
plot(x = word$Study, y = word$Recall, xlab = "Time Spent Studying", ylab = "Number of Words Recalled", xlim=c(0.4, 3.4), ylim = c(0, 30), bty = "l", xaxt = "n", pch = 20, las = 1, cex.axis = 0.7, cex.lab = 0.7)
axis(side = 1, at = 1:3, labels = c("30 minutes", "60 minutes", "90 minutes"), cex.axis = 0.7)
boxplot(word$Recall[word$Study == 1], word$Recall[word$Study == 2], word$Recall[word$Study == 3], at = c(1:3), add = TRUE, axes = FALSE, boxwex = 0.2, col = rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.3))
segments(x0 = 1, y0 = 8, x1 = 2, y1 = 11, lty = "dotted")
segments(x0 = 2, y0 = 11, x1 = 3, y1 = 17, lty = "dotted")
points(x = 1:3, y = c(8, 11, 17), pch = 17)
dev.off()



