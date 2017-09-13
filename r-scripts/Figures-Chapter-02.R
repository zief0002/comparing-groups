##################################################
### Read in Data Sets 
###################################################
latino <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")


##################################################
### Figure 2.4 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-2-4.pdf")
boxplot(latino$Achieve, las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


##################################################
### Figure 2.5 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-2-5.pdf")
plot(density(latino$Achieve), las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


##################################################
### Figure 2.6 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-2-6.pdf")
plot(density(subset(latino, latino$ImmAge >= 4 & latino$ImmAge <= 7)$Achieve), bty = "l", main = "", xlab = "Achievement", las = 1, cex.axis = 0.7, cex.lab = 0.7)
dev.off()


##################################################
### Figure 2.7 
################################################### 
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-2-7.pdf")
barplot(height = table(latino$English), names.arg = c("Not Fluent", "Fluent"), las = 1, cex.axis = 0.7, cex.lab = 0.7, cex = 0.7)
dev.off()


##################################################
### Figure 2.8 
###################################################
pdf(file="/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Comparing-Groups-Monograph-LaTeX-Files/Graphics/fig-2-8.pdf")
barplot(height = prop.table(table(latino$English)), names.arg = c("Not Fluent", "Fluent"), las = 1, cex.axis = 0.7, cex.lab = 0.7, cex = 0.7)
dev.off()


