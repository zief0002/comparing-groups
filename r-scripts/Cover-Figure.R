household <- read.csv("/Users/zief0002/Documents/Research/Comparing-Groups-Wiley/Data/VLSSperCapita.csv", header=TRUE)

library(colorspace)
library(dichromat)

rural.households <- household$Dollars[household$Area=="Rural"]
urban.households <- household$Dollars[household$Area=="Urban"]

d1 <- density(rural.households)
d2 <- density(urban.households)


pdf(file="/Users/zief0002/Desktop/cover-fig.pdf")

plot(d1, main= "", xlab= "", ylab="", bty="n", xaxt="n", yaxt="n", col= rgb(0.8890747, 0.8890747, 0.8890747),  xlim=c(0, 1500), ylim=c(0,0.0087), typ="n")

axis(side=1, at=seq(from=0, to=1500, by=500), pos=0)
mtext(side=1, text="Household Per Capita Expenditures (in U.S. Dollars)", line=2)

polygon(d1, col= rgb(0.8890747, 0.8890747, 0.8890747, 0.3921569), lty="solid")
polygon(d2, col= rgb(0.02692296, 0.4132267, 0.05507055, 0.3921569), lty="dotted")

boxplot(rural.households, urban.households, horizontal=TRUE, add=TRUE, at=c(.0077,.0084), boxwex=.0004, col=c(rgb(0.8890747, 0.8890747, 0.8890747, 0.3921569), rgb(0.02692296, 0.4132267, 0.05507055, 0.3921569)), frame.plot=FALSE, yaxt="n", xaxt="n", outpch=NA, whisklty="solid")

#text(x=850, y=0.0077, label="Rural Households", pos=4, cex=.8)
#text(x=850, y=0.0084, label="Urban Households", pos=4, cex=.8)		
text(x= 182, y= 0.0059, labels="Rural Households", pos=4, cex=.8)
text(x= 495, y= 0.0012, labels="Urban Households", pos=4, cex=.8)
dev.off()