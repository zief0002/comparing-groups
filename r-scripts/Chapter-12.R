###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 04, 2010
### Chapter 12: Unplanned Contrasts
###################################################


###################################################
### Command Snippet 12.1: Syntax for Reading in Diet Data
###################################################
diet <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(diet)
tail(diet)
str(diet)
summary(diet)

###################################################
### Command Snippet 12.2: Fitting the Omnibus Model Using lm( )
###################################################
omnibus.model <- lm(diet$WeightChange ~ diet$Diet)
omnibus.model


###################################################
### Command Snippet 12.3: Computing Total Sum of Squares
###################################################
var(diet$WeightChange) * (240 - 1)


###################################################
### Command Snippet 12.4: Computing Residual Sum of Squares
###################################################
sum(omnibus.model$residuals ^ 2)
49959.62 - 46744.3


###################################################
### Command Snippet 12.5: Randomization Test for the Omnibus
###################################################
permuted <- replicate(n = 4999, expr = lm(sample(diet$WeightChange) ~ diet$Diet)$residuals)
eta.squared.omnibus <- function(data) {
	ss.total <- 49959.62
	ss.residual <- sum(data ^ 2)
	ss.diet <- ss.total - ss.residual
	ss.diet / ss.total
	}
perms <- apply(X = permuted, MARGIN = 2, FUN = eta.squared.omnibus)
length(perms[abs(perms) >= 0.064])


###################################################
### Command Snippet 12.6: Unadjusted and Dunn-Bonferroni Adjusted p-Values 
###################################################
p.unadjusted = c(0.0008, 0.0010, 0.0092, 0.3896, 0.5374, 0.7896)
p.unadjusted * 6


###################################################
### Command Snippet 12.7: Benjamani-Hochberg Adjusted p-Values 
###################################################
p.adjust(p = p.unadjusted, method = "BH")


###################################################
### Command Snippet 12.8: Point Estimates, Lower and Upper Bootstrap Limits
###################################################
point.estimate <- c(-9.2, -8.5, -7.0, -2.3, -1.6, 0.7)
lower.limit <- c(-14.2, -13.9, -12.2, -6.9, -6.3, -5.8)
upper.limit <- c(-4.3, -3.1, -2.1, 2.6, 3.8, 4.1)


###################################################
### Command Snippet 12.9: Set Up Plot
################################################### 
plot(x = point.estimate, xlab = "Contrast Value", ylab = " ", xlim = c(-30, 10), ylim = c(1, 6), type = "n", bty = "n", yaxt = "n")


###################################################
### Command Snippet 12.10: Set Up Plot
################################################### 
segments(x0 = lower.bound, y0 = 1:6, x1 = upper.bound, y1 = 1:6, lty = "dotted")


###################################################
### Command Snippet 12.11: Add the Intervals
################################################### 
points(x = point.estimate, y = 1:6, pch = 20)
text(x = -30, y = 1:6, label = c("Atkins-Zone", "Atkins-Ornish", "Atkins-LEARN", "LEARN-Zone", "LEARN-Ornish", "Ornish-Zone"), cex = 0.8, pos = 4)
abline(v = 0)



