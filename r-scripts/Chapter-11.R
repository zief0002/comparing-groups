###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 05, 2010
### Chapter 11: Planned Contrasts
###################################################

###################################################
### Command Snippet 11.1: Read in and Examine the Diet.csv Data
###################################################
diet <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(diet)
tail(diet)
str(diet)
summary(diet)


###################################################
### Command Snippet 11.2: Examine Research Question 1
###################################################
diet1 <- subset(x = diet, subset = Diet == "Atkins" | Diet == "Ornish")
plot(density(diet1$WeightChange[diet1$Diet == "Ornish"]), main = " ", xlab = "12-Month Weight Change", xlim = c(-80, 40), bty = "l")
lines(density(diet1$WeightChange[diet1$Diet == "Atkins"]), lty = "dashed")
legend(x = -75, y = 0.025, legend = c("Atkins", "Ornish"), lty = c("dashed", "solid"), bty = "n")
tapply(X = diet1$WeightChange, INDEX = diet1$Diet, FUN = mean)
tapply(X = diet1$WeightChange, INDEX = diet1$Diet, FUN = sd)


###################################################
### Command Snippet 11.3: Examine Research Question 2
###################################################
diet2 <- diet
levels(diet2$Diet) <- c("Atkins", "Others", "Others", "Others")
plot(density(diet2$WeightChange[diet2$Diet == "Others"]), main = " ", xlab = "12-Month Weight Change", xlim = c(-80, 40), bty = "l")
lines(density(diet2$WeightChange[diet2$Diet == "Atkins"]), lty = "dashed")
legend(x = -75, y = 0.030, legend = c("Atkins", "Others"), lty = c("dashed", "solid"), bty = "n")
tapply(X = diet2$WeightChange, INDEX = diet2$Diet, FUN = mean)
tapply(X = diet2$WeightChange, INDEX = diet2$Diet, FUN = sd)


###################################################
### Command Snippet 11.4: Examine Research Question 3
###################################################
diet3 <- diet
levels(diet3$Diet) <- c("CR", "BM","BM","CR")
plot(x = density(x = diet3$WeightChange[diet3$Diet == "BM"]), main = " ", xlab = "12-Month Weight Change", xlim = c(-80, 45), bty = "l")
lines(density(diet3$WeightChange[diet3$Diet == "CR"]), lty = "dashed")
legend(x = -75, y = 0.030, legend = c("Carbohydrate Restrictive", "Behavior Modification"), lty = c("dashed", "solid"), bty = "n")
tapply(X = diet3$WeightChange, INDEX = diet3$Diet, FUN = mean)
tapply(X = diet3$WeightChange, INDEX = diet3$Diet, FUN = sd)


###################################################
### Command Snippet 11.5: Compute Estimate of Contrast 1
###################################################
levels(diet$Diet)
con1 <- c(1, 0, -1, 0)
tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con1
sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con1)


###################################################
### Command Snippet 11.6: Compute Estimate of Contrast 2 and Contrast 3
###################################################
con2 <- c(3,-1,-1,-1)
con3 <- c(1,-1,-1,1)
sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con2)
sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con3)


###################################################
### Command Snippet 11.7: Randomization Test for Contrast 1
###################################################
set.seed(100)
permuted <- replicate(n = 4999, expr = sample(diet$WeightChange))
contrast.1 <- function(data) {
	(1) * mean(data[1:60]) +
	(0) * mean(data[61:120]) +
	(-1) * mean(data[121:180]) +
	(0) * mean(data[181:240]);
	}
contrast.1(diet$WeightChange)
perm.contrasts.1 <- apply(X = permuted, MARGIN = 2, FUN = contrast.1)
plot(density(perm.contrasts.1))
mean(perm.contrasts.1)
sd(perm.contrasts.1)
length(diffs[abs(perm.contrasts.1) >= 8.6])
(4 + 1) / (4999 + 1)

###################################################
### Command Snippet 11.8: Randomization Test for Contrast 2
###################################################
contrast.2 <- function(data) {
	(3) * mean(data[1:60]) +
	(-1) * mean(data[61:120]) +
	(-1) * mean(data[121:180]) +
	(-1) * mean(data[181:240]);
	}
contrast.2(diet$WeightChange)
perm.contrasts.2 <- apply(X = permuted, MARGIN = 2, FUN = contrast.2)
plot(density(perm.contrasts.2))
mean(perm.contrasts.2)
sd(perm.contrasts.2)
length(perm.contrasts.2[abs(perm.contrasts.2) >= 24.7])
(0 + 1) / (4999 + 1)


###################################################
### Command Snippet 11.9: Randomization Test for Contrast 3
###################################################
contrast.3(diet$WeightChange)
perm.contrasts.3 <- apply(X = permuted, MARGIN = 2, FUN = contrast.3)
plot(density(perm.contrasts.3))
mean(perm.contrasts.3)
sd(perm.contrasts.3)
length(perm.contrasts.3[abs(perm.contrasts.3) >= 6.3])
(477 + 1) / (4999 + 1)


###################################################
### Command Snippet 11.10: Computation of Total Sum of Squares
###################################################
var(diet$WeightChange) * (length(diet$WeightChange) - 1)


###################################################
### Command Snippet 11.11: Computation of Sum of Squares for Contrast 1
###################################################
psi <- sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con1)
numerator <- psi ^ 2
numerator
denominator <- sum(con1 ^ 2 / table(diet$Diet))
denominator
numerator / denominator


###################################################
### Command Snippet 11.12: Computation of Sum of Squares for Contrast 2 and 3
###################################################
numerator <- sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con2) ^ 2
denominator <- sum(con2 ^ 2 / table(diet$Diet))
numerator / denominator
18317 / 49960
numerator <- sum(tapply(X = diet$WeightChange, INDEX = diet$Diet, FUN = mean) * con3) ^ 2
denominator <- sum(con3 ^ 2 / table(diet$Diet))
numerator / denominator
590 / 49960


###################################################
### Command Snippet 11.13: Nonparametric Bootstrap Interval for Eta-Squared (Contrast 1)
###################################################
eta.squared <- function(data, indices) {
	d <- data[indices,]
	num <- sum(tapply(X = d$WeightChange, INDEX = d$Diet, FUN = mean) * con1) ^ 2
	den <- sum(con1 ^ 2 / table(d$Diet))
	SS.con <- num / den
	SS.tot <- var(d$WeightChange) * 239
	SS.con / SS.tot
	}
eta.squared(diet)
library(boot)			
nonpar.boot <- boot(data = diet, statistic = eta.squared, R = 4999, strata = diet$Diet)
plot(density(nonpar.boot$t), xlab = "Bootstrapped Eta-Squared", main = " ")
nonpar.boot
boot.ci(boot.out = nonpar.boot, type = "bca")


###################################################
### Command Snippet 11.14: Check Orthogonality 
###################################################
sum(con1 * con2)
sum(con1 * con3)
sum(con2 * con3)


###################################################
### Command Snippet 11.15: Read in and Examine the WordRecall.csv Data, Compute and Plot the Conditional Distributions 
###################################################
word <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(word)
tail(word)
str(word)
summary(word)
plot(x = word$Study, y = word$Recall, xlab = "Time Spent Studying", ylab = "Number of Words Recalled", xlim=c(0.4, 3.4), ylim = c(0, 30), bty = "l", pch = 20)
boxplot(word$Recall[word$Study == 1], word$Recall[word$Study == 2], word$Recall[word$Study == 3], at = c(1:3), add = TRUE, axes = FALSE, boxwex = 0.2, col = rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.3))  
tapply(X = word$Recall, INDEX = word$Study, FUN = mean)
tapply(X = word$Recall, INDEX = word$Study, FUN = sd)
table(word$Study)


###################################################
### Command Snippet 11.16: Compute the Polynomial Contrasts 
###################################################
poly.cont <- contr.poly(3)
poly.cont
con.linear <- poly.cont[ ,1]
con.linear
con.quad <- poly.cont[ ,2]
con.quad

