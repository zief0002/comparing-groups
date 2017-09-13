###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 05, 2010
### Chapter 09: Bootstrap Intervals and Effect Sizes
###################################################


###################################################
### Read in the LatinoEd.csv Data and Load boot Package
###################################################
latino <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
library(boot)


###################################################
### Command Snippet 9.1: Bootstrap Under Assumption that Mean Difference is 3
###################################################
pv.3 <- function(data, indices) {
	d <- data[indices,];
	mean(x = d$Achieve[1:34]) - mean(x = d$Achieve[35:150]) + 3;
	} 
set.seed(100)
model.boot <- boot(data = latino, statistic = pv.3, R = 4999)
mean(model.boot$t)
sd(model.boot$t)
length(model.boot$t[model.boot$t >= 5.9])
(875 + 1)/(4999 + 1)


###################################################
### Command Snippet 9.2: Function to Compute Mean Difference Under Alternative Model
###################################################
mean.diff.alt <- function(data, indices) {
	d <- data[indices, ]
	mean(d$Achieve[d$Mex == 0]) - mean(d$Achieve[d$Mex == 1])
	} 


###################################################
### Command Snippet 9.3: Test mean.diff.alt() Function
###################################################
mean.diff.alt(latino)


###################################################
### Command Snippet 9.4: Carry Out Nonparametric Bootstrap Under Alternative Model and Examine Bootstrap Distribution
###################################################
altmodel.boot <- boot(latino, statistic = mean.diff.alt, R = 4999, strata = latino$Mex)
plot(density(altmodel.boot$t))
mean(altmodel.boot$t)
sd(altmodel.boot$t)


###################################################
### Command Snippet 9.5: Nonparametric Bootstrap BCa Interval
###################################################
boot.ci(altmodel.boot, type = "bca")


###################################################
### Command Snippet 9.6: Computing Standardized Effect Size 
###################################################
library(MBESS)
smd(Mean.1 = mean(latino$Achieve[latino$Mex == 0]), Mean.2 = mean(latino$Achieve[latino$Mex == 1]), s.1 = sd(latino$Achieve[latino$Mex == 0]), s.2 = sd(latino$Achieve[latino$Mex == 1]), n.1 = length(latino$Achieve[latino$Mex == 0]), n.2 = length(latino$Achieve[latino$Mex == 1]))

	
###################################################
### Command Snippet 9.7: Computing Standardized Effect Size 
###################################################
library(WRS)
0.642*smd(Mean.1 = mean(latino$Achieve[latino$Mex == 0], tr = 0.2), Mean.2 = mean(latino$Achieve[latino$Mex == 1], tr = 0.2), s.1 = sqrt(winvar(latino$Achieve[latino$Mex == 0], tr = 0.2)), s.2 = sqrt(winvar(latino$Achieve[latino$Mex == 1], tr = 0.2)), n.1 = length(latino$Achieve[latino$Mex == 0]), n.2 = length(latino$Achieve[latino$Mex == 1]))



###################################################
### Command Snippet 9.8: Bootstrap Robust Standardized Effect Size, Examine Bootstrap Distribution and Compute Bootstrap Interval 
###################################################
robust.std.effect <- function(data, indices) {
	d <- data[indices,]
	0.642*smd(Mean.1 = mean(d$Achieve[d$Mex == 0], tr = 0.2), Mean.2 = mean(d$Achieve[d$Mex == 1], tr = 0.2), s.1 = sqrt(winvar(d$Achieve[d$Mex == 0], tr = 0.2)), s.2 = sqrt(winvar(d$Achieve[d$Mex == 1], tr = 0.2)), n.1 = length(d$Achieve[d$Mex == 0]), n.2 = length(d$Achieve[d$Mex == 1]))
	} 
robust.boot <- boot(data = latino, statistic = robust.std.effect, R = 4999, strata = latino$Mex)
plot(density(robust.boot$t))
mean(robust.boot$t)
sd(robust.boot$t)
boot.ci(boot.out = robust.boot, type = "bca")



###################################################
### Command Snippet 9.9: Create Q-Q Plot
###################################################
mex.z <- latino$z.achieve[latino$Mex == 1]
qqnorm(y = mex.z, ylab = "Empirical Quantiles", xlab =" Normal Quantiles ")
abline (a = 0, b = 1)


###################################################
### Command Snippet 9.10: Function to Sample from N(0, 1) Distribution
###################################################
mex.gen <- function (data, mle ){
  rnorm(n = length(data), mean = 0, sd = 1)
  }


###################################################
### Command Snippet 9.11: Parametric Bootstrap
###################################################
mex.qqboot <- boot(data = mex.z, statistic = sort, R = 4999, sim = "parametric", ran.gen = mex.gen)


###################################################
### Command Snippet 9.12: Create and Examine Bootstrap Envelope
###################################################
mex.env <- envelope(boot.out = mex.qqboot)
str(mex.env)


###################################################
### Command Snippet 9.13: Q-Q Plot and Plotted Bootstrap Envelope
###################################################
my.qq <- qqnorm(y = sort(mex.z), ylim = c(-3.5, 3.5), ylab = "Empirical Quantiles", xlab = "Normal Quantiles")
abline(a = 0, b = 1, col = "red")
lines(x = my.qq$x, y = mex.env$overall[1, ])
lines(x = my.qq$x, y = mex.env$overall[2, ])

