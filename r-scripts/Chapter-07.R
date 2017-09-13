###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 05, 2010
### Chapter 07: Bootstrap Tests
###################################################





###################################################

### Command Snippet 7.1: Read in and Examine LatinoEd.csv Data 

###################################################

latino <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")

plot(density(latino$Achieve[latino$Mex == 0], bw = 5.5), main= " ", xlab = "Educational Achievement", bty = "l", lty = "dashed", xlim = c(0, 100))
lines(density(latino$Achieve[latino$Mex == 1], bw = 5.5), lty = "solid")
legend(x = 5, y = 0.030, legend = c("Non-Mexican Immigrants", "Mexican Immigrants"), lty = c("dashed", "solid"), bty = "n")
boxplot(latino$Achieve[latino$Mex == 0], latino$Achieve[latino$Mex == 1], horizontal = TRUE)
tapply(X = latino$Achieve, INDEX = latino$Mex, FUN = mean)
tapply(X = latino$Achieve, INDEX = latino$Mex, FUN = sd)
table(latino$Mex)



###################################################

### Command Snippet 7.2: Standardize the Achievement Scores

###################################################

latino$z.achieve <- scale(x = latino$Achieve)
head(latino)
mean(latino$z.achieve)
sd(latino$z.achieve)
tapply(X = latino$z.achieve, INDEX = latino$Mex, FUN = mean)



###################################################
### Command Snippet 7.3: Function for Sampling Observations from N(0, 1) Distribution
###################################################
lat.gen <- function(data, mle){
  rnorm(n = length(data), mean = 0, sd = 1)
  } 




###################################################

### Command Snippet 7.4: Function to Compute Mean Difference

###################################################

mean.diff <- function(data, indices) {
	mean(data[1:34]) - mean(data[35:150])
	} 





###################################################

### Command Snippet 7.5: Use the boot( ) function

###################################################

library(boot)
par.boot <- boot(data = latino$z.achieve, statistic = mean.diff, R = 4999, sim = "parametric", ran.gen = lat.gen)



###################################################
### Command Snippet 7.6: Examine Structure of par.boot
###################################################
str(par.boot)





###################################################

### Command Snippet 7.7: Examine the Bootstrap Distribution

###################################################

plot(density(par.boot$t))

abline(v = 0)

mean(par.boot$t)

sd(par.boot$t)



###################################################
### Command Snippet 7.8: Count the Number of Bootstrapped Mean Differences at Least as Extreme as the Observed Result
###################################################

length(par.boot$t[abs(par.boot$t) >= 0.39])





###################################################

### Command Snippet 7.9: Plot of Kernel Density of Achievement with Variability Bands

###################################################

library(sm)

sm.density(latino$Achieve[latino$Mex == 0], model = "normal", rugplot = FALSE)
sm.density(latino$Achieve[latino$Mex == 1], model = "normal", rugplot = FALSE)





###################################################

### Command Snippet 7.10: Compute Mean Difference - Nonparametric Bootstrap

###################################################

mean.diff.np <- function(data, indices) {
	d <- data[indices, ]
	mean(d$Achieve[1:34]) - mean(d$Achieve[35:150])
	}  

nonpar.boot <- boot(data = latino, statistic = mean.diff.np, R = 4999)



###################################################
### Command Snippet 7.11: Examine the Bootstrap Distribution
###################################################

plot(density(nonpar.boot$t))
abline(v=0)
mean(nonpar.boot$t)
sd(nonpar.boot$t)

length(nonpar.boot$t[abs(nonpar.boot$t) >= 5.92])





###################################################

### Command Snippet 7.12: Compute Studentized Mean Difference (t-Statistic)

###################################################

tapply(X = latino$Achieve, INDEX = latino$Mex, FUN = mean)
tapply(X = latino$Achieve, INDEX = latino$Mex, FUN = var)
table(latino$Mex)
numerator <- 64.5 - 58.6
numerator
pool.var <- (33 * 169.8 + 115 * 244.2) / 148
pool.var
denominator <- sqrt(pool.var * (1 / 34 + 1 / 116))
denominator
t <- numerator/denominator
t




###################################################

### Command Snippet 7.13: Function to Compute Studentized Mean Difference, Carry Out Nonparametric Bootstrap, Examine Bootstrap Distribution, and Compute p-Value

###################################################

studentized.mean.diff <- function(data, indices) {
	d <- data[indices, ]
	nonmex <- d$Achieve[1:34]
	mex <- d$Achieve[35:150]
	num <- mean(nonmex) - mean(mex)
	pool.var <- (33 * var(nonmex) + 115 * var(mex)) / 148
	den <- sqrt(pool.var * (1 / 34 + 1 / 116))
	num/den
	} 

studentized.boot <- boot(data = latino, statistic = studentized.mean.diff, R = 4999)
plot(density(studentized.boot$t), xlab="Bootstrapped t-Statistic", main="")
mean(studentized.boot$t)
sd(studentized.boot$t)
length(studentized.boot$t[abs(studentized.boot$t) >= 2.01])
(219 + 1) / (4999 + 1)




###################################################

### Command Snippet 7.14: Test mean.diff()

###################################################

mean.diff(latino$z.achieve)



###################################################
### Command Snippet 7.15: Order the Mex Variable
###################################################

order(latino$Mex)



###################################################
### Command Snippet 7.16: Order the Rows of the latino Data Frame Using the Mex Variable and Assign the Ordered Data Frame to a New Object 
###################################################

ordered.latino <- latino[order(latino$Mex), ]

head(ordered.latino)

tail(ordered.latino)



###################################################
### Command Snippet 7.17: Test mean.diff() Using Ordered Data Frame 
###################################################

mean.diff(ordered.latino$z.achieve)









