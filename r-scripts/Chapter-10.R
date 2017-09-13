###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 06, 2010
### Chapter 10: Dependent Samples
###################################################



###################################################

### Command Snippet 10.1: Read in and Examine the PSAT.csv Data

###################################################

psat <- read.table(file = file.choose(), sep = ",", header = TRUE, row.names = "ID")
head(psat)
tail(psat)
str(psat)
summary(psat)




###################################################
### Command Snippet 10.2: Sort the PSAT Scores
###################################################
sort(x = psat$PSAT)

### Carry out the randomization
set.seed(100)
c(replicate(n = 20, expr = sample(c(0,1))))



###################################################

### Command Snippet 10.3: Read in and Examine the OrderedPSAT.csv Data 

###################################################

psat2 <- read.table(file = file.choose(), sep = ",", header = TRUE)
head(psat2)
tail(psat2)
str(psat2)
summary(psat2)



###################################################
### Command Snippet 10.4: Examine the Conditional Distribution of PSAT Scores 
###################################################
plot(density(psat2$PSAT[psat2$Condition == "Control"]), lty = "dashed")
lines(density(psat2$PSAT[psat2$Condition == "Treatment"]), lty = "solid")
tapply(X = psat2$PSAT, INDEX = psat2$Condition, FUN = mean)
tapply(X = psat2$PSAT, INDEX = psat2$Condition, FUN = sd)




###################################################

### Command Snippet 10.5: Read in and Examine the BlockedPSAT.csv Data and Examine the Conditional Distributions of Math Achievement

###################################################

math <- read.table(file = file.choose(), sep = ",", header = TRUE)
head(math)
tail(math)
str(math)
summary(math)
plot(density(math$Achievement[math$Condition == "Control"], bw = 4), lty = "dashed")
lines(density(math$Achievement[math$Condition == "Treatment"], bw = 4), lty = "solid" )
boxplot(math$Achievement[math$Condition == "Control"], math$Achievement[math$Condition == "Treatment"], names = c("Control", "Treatment"))
tapply(X = math$Achievement, INDEX = math$Condition, FUN = mean)
tapply(X = math$Achievement, INDEX = math$Condition, FUN = sd)





###################################################

### Command Snippet 10.6: Reshape the Math Data

###################################################

math.wide <- reshape(data = math, direction = "wide", idvar = "Block", timevar =  "Condition", v.names = c("PSAT", "Achievement"))
head(math.wide)



###################################################
### Command Snippet 10.7: Simplify the Wide Data Frame
###################################################
math2 <- math.wide[ ,c(3, 5)]



###################################################

### Command Snippet 10.8: Permute the Data Within Each Row

###################################################

apply(X = math2, MARGIN = 1, FUN = sample)



###################################################
### Command Snippet 10.9: Permute the Data Within Each Row and Compute the Means
###################################################

apply(X = apply(X = math2, MARGIN = 1, FUN = sample), MARGIN = 1, FUN = mean)



###################################################
### Command Snippet 10.10: Permute the Data Within Each Row, Compute the Means, and Compute the Difference in Means
###################################################

diff(apply(X = apply(X = math2, MARGIN = 1, FUN = sample), MARGIN = 1, FUN = mean))





###################################################

### Command Snippet 10.11: Randomization Test for Matched Data

###################################################

permuted <- replicate(n = 4999, expr = diff(apply(X = apply(X = math2, MARGIN = 1, FUN = sample), MARGIN = 1, FUN = mean)))
plot(density(permuted), xlab = "Permuted Mean Difference", main = " ")
mean(permuted)
sd(permuted)
length(permuted[abs(permuted) >= 2.8])
(211 + 1) / (4999 + 1)






###################################################

### Command Snippet 10.12: Randomization Test for Matched Data Under the Incorrect Assumption of Independence

###################################################

all.math <- c(math2$Achievement.Control, math2$Achievement.Treatment)
permuted.ind <- replicate (n = 4999, expr = sample(all.math))
mean.diff <- function (data) {
  mean(data[1:20]) - mean(data[21:40])
  }
diffs <- apply(X = permuted.ind , MARGIN = 2, FUN = mean.diff)
length(diffs[abs(diffs) >= 2.8])
(928 + 1) / (4999 + 1)





###################################################

### Command Snippet 10.13: Coerce the math2 Data Frame into a Time Series Object

###################################################

ts.math <- as.ts(math2)



###################################################
### Command Snippet 10.14: Function to Compute Glass' Delta
###################################################

glass.delta <- function(data){
  numerator <- abs(mean(data[ ,2]) - mean(data[ ,1]))
  denominator <- sd(data[ ,1])
  numerator / denominator
  } 
glass.delta(ts.math)



###################################################
### Command Snippet 10.15: Block Bootstrap
###################################################

library(boot)
match.boot <- tsboot(tseries = ts.math, statistic = glass.delta, R = 4999, sim = "fixed", l = 2) 
plot(density(match.boot$t))
mean(match.boot$t)
sd(match.boot$t)
match.boot




###################################################
### Command Snippet 10.16: Percentile Bootstrap Interval
###################################################

boot.ci(match.boot, type = "perc")





