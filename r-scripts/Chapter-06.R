###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### October 15, 2011
### Chapter 06: Randomization and Permutation Tests
###################################################





###################################################
### Command Snippet 6.1: Read in and Examine the AfterSchool.csv Data 
###################################################

asp <- read.table(file = file.choose(), header = TRUE, sep = ",", row.names = "ID")
head(asp)
tail(asp)
str(asp)
summary(asp)

# Plot
plot(density(asp$Delinq[asp$Treatment == 1], bw = 3), 
	col = "#377EB8", lty = "solid",
	xlab= "T-Scaled Delinquency Measure", 
	main= " ", bty = "l")
lines(density(asp$Delinq[asp$Treatment == 0], bw = 3), 
	col = "#E41A1C", lty ="dashed")
legend(x = 70, y = 0.085, 
	legend = c("Control Group", "Treatment Group"), 
	col = c("#E41A1C", "#377EB8"), 
	lty = c("dashed", "solid"), 
	bty = "n")

# Summary measures	
tapply(X = asp$Delinq, INDEX = asp$Treatment, FUN = mean)
tapply(X = asp$Delinq, INDEX = asp$Treatment, FUN = sd)
table(asp$Treatment)





###################################################
### Command Snippet 6.2: Single Random Permutation of the Data
###################################################

permuted <- sample(asp$Delinq)

head(permuted)
tail(permuted)
summary(permuted)





###################################################
### Command Snippet 6.3: Mean Difference in the Permuted Data
###################################################

mean(permuted[1:169]) - mean(permuted[170:356])



###################################################
### Command Snippet 6.4: Carry Out 4999 Random Permutations
###################################################

permuted <-replicate(n = 5000, expr = sample(asp$Delinq))





###################################################
### Command Snippet 6.5: Function to Compute Mean Difference
###################################################

mean.diff <- function(data){
  mean(data[1:169]) - mean(data[170:356]);
  }



###################################################
### Command Snippet 6.6: Test mean.diff() Function
###################################################

mean.diff(asp$Delinq)



###################################################
### Command Snippet 6.7: Apply mean.diff() Function to the Permuted Data
###################################################

diffs <- apply(X = permuted, MARGIN = 2, FUN = mean.diff)





###################################################
### Command Snippet 6.8: Plot and Summarize Randomization Distribution
###################################################

plot(density(diffs), 
	xlab = "Mean Difference", ylab = "Density",
	main = " ")

summary(diffs)
sd(diffs)





###################################################
### Command Snippet 6.9: Count Elements More Extreme than Observed Result
###################################################

length(diffs[diffs <= -0.17])
length(diffs[diffs >=  0.17])

# or...
length(diffs[abs(diffs) >= 0.17])





###################################################
### Command Snippet 6.10: Permute Difference in Variances
###################################################

var.diff <- function ( data ) {
  var (data[1:169]) - var(data[170:356]);
  }

var.diffs <- apply (X = permuted , MARGIN = 2, var.diff )
length (var.diffs [abs(var.diffs) >= 0.31])





