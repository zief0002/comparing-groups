###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 03, 2010
### Chapter 02: Data Representation and Preparation
###################################################


###################################################
### Command Snippet 2.1/2.2: Find Path Name
###################################################
file.choose()


###################################################
### Command Snippet 2.3: Read in Latino Data
###################################################
latino <- read.table(file = "Documents/Data/LatinoEd.csv", header = TRUE, sep = ",")


###################################################
### Command Snippet 2.4: Find Path Name
###################################################
head(latino)
tail(latino)


###################################################
### Command Snippet 2.5: Read in Latino Data Using ID for Row Names
###################################################
latino <- read.table(file = "Documents/Data/LatinoEd.csv", header = TRUE, sep = ",", row.names = "ID")
head(latino)
tail(latino)


###################################################
### Command Snippet 2.6: Examine Structure of Data Frame
###################################################
str(latino)


###################################################
### Command Snippet 2.7: Summary of Data Frame
###################################################
summary(latino)


###################################################
### Command Snippet 2.8: Box-and-Whiskers Plot Produces an Error
###################################################
boxplot(Achieve)


###################################################
### Command Snippet 2.9: Box-and-Whiskers Plot and Density Plot
###################################################
boxplot(latino$Achieve)
plot(density(latino$Achieve))


###################################################
### Command Snippet 2.10: Save a PDF File of the Density Plot
###################################################
pdf(file = "/Desktop/VLSS-Age.pdf")
plot(density(latino$Achieve))
dev.off()


###################################################
### Command Snippet 2.11: Evaluate a Logical Expression
###################################################
latino$ImmAge >= 4


###################################################
### Command Snippet 2.12: Evaluate Multiple Logical Expressions
###################################################
latino$ImmAge >= 4 & latino$ImmAge <= 7


###################################################
### Command Snippet 2.13: Subset of the Latino Data Frame
###################################################
latino.sub <- subset(latino, subset = latino$ImmAge >= 4 & latino$ImmAge <= 7)
summary(latino.sub)


###################################################
### Command Snippet 2.14: Contingency Table
###################################################
my.tab <- table(latino$English)  # Categorical data
my.tab


###################################################
### Command Snippet 2.15: Contingency Table of Proportions
###################################################
prop.table(my.tab)


###################################################
### Command Snippet 2.16: Two-Way Contingency Table
################################################### 
my.tab.2 <- table(latino$English, latino$Mex, dnn=c("English", "Mex"))
my.tab.2
prop.table(my.tab.2) 


###################################################
### Command Snippet 2.17: Bar Plots
################################################### 
barplot(height = my.tab, names.arg = c("Not Fluent", "Fluent"))
barplot(height = prop.table(my.tab), names.arg = c("Not Fluent", "Fluent"))



