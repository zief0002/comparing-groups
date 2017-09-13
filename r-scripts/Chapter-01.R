###################################################
### Comparing Groups: Randomization and Bootstrap Methods Using R 
### Andrew S. Zieffler, Jeffrey Harring, and Jeffrey D. Long
### December 03, 2010
### Chapter 01: An Introduction to R 
###################################################

###################################################
### Command Snippet 1.1: Installing Packages
###################################################
install.packages(pkgs = "boot", dependencies = TRUE)


###################################################
### Command Snippet 1.2: Loading Packages
###################################################
library(boot)


###################################################
### Command Snippet 1.3: Install WRS Package
###################################################
install.packages(pkgs = "WRS", dependencies = TRUE, repos = "http://R-Forge.R-project.org/")


###################################################
### Command Snippet 1.4: Arithmetic Computations
###################################################
3 + 2 
4 * 5  
10 ^ 3  
1 / 0 


###################################################
### Command Snippet 1.5: Computations using Functions
###################################################
sqrt(100)  
log(7)  
sin(50)  
exp(3) 


###################################################
### Command Snippet 1.6: Arguments 
###################################################
log(100, 10)
log(10, 100)
log(x = 100, base = 10)
log(base = 10, x = 100) 
log(100, base = 10)   



###################################################
### Command Snippet 1.7 : Errors and Warnings
###################################################
log(X)
sqrt(-3)


###################################################
### Command Snippet 1.8: Chaining Computations 
###################################################
sqrt(sin(pi / 2))
sqrt(log(100, base = 10))


###################################################
### Command Snippet 1.9: Assignment 
###################################################
chili <- log(100, base = 10)
sqrt(chili)
chili <- 3
sqrt(chili)
chili <-  25
sqrt(chili)
chili
ls()


###################################################
### Command Snippet 1.10: Collections 
###################################################
ages <- c(1, 6, 7, 7, 10)
ages


###################################################
### Command Snippet 1.11: Collections and Sequences
###################################################
X <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
X
X <- seq(from = 2, to = 24, by = 2)
X


###################################################
### Command Snippet 1.12: More Sequences
###################################################
X <- 1:10
X
X <- seq(from = 1, to = 10, by = 1)
X


###################################################
### Command Snippet 1.13: Collections with Repeated Elements
###################################################
X <- rep(1, times = 10)
X


###################################################
### Command Snippet 1.14: Collections with Multiple Repeated Elements
###################################################
X <- rep(c(1, 0), times = c(10, 15))
X


###################################################
### Command Snippet 1.15: Functions Applied to Collections 
###################################################
ages + 5
mean(ages)


###################################################
### Command Snippet 1.16: Character and Logical Vectors 
###################################################
educational.level <- c("High School", "College", "Some College", "College", "College" )
logical.vector <- c(FALSE, TRUE, FALSE, TRUE, TRUE)


###################################################
### Command Snippet 1.17: R Help
###################################################
?cor
help(correlation)
??correlation


###################################################
### Command Snippet 1.18: Matrices 
###################################################
X <- 1:12
X
A <- matrix(data = X, nrow = 3, ncol = 4, byrow = FALSE)
A
B <- matrix(data = X, nrow = 3, ncol = 4, byrow = TRUE)
B


###################################################
### Command Snippet 1.19: Indexing
###################################################
B[2, 4]
B[2, 4] <- 50
B


###################################################
### Command Snippet 1.20: Matrix Addition/Subtraction
###################################################
A + B
A - B


###################################################
### Command Snippet 1.21: Scalar Multiplication
###################################################
3 * A


###################################################
### Command Snippet 1.22: Matrix Multiplication
###################################################
X <- c(0, 10, 6, 5)
A <- matrix(data = X, nrow = 2, ncol = 2, byrow = TRUE)
A
Y <- c(3, 2, 7, 9)
B <- matrix(data = Y, nrow = 2, ncol = 2, byrow = TRUE)
B
A %*% B

