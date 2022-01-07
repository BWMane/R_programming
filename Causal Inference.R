
# Exercise 1: 
data(mtcars)

# mpg: Miles/(US) gallon
# cyl: Number of cylinders
# disp: Displacement (cu.in.)
# hp: Gross horsepower
# drat: Rear axle ratio
# wt: Weight (1000 lbs)
# qsec: 1/4 mile time
# vs: V/S
# am: Transmission (0 = automatic, 1 = manual)
# gear: Number of forward gears
# carb: Number of carburetors

# Compute the unconditional expectation of mpg
lm(mpg ~ 1, data=mtcars)

# Compute the conditional expectation of mpg, conditional on number of cylinders and number of forward gears
lm(mpg ~ 1 + cyl + gear, data=mtcars)

# Compute the conditional expectation of mpg, conditional on number of cylinders, number of forward gears, and weight (1000 lbs)
std = function(Y) (Y-mean(Y))/sd(Y)
std.mtcars <- as.data.frame(lapply(mtcars[,c('mpg','cyl','gear','wt')], std))
b <- coef(lm(mpg ~ 1 + cyl + gear + wt, data=std.mtcars))
round(b, 5)

# Compute the conditional expectation from the previous question separately for cars with manual and automatic transmission
manual.mtcars <- subset(mtcars, am==0)
lm(mpg ~ 1 + cyl + gear + wt, data=manual.mtcars)

automatic.mtcars <- subset(mtcars, am==1)
lm(mpg ~ 1 + cyl + gear + wt, data=automatic.mtcars)

# Exercise 5
data <- read.csv("test_data.csv")

# There are multiple ways to write such a loop, here are two examples:

  # Option 1
  est <- list()
  for (i in 1:2) {
    est[[i]] <- summary(lm(test_score ~ 1 + treatment, subset=(block==i), data=data))
    print(est)
  }
  
  # Option 2
  require(arm) # This package has a nice display() function for lm() models
  for (i in 1:2) {
    display(lm(test_score ~ 1 + treatment, subset=(block==i), data=data))
  }
  
  # Average treatment effect
  summary(lm(test_score ~ 1 + treatment + factor(block), data=data))
  