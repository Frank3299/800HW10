############################
#ZoolOgy800 Homework Week 10#
############################

# Author: Frank
# Data: Nov-06-2025


#*********************************************************
#Homework Week 10
#Submission instructions
#Submit a single URL to a public GitHub repository on Canvas.  Please make sure it works – i.e., that you can clone the repo as a project yourself.  Be sure to indicate in the submission who is in your group.  Submit a single URL for each group, but if you’re not the one submitting the URL, submit a comment mentioning the name of the person submitting for your group.

#*********************************************************

#*********************************************************
#Problem
#Ordinary least squares regression is a valuable tool, but one that comes with several important assumptions that should be checked.  Violations of these assumptions can sometimes result in biased estimates of the regression parameters with consequences for the accuracy of predictions.

#Objective 1
#Find real data related to your research focus that fits the assumptions of ordinary least squares regression, estimate the regression parameters, and compare model predictions at two X values.
#A. Using either your own data or data that you find in an online database find two continuous variables that might reasonably be hypothesized to have a causal association (i.e., one variable is clearly the response, Y, and the other the predictor, X) and have sufficient numbers of paired observations (> 30).
#B. Using lm(), fit a linear regression to these data.
#C. Evaluate the model residuals for signs that regression assumptions are violated.  You should evaluate at least three assumptions and for each one state to what extent you believe it is violated and how you know.  You should plot figures and write your response as comments embedded in the code.
#D. Generate predictions and associated prediction intervals for two X values: one at the median of X and the other at the 95th percentile of X.  How do the prediction intervals differ?
  
#***********************************************************

library(ggplot2)
library(dplyr)

# read the data set
gsw2024 <- readRDS("../../data/north_2024_test_data_cleaned.rds")

gsw2024

# Constants
alpha <- 13   # intercept
beta <- 7    # slope
n <- 100     # number of observations
x <- runif(n, 0, 10)  # x from uniform distribution 0-10

# Define different sigma values
sigma_values <- c(1, 10, 25)

# Generate y values for each sigma
data <- lapply(sigma_values, function(sigma) {
  y <- alpha + beta * x + rnorm(n, mean = 0, sd = sigma)
  data.frame(x = x, y = y, sigma = factor(sigma))
}) %>% bind_rows()


ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ sigma, nrow = 1) +
  labs(
    title = "Effect of Increasing Observation Error",
    x = "Predictor (x)",
    y = "Response (y)",
    caption = "σ = standard deviation of random error"
  ) +
  theme_minimal()

# C. When εi or σ is very small, then x and y have very strong correlation. When σ is bigger, looks they have bigger variance. 

#***********************************************************
#Objective 2 
#Recalling Don Corleone’s loaded coin, we’re interested in knowing how many coin flips are required to be able to consistently determine whether the coin is unfair (p(heads)>0.5) for different degrees of unfairness.  
#A. Using simulations of coin flips (Bernoulli trials), plot the probability (number of times out of 100) that you determine the coin is significantly unfair (alpha < 0.05) for 1 to 20 coin flips when p = 0.55. 
#B. Repeat this analysis for p = 0.6 and p = 0.65 and add these lines to the plot from A.
#*********************************************************

test_coin <- function(p, n, reps = 100){
  count_sig <- 0
  
  for(i in 1:reps){
    # Simulate n coin tosses (1 = heads)
    x <- rbinom(n, size = 1, prob = p)
    
    # Perform a binomial test (to test whether p is greater than 0.5).
    test <- binom.test(sum(x), n, p = 0.5, alternative = "greater")
    
    if(test$p.value < 0.05){
      count_sig <- count_sig + 1
    }
  }
  
  return(count_sig)
}

#----------------------------------------------
# A. p = 0.55，samples from 1 to 20
#----------------------------------------------

p1 <- 0.55
results_p55 <- sapply(1:20, function(n) test_coin(p1, n))

#----------------------------------------------
# B. p = 0.60 & p = 0.65
#----------------------------------------------

p2 <- 0.60
p3 <- 0.65

results_p60 <- sapply(1:20, function(n) test_coin(p2, n))
results_p65 <- sapply(1:20, function(n) test_coin(p3, n))



plot(1:20, results_p55, type="l", lwd=2, col="blue",
     xlab="Number of flips", ylab="Times significant (out of 100)",
     ylim=c(0, 50),             
     main="Detection Power vs Number of Coin Flips")


lines(1:20, results_p60, lwd=2, col="red")
lines(1:20, results_p65, lwd=2, col="purple")

legend("topleft",
       legend=c("p = 0.55", "p = 0.60", "p = 0.65"),
       col=c("blue", "red", "purple"), lwd=2)
