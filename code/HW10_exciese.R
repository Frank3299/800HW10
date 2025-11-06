############################
#ZoolOgy800 Homework Week 9#
############################

# Author: Frank
# Data: Oct-30-2025


#*********************************************************
#Submission instructions 
#Submit a single URL to a public GitHub repository on Canvas.  Please make sure it works – i.e., that you can clone the repo as a project yourself.  Be sure to indicate in the submission who is in your group.  Submit a single URL for each group, but if you’re not the one submitting the URL, submit a comment mentioning the name of the person submitting for your group. 
#Problem 
#One important use of probability distributions is to test the ability of statistical tests to provide an accurate and unbiased estimate of effects of known size and determine whether they are significantly different from some reference value – usually zero (power analysis).  More broadly, the combination of simulating data from a known process and then attempting to estimate parameters of the data generation process is called a simulation-estimation framework.  Here we will begin to explore this approach using a few different probability distributions: the uniform, normal, binomial, and beta. 
#*********************************************************

#*********************************************************
#Objective 1 
#A. Use the equation for a simple linear regression to generate 100 observations of x and y.  Consistent with the assumptions of linear regression, x should be measured without error, but the observed values of y should include normally distributed error with mean zero. 
#𝑦𝑖 = 𝛼+𝛽𝑥𝑖 +𝜀𝑖. 
#where yi is an individual observation of the response (dependent) variable, α is the intercept, β is the slope, xi is an individual observation of the predictor (independent) variable, and εi is a random variable drawn from a normal distribution with mean = 0 and standard deviation = σ. 
#You can make up values for the constants α and β such that the yi  fall between anywhere betwen -100 and +100 and draw random values of xi from a uniform distribution between 0 and 10. 
#B. Using facet wrapping in ggplot, create a multipanel figure comparing plots of y vs. x in one row for  three different values of σ: 1, 10, and 25. 
#C. How does the ability to visually detect a relationship between y and x change as observation error increases (no answer or analysis needed – just think about this)
#**********************************************************
#*
library(ggplot2)
library(dplyr)

set.seed(123)

# 参数
alpha <- 13
beta <- 7
n <- 100

# 真实 X
x_true <- runif(n, 0, 10)

# X 的观测误差
x_sigma <- 1
x_error <- rnorm(n, 0, x_sigma)
x_obs <- x_true + x_error

# 不同 sigma
sigma_values <- c(1, 10, 25)

# 生成数据
data <- lapply(sigma_values, function(sigma) {
  
  y <- alpha + beta * x_true + rnorm(n, 0, sigma)  # Y 不随 X 误差变化
  
  data.frame(
    x_true = x_true,
    x_obs = x_obs,
    y = y,
    sigma = factor(sigma)
  )
}) %>% bind_rows()

# 为了方便绘制对比，将数据“长格式化”
data_long <- data %>%
  tidyr::pivot_longer(cols = c(x_true, x_obs), names_to = "X_type", values_to = "X_value")

# 绘图：对比 X 误差前后
ggplot(data_long, aes(x = X_value, y = y, color = X_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(group = X_type), method = "lm", se = FALSE) +
  geom_segment(data = data, aes(x = x_true, xend = x_obs, y = y, yend = y),
               inherit.aes = FALSE, color = "gray", alpha = 0.3) +
  facet_wrap(~ sigma, nrow = 1) +
  labs(
    title = "Comparison of Y vs X: Before and After Adding X Error",
    x = "X",
    y = "Y",
    color = "X type",
    caption = "Green: True X, Blue: Observed X, Gray arrows: X error, Each panel: different Y σ"
  ) +
  scale_color_manual(values = c("x_true" = "darkgreen", "x_obs" = "blue")) +
  theme_minimal()
