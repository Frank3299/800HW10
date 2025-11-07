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
#Ordinary least squares regression is a valuable tool, but one that comes with several important assumptions that should be checked.  Violations of these assumptions can sometimes result in diffed estimates of the regression parameters with consequences for the accuracy of predictions.

#Objective 1
#Find real data related to your research focus that fits the assumptions of ordinary least squares regression, estimate the regression parameters, and compare model2024gswearlyTref predictions at two X values.
#A. Using either your own data or data that you find in an online database find two continuous variables that might reasonably be hypothesized to have a causal association (i.e., one variable is clearly the response, Y, and the other the predictor, X) and have sufficient numbers of paired observations (> 30).
#B. Using lm(), fit a linear regression to these data.
#C. Evaluate the model2024gswearlyTref residuals for signs that regression assumptions are violated.  You should evaluate at least three assumptions and for each one state to what extent you believe it is violated and how you know.  You should plot figures and write your response as comments embedded in the code.
#D. Generate predictions and associated prediction intervals for two X values: one at the median of X and the other at the 95th percentile of X.  How do the prediction intervals differ?
  
#***********************************************************

library(ggplot2)
library(dplyr)

# read the data set
gsw2024 <- readRDS("data/north_2024_test_data_cleaned.rds")
gsw2024

#A
summary(gsw2024$gsw_early)
summary(gsw2024$Tref_early)
length(na.omit(gsw2024$gsw_early))  
length(na.omit(gsw2024$Tref_early))

#B
model2024gsw <- lm(Tref_early ~gsw_early, data = gsw2024)
summary(model2024gsw)  # check the model2024gswearlyTref


#C
par(mfrow = c(2,2))
plot(model2024gswearlyTref)  

# 1. Linear Relationship
plot(gsw2024$Tref_early, gsw2024$gsw_early, main="Scatterplot of Y vs X")
abline(model2024gswearlyTref, col="red")

#I think from the Residuals vs Fitted plot, the red loess curve is clearly curved, indicating violation of the linearity assumption.The spread of residuals is not constant across fitted values, which means could not be a fit line model. 


# 2. Normal Q-Q
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

#The Q-Q plot is has very substantial deviation from the 1 to 1 line, especially in the upper tail, indicating that residuals are strongly non-normal. I think this violates the normality assumption.

# 3. Residual Normality
hist(residuals(model2024gswearlyTref), main="Histogram of Residuals")
shapiro.test(residuals(model2024gswearlyTref))  # 正态性检验
#It is clear that the Scale-Location plot has an upward trend in the red line, suggesting increasing residual variance with fitted values. I think is indicates heteroscedasticity and aginst of the constant variance assumption, 


# 4. Homogeneity of residual variance
plot(fitted(model2024gswearlyTref), residuals(model2024gswearlyTref), main="Residuals vs Fitted")
abline(h=0, col="red")




# D. Generate Predictions and Prediction Intervals
# Calculate the median and 95th quantile of X
X_median <- median(gsw2024$Tref_early, na.rm=TRUE)
X_95 <- quantile(gsw2024$Tref_early, 0.95, na.rm=TRUE)

# 创建新数据框用于预测
newdata2024 <- data.frame(Tref_early = c(X_median, X_95))

# 生成预测和预测区间
pred <- predict(model2024gswearlyTref, newdata2024, interval = "prediction")
pred

# 95% have bigger range. 


#***********************************************************
#Objective 2
#I’ve said in class that the regression parameter estimates are fairly robust to modest deviations from normality.  However, estimates of uncertainty or more sensitive.  Evaluate whether this is true.
#A.	Generate linear regression data where the error in the response variable Y is not quite normally distributed (but still unimodal).  A lognormal or negative binomial distribution should work.  No error in X this time. 100 X, Y pairs should be good.
#B.	Fit a linear regression to the data.
#C.	Repeat this process and keep track of the true and estimated slope and intercept. 
#D.	How well do the estimated slope and intercept match the true values?
#E.	For each simulation, generate a 95% prediction interval for each X value.
#F.	What fraction of your data (Y values) falls within the 95% prediction interval?  
#G.	What does this imply for how your estimated uncertainty in the predictions compares to the true uncertainty.

#***********************************************************

#A
n <- 100          # 每次模拟 100 个点
a_true <- 7       # 真实截距
b_true <- 5       # 真实斜率

# 用 lognormal 产生偏态误差
meanlog <- 0
sdlog <- 2

X <- runif(n, 0, 25)    # make the X without error
err <- rlnorm(n, meanlog, sdlog)
err <- err - mean(err)
Y <- a_true + b_true * X + err

#B
modelO2 <- lm(X ~ Y)
summary(modelO2)  

ggplot(data.frame(Y = Y), aes(x = Y)) +
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  labs(
    title = "Histogram of Y",
    x = "Y",
    y = "Count"
  ) +
  theme_minimal()

ggplot(data.frame(X, Y), aes(x = X, y = Y)) +
  geom_point() +                                  
  geom_smooth(method = "lm", se = TRUE, color = "red") +  
  labs(
    title = "Linear Regression: X ~ Y",
    x = "x",
    y = "Y"
  ) +
  theme_minimal()

#C
simO2 <- 1000     #Simulate 1000 times

# Store Results 
est_a <- numeric(simO2)  #Save the intercept of the regression model to est_a[i].
est_b <- numeric(simO2)  #Save the slope of the regression model to est_b[i].
coverage <- numeric(simO2)

for (i in 1:simO2) {
  X <- runif(n, 0, 20) 
  err <- rlnorm(n, meanlog, sdlog)
  err <- err - mean(err)
  Y <- a_true + b_true * X + err
  modelO2_1 <- lm(Y ~ X)
  
  est_a[i] <- coef(modelO2_1)[1]   
  est_b[i] <- coef(modelO2_1)[2]
  
  #E the prediction interval for each X
  pred <- predict(modelO2_1, interval = "prediction")
  
  #f Calculate the proportion of the true Y that falls within the PI.
  coverage[i] <- mean(Y >= pred[, "lwr"] & Y <= pred[, "upr"]) 
}


# D the different between estimates and true values of slope & intercept 
mean(est_a); mean(est_b)
diff_a <- mean(est_a) - a_true
diff_b <- mean(est_b) - b_true
diff_a; diff_b
# this reslut show that the estimates slope and intercept are really have a little difference with the true values. 

#G
mean(coverage)       
sd(coverage)         
quantile(coverage, c(.05, .25, .5, .75, .95))

#It shows that most of Y is still within the estimated range.

ggplot(data.frame(coverage = coverage), aes(x = coverage)) +
  geom_histogram(bins = 15, fill = "blue", color = "black") +
  geom_vline(xintercept = 0.95, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Prediction Interval Coverage",
    x = "Coverage",
    y = "Count"
  ) +
  theme_minimal()
