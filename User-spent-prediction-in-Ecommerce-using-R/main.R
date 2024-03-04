# ---------------------------------------------------------------------------
# IMPORT DATA AND SETUP
# ---------------------------------------------------------------------------
data <- read.csv("./data/customer-data")

str(data)
summary(data)

# ---------------------------------------------------------------------------
# CREATE PLOTS AND SEARCH FOR CORRELATIONS
# ---------------------------------------------------------------------------
library(ggplot2)

# Is there a correlation between Time on Website & Yearly Amount Spent?
ggplot(data, aes(x=Time.on.Website, y=Yearly.Amount.Spent)) + 
  geom_point(colour="orange") + 
  ggtitle("Time on website against vs Yearly amount spent") + 
  xlab("Time on Website") +
  ylab("Yearly Amount Spent")

# Is there a correlation between Avg Session Length & Yearly Amount Spent?
ggplot(data, aes(x=Avg..Session.Length, y=Yearly.Amount.Spent)) + 
  geom_point(colour="orange") +
  ggtitle("Average session length against vs Yearly amount spent") + 
  xlab("Time on Website") +
  ylab("Yearly Amount Spent")

# pairplot of all continuous variables --> 
#### length of membership seems the most correlated
pairs(data[c("Avg..Session.Length", 
             "Time.on.App", 
             "Time.on.Website", 
             "Length.of.Membership",
             "Yearly.Amount.Spent")],
      col = "orange",
      pch = 16,
      labels = c("Avg Session Length", 
                 "Time on App", 
                 "Time on website",
                 "Length of Membership",
                 "Yearly spent"),
      main = "Pairplot of variables")

# ---------------------------------------------------------------------------
# EXPLORING THE SELECTED VARIABLES
# ---------------------------------------------------------------------------

# is the variable normally distributed?
hist(data$Length.of.Membership)
# with ggplot
ggplot(data, aes(x=Length.of.Membership)) + 
  geom_histogram(
    color= "white", 
    fill="orange",
    binwidth = 0.5)

# check distribution with boxplot
boxplot(data$Length.of.Membership)
# with ggplot
ggplot(data, aes(x=Length.of.Membership)) + 
  geom_boxplot(
    fill="orange",
  )

# ---------------------------------------------------------------------------
# FITTING A LINEAR MODEL
# ---------------------------------------------------------------------------

attach(data)

lm.fit1 <- lm(Yearly.Amount.Spent~Length.of.Membership)

summary(lm.fit1)
# --> p value is below significance level, so the variable Length of 
#     membership is significant.
# --> beta_1 is 64.219, which means that an increase in the variable value
#     causes an increase in the target variable.

plot(Yearly.Amount.Spent~Length.of.Membership)
abline(lm.fit1, col="red")

# ---------------------------------------------------------------------------
# RESIDUALS ANALYSIS
# ---------------------------------------------------------------------------

qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1), col="red")

shapiro.test(residuals(lm.fit1))
# --> the p value is > 0.05 so Ho cannot be rejected. The residuals 
#     distribution is normal.
# --> normality of residuals is an assumption of the linear model.
#     this means that the chosen model works correctly.

# ---------------------------------------------------------------------------
# EVALUATE THE QUALITY OF THE MODEL
# ---------------------------------------------------------------------------

# create a random training and a testing set
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

# estimate the linear fit with the training set
lm.fit0.8 <- lm(Yearly.Amount.Spent~Length.of.Membership, data=train)
summary(lm.fit0.8)

# predict on testing set
prediction0.8 <- predict(lm.fit0.8, newdata = test)
err0.8 <- prediction0.8 - test$Yearly.Amount.Spent
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE=rmse,mape=mape,R2=summary(lm.fit0.8)$r.squared) # to print the 3 parameters

# ---------------------------------------------------------------------------
# MULTIPLE REGRESSION
# ---------------------------------------------------------------------------
attach(data)
lm.fit <- lm(Yearly.Amount.Spent~Avg..Session.Length +
                                    Time.on.App + 
                                    Time.on.Website +
                                    Length.of.Membership)

summary(lm.fit)

# --------------
# findings :
# 3 of the 4 variables studied seem to have an positive impact on the
# response variable. the most important remains length of membership, with 
# a coefficient 1.5 and 2.4 higher than Time on App and Avg Session Length 
# respectively. 
# Time on website seems to have little impact in the response. 

# ---------------------------------------------------------------------------
# EVALUATE THE MULTIPLE REGRESSION MODEL
# ---------------------------------------------------------------------------

# create a random training and a testing set
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

# estimate the linear fit with the training set
multi.lm.fit0.8 <- lm(Yearly.Amount.Spent~Avg..Session.Length +
                        Time.on.App + 
                        Time.on.Website +
                        Length.of.Membership, 
                      data=train)
summary(multi.lm.fit0.8)

# predict on testing set
prediction.multi0.8 <- predict(multi.lm.fit0.8, newdata = test)
err0.8 <- prediction.multi0.8 - test$Yearly.Amount.Spent
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE=rmse,mape=mape,R2=summary(lm.fit0.8)$r.squared) # to print the 3 parameters

# --------
# findings
# by using a multiple linear model, we have created a much more accurate
# predictor of the response variable. R2 went from 0.65 to 0.98
# and the RSE went from 47.14 to 9.97 dollars.




