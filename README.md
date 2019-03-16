# Assignment-8.2
library(mlbench)
#load the dataset
data("BostonHousing")
head(BostonHousing)
#library
library(lars)
x <- as.matrix(BostonHousing[,c(1:3,5:13)])
y <- as.matrix(BostonHousing[,14])

fit <- lars(x, y, type="lasso")

summary(fit)

#result
fit

best_step <- fit$df[which.min(fit$RSS)]
best_step

predictions <- predict(fit, x, s=best_step, type="fit")$fit

mse <- mean((y - predictions)^2)
print(mse)


library(glmnet)


fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
fit

summary(fit)

predictions <- predict(fit, x, type="link")

mse <- mean((y - predictions)^2)
print(mse)


# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
fit
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
mse <- mean((y - predictions)^2)
print(mse)

#Multivariate Adaptive Regression Splines (MARS) is a non-parametric 
#regression method that models multiple nonlinearities in data using 
#hinge functions (functions with a kink in them)
# load the package
install.packages('earth')
library(earth)

# fit model
fit <- earth(medv~.,data=BostonHousing)
fit
# summarize the fit
summary(fit)
# summarize the importance of input variables
evimp(fit)
# make predictions
predictions <- predict(fit, BostonHousing)
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

#stepwise regression model
# fit model
base <- lm(medv~., BostonHousing)
base
# summarize the fit
summary(base)
# perform step-wise feature selection
fit <- step(base)
fit
# summarize the selected model
summary(fit)
# make predictions
predictions <- predict(fit, BostonHousing)
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

#Principal Component Regression (PCR) creates a linear regression model 
#using the outputs of a Principal Component Analysis (PCA) to estimate 
#the coefficients of the model. PCR is useful when the data has highly 
#correlated predictors.
# load the package
install.packages('pls')
library(pls)

# fit model
fit <- pcr(medv~., data=BostonHousing, validation="CV")
fit
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, BostonHousing, ncomp=6)

mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

#PLS is appropriate for data with highly-correlated predictors.
#Partial Least Squares (PLS) Regression creates a linear model of the 
#data in a transformed projection of problem space

library(pls)

fit <- plsr(medv~., data=BostonHousing, validation="CV")
fit

summary(fit)

predictions <- predict(fit, BostonHousing, ncomp=6)

mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

 
#compelling reasons to exclude outliers in your data
library(MASS)
rmod <- rlm (medv ~ ., psi = psi.huber, data = BostonHousing) 
rmod

summary(rmod) # model summary

predictions<-predict(rmod, BostonHousing) # apply model to predict response on test data


mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)
