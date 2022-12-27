---
  title: "HW5"
author: "Luke Geel"
date: "3/11/2021"
output: html_document
---

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
copier_spring <- loadRData("/Users/lukegeel/Downloads/Homework-20210223/copier_spring2021.RData")
patient_satisfaction <- loadRData("/Users/lukegeel/Downloads/Homework-20210223/patient_satisfaction_spring2021.RData")
gpa <- loadRData("/Users/lukegeel/Downloads/gpa_spring2021.RData")
grocery <- loadRData("/Users/lukegeel/Downloads/Homework-20210223/grocery_spring2021.RData")



#1.23
#a. The sum of residuals is 2.67e-15 which is incredibly close to 0.
#b. Sigma squared: 0.74 Sigma: 0.86 These are expressed in GPA.
library(tidyverse)
view(gpa)
model <- lm(gpa)
resids <- resid(model)
sum(resids)
view(resids)
var(resids)
sd(resids)

#1.24
#a. Sum of the squared residuals: 5495.74. This is equal to the minimum quantity Q.
#b. Sigma is 11.3 and sigma squared is 127.8.These are expressed in minutes.

model1 <- lm(copier_spring)
resids1 <- resid(model1)
sqresids1 <- (resids1)^2
sum(sqresids1)

x <- data$X
y <- data$Y
n <- length(y)
X <- cbind(rep(1,n),x)
Xty <- t(x)%*%y
XtX <- t(x)%*%x
XtX.inv <- solve(XtX)
b <- xtx.inv%*%Xty
y.hat <- x%*%b
e <- y-y.hat
e_sum=sum(e)
s.sq <- sum(e^2)/(n-2)

#1.27
#a. y_hat=158.8-1.23x. The linear regression function seems like it fits the data well.
#b.Difference in means for women differing in age by 1 year: 1.23
Mean mass for women 60 years old = 73.8
residual for the eigth case: -2.39
Estimate sigma squared: 75.28

x <- data$X
y <- data$Y
n <- length(y)
X <- cbind(rep(1,n),x)
Xty <- t(x)%*%y
XtX <- t(x)%*%x
XtX.inv <- solve(XtX)
b <- xtx.inv%*%Xty
y_hat <- x%*%b
e <- y-y_hat
e_sum=sum(e)
s.sq <- sum(e^2)/(n-2)

linmod <- lm(y~x)
b0 <- linmod$coef[1]
b1 <- linmod$coef[2]
plot(x,y)
abline(a=b0,b=b1)

#1.36
#∑yiei = ∑(b0+b1xi)ei = b0∑ei+b1∑xiei
#b0∑ei = 1.17
#b1∑xiei = -1.17
#Thus b0∑ei+b1∑xiei = 0

#1.43
#a. 1.y_hat = -87.3 - 0.002x1
#2. y_hat = -87.3 + 0.446x2
#3. y_hat = -87.3 + 0.132x3 
#b. Yes I believe that the linear regression model is a good fit for each of the three predictor variables.
#c.

y <- data$x8
x1 <- data$x5
x2 <- data$x9
x3 <- data$x16

linmod <- lm(y~x1+x2+x3)
b0 <- linmod$coef[1]
b1 <- linmod$coef[2]
b2 <- linmod$coef[3]
b3 <- linmod$coef[4]

plot(x1,y)
abline(a=b0,b=b1)
plot(x2,y)
abline(a=b0,b=b1)
plot(x3,y)
abline(a=b0,b=b1)

#6.27
#a. b = 33.93
#b. e = -2.7, -1.23, -1.64, -1.33, -0.09, 6.99
#c.
#d. Sum of squared residuals = 3009.9
#e. s^2 = 614.4
#f. y = 33.93 + 2.78 * 10 - 0.264 * 30 = 33.81
#g. s^2(y) = 614.4

y <- c(42,33,75,28,91,55)
x1 <- c(7,4,16,3,21,8)
x2 <- c(33,41,7,49,5,31)

n <- length(y)
x <- cbind(rep(1,n),x1,x2)
Xty <- t(x)%*%y
XtX <- t(x)%*%X
XtX.inv <- solve(XtX)
b <- XtX.inv%*%Xty
b0 <- b[1]
b1 <- b[2]
b2 <- b[3]

y_hat <- X%*%b
e <- y-y_hat
y_mean <- mean(y)
SSR <- sum((y_hat-y_mean)^2)
H <- X%*%XtX.inv%*%XtX.inv
s.2 <- sum((y-mean(y))^2)/(n-1)


#6.28
#c. 1: y = -9.75 + 0.001x1 + 0.07x2 + 0.089x3
#2: y = -165.6 + 0.103x1 + 5.95x2 + 0.13x3
#d. Because R^2 is larger for the second model, it means that the second model is preferred as the data fits it better.


y <- data$X8
x1 <- data$X5
x2 <- data$X4
x3 <- data$X16
x4 <- (x1/x2)
x5 <- data$X7

linmod1 <- lm(y~x1+x2+x3)
linmod2 <- lm(y~x3+x4+x5)
b0 <- linmod$coef[1]
b1 <- linmod$coef[2]
b2 <- linmod$coef[3]
b3 <- linmod$coef[4]

b02 <- linmod2$coef[1]
b12 <- linmod2$coef[2]
b22 <- linmod2$coef[3]
b32 <- linmod2$coef[4]

n <- length(y)
X1 <- cbind(rep(1,n),x1,x2,x3)
X2 <- cbind(rep(1,n),x3,x4,x5)
X1ty <- t(X1)%*%y
X2ty <- t(X2)%*%y
X1tX1 <- t(X1)%*%X1
X2tX2 <- t(X2)%*%X2
X1tX1.inv <- solve(X1tX2)
X2tX2.inv <- solve(X2tX2)
b1 <- X1tX1.inv%*%X1ty
b2 <- X2tX2.inv%*%X2ty

y1_hat <- X1%*%b1
y2_hat <- X2%*%b2
y_mean = mean(y)
SSR1 <- sum((y1_hat-y_mean)^2)
SSR2 <- sum((y2_hat-y_mean)^2)
SST1 <- sum((y-y_mean)^2)
R2_1 <- SSR1/SST1
R2_2 <- SSR2/SST1
```

