## In-Class

library(MASS)
set.seed(8)
x= mvrnorm(100, mu, sigma)
pca= prcomp(x)
plot(x, xlim=c(-4,4), ylim=c(-4,4), pch=20)
plot(pca)

v.x = diag(var(x))
v.z = pca$sdev^2
pca$rotation
c(diag(var(x)), sum(diag(var(x))))
c(var.z=v.z, sum.z=sum(v.z))
pca$rotation

library(pls)
p1 = pcr(mpg~ .-name-origin, data=Auto, scale=T, validation='CV')
psummary(p1)
validationplot(p1, val.type ='MSEP')
validationplot(p1, val.type ='R2')

p2 = plsr(mpg~ .-name-origin, data=Auto, scale=T, validation='CV')
summary(p2)
summary(lm(mpg~.-name-origin, data=Auto))


## In-Class Lab

# Ridge Regression
prostate = read.table('prostate.data.txt', header=TRUE)
summary(prostate)
x = model.matrix(~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate)
y = prostate$lpsa
plot(glmnet(x, y, alpha=0)) # Plot Ridge regression coefficients vs L1-Norm
cv.r = cv.glmnet(x,y, alpha=0) # Ridge regression cross-validation
plot(cv.r) # Plot MSE vs log-lambda; Want lowest MSE; Vertical bars are Confidence Intervals
cv.r$lambda.min
ridge = glmnet(x, y, alpha=0, lambda=cv.r$lambda.min)
cbind(coef(cv.r, s='lambda.min'), coef(ridge))


# Lasso Regression
plot(glmnet(x,y)) # default alpha=1=Lasso
cv.l = cv.glmnet(x, y, alpha=1)
plot(cv.l)
cv.l$lambda.min
lasso = glmnet(x, y, alpha=1, lambda=cv.l$lambda.min)
cbind(coef(cv.l, s='lambda.min'), coef(lasso))

# Principle Component Regression
library(pls)
pr1 = pcr(y~x, data=prostate, scale=T, validation='CV')
pr1 = pcr(lpsa~.-svi-gleason, data=prostate, scale=T, validation='CV') # Regress on all IV's except svi and gleason, which are categorical
validationplot(pr1, val.type='MSEP')
validationplot(pr1, 'R2')
summary(pr1)

# Partial Least Squares Regression
pr2 = plsr(lpsa~.-svi-gleason, data=prostate, scale=T, validation='CV') # Regress on all IV's except svi and gleason, which are categorical
validationplot(pr2, val.type='MSEP')
validationplot(pr2, 'R2')
summary(pr2)

