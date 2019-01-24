## Subset Selection

library(leaps)

# Best subsets selection
bst <- regsubsets(mpg~cylinders+horsepower+weight, Auto, nbest=2, method='e')
smrb <- summary(bst)
# Want cp close to the number of IV's, r^2 & adjr^2 higher, smaller bic
cbind(smrb$outmat, r2=round(smrb$rsq,2), adjr2=round(smrb$adjr2,2),
      cp=round(smrb$cp, digits=2), bic=round(smrb$bic,2))

# Forward selection (generally not recommended)
fwd <- regsubsets(mpg~cylinders+horsepower+weight, Auto, method='forward')
smrf <- summary(fwd)
cbind(smrf$outmat, r2=round(smrf$rsq,2), adjr2=round(smrf$adjr2,2),
      cp=round(smrf$cp, digits=2), bic=round(smrf$bic,2))

md <- lm(mpg~cylinders+horsepower+weight, data=Auto)
step(md, direction='forward')

# Backward Selection (choose if p=length of IV's < 15 features)
bwd <- regsubsets(mpg~cylinders+horsepower+weight, Auto, method='backward')
smr <- summary(bwd)
cbind(smr$outmat, r2=round(smr$rsq,2), adjr2=round(smr$adjr2,2),
      cp=round(smr$cp, digits=2), bic=round(smr$bic,2))

md <- lm(mpg~cylinders+horsepower+weight, data=Auto)
stepAIC(md, direction='backward')

# Stepwise (Hybrid) Selection (choose if p=length of IV's >= 15 features)
stp <- regsubsets(mpg~cylinders+horsepower+weight, Auto, method='seqrep')
smr <- summary(stp)
cbind(smr$outmat, r2=round(smr$rsq,2), adjr2=round(smr$adjr2,2),
      cp=round(smr$cp, digits=2), bic=round(smr$bic,2))

md <- lm(mpg~cylinders+horsepower+weight, data=Auto)
stp <- stepAIC(md, direction='both')