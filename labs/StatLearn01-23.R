# Import Auto dataset
library(ISLR)
# Plot Correlation Matrix of Auto
cormat.Auto = plot(Auto)
# Linear regression model with mpg as DV, removing 'name' feature
model = lm(mpg~.-name, data=Auto)
# Check relationships between IV's and DV's; Lower Pr value is better, more stars means more significant
summary(model)
# Check correlation values between model variables
cor(subset(Auto, select=-name))
# Set frame to hold 4 graphs, plot the linear model
opt = par(mfrow=c(2,2))
plot(model, pch=20)

# Include model interactions 
model.interactions = lm(mpg~.-name+(cylinders+displacement+horsepower+weight+acceleration+year+origin), data=Auto)
summary(model.interactions)

# Check residuals
auto = Auto
auto$residual = model$residuals
head(auto)
plot(auto[,-9])
# Transform model based on residual plot patterns between DV and IV's
model.trans = lm(mpg~cylinds+log(displacement)+log(horsepower)+log(weight)+sqrt(acceleration)+year+origin, data=Auto)
# Check if model has improved
summary(model.trans)


