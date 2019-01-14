# Import training and test data
d <- read.table("C:/Users/melcb01/Downloads/zip.train.gz")
d2 <- read.table("C:/Users/melcb01/Downloads/zip.test.gz")
dim(d2)

index <- d2[,1] == 2 | d2[,1]

# Visualization
idx <- d[,1] == 2 | d[,1] == 3
train <- d[idx,]
pca <- princomp(train[,1])
plot(pca$scores)
col <- rep('orange', nrow(train))
# if actual digit is 2, actual color is assigned to blue
col[(train[,1] == 2)] <- 'blue'
head(col)
# 2D Visualization - try to find some line as a boundary between 2's and 3's
plot(pca$scores, col=col)
# 3D Visualization - try to find some surface as a boundary between 2's and 3's
library(scatterplot3d)

# Linear regression
reg1 <- lm(V1~.-V1, data=d)
pred <- predict(reg1, newdata=d2)
head(pred)
final.pred <- ifelse(pred < 2.5, 2, 3)



