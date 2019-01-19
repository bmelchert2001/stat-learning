# Import training and test data
d = read.table("C:/Users/melcb01/Downloads/zip.train.gz")
d2 = read.table("C:/Users/melcb01/Downloads/zip.test.gz")
names(d)[c(1:4, 255:ncol(d))]
dim(d2)

# Subset only training data observations by target column values equal to 2 or 3
idx1 = d[,1] == 2 | d[,1] == 3
training = d[idx1,]
# PCA to reduce dimensionality of feature columns
pca = princomp(training[,-1])
# Set color to replicate organge up through the length of training data observations
col = rep("orange", nrow(training))
# Change only colors of training target that are 2 to blue
col[(training[,1] == 2)] = "blue"
# Visualizations
plot(pca$scores[,1:2], col=col, pch=16, xlab="PC1", ylab="PC2", main="Blue=2, Orange=3")
library(scatterplot3d)
scatterplot3d(pca$scores[,1:3], color=col, pch=16, xlab="PC1", ylab="PC2", zlab="PC3", main="Blue=2, Orange=3")

# Subset only test data observations by target column values equal to 2 or 3
idx2 = d2[,1] == 2 | d2[,1] == 3
testing = d2[idx2,]
dim(testing)

# Linear regression
reg1 = lm(V1~.-V1, data=training)
pred = predict(reg1, newdata=testing)
head(pred)
pred_clf <- ifelse(pred < 2.5, 2, 3)
# Overall predictions between two classes
table(pred_clf)
# Check error and accuracy of predictions
table(testing[,1],pred_clf)
acc = (191+158)/(198+166)

# K Nearest Neighbors
k1 = knn(train=training[,-1], test=testing[,-1], cl=training[,1], k=1)
table(k1)
k3 = knn(train=training[,-1], test=testing[,-1], cl=training[,1], k=3)
table(testing[,1],k3)

# Loop over different values of k, plot associated accuracy
err.out <- data.frame();
for (i in c(1,3,5,7,15)){
  train.k <- knn(train = training[,-1], test=training[,-1], k=i, cl=training[,1])
  train.rslt <- table(training[,1], train.k)
  train.err <- (train.rslt[2,1]+train.rslt[1,2])/sum(train.rslt)
  
  test.k <- knn(train = training[,-1], test=testing[,-1], k=i, cl=training[,1])
  tab.test <- table(testing[,1],test.k)
  test.err <- (tab.test[2,1]+tab.test[1,2])/sum(tab.test)
  
  err.out<-rbind(err.out,c(i,train.err, test.err))
}
names(err.out)<-c("k", "training", "testing")

reg.test  <- 15/364
reg.train <- 8/1389

plot(err.out$k, err.out$testing, xlab="k", ylab="", type="b",
     lty=1, col=1, pch=15, lwd=3, ylim=c(0,0.05), cex=2,
     main="Error Rate as a Function of k")
lines(err.out$k, err.out$training, lty=5, col=2, lwd=3, pch=17, type="b", cex=2)
lines(c(1,15), c(reg.test, reg.test), lty=3, col=3, lwd=3)
lines(c(1,15), c(reg.train, reg.train), lty=4, col=4, lwd=3)
legend(11.6, .03, c("KNN - Test","KNN - Train", "Regr - Test", "Regr - Train"),
       lty=c(0,0,3,4), lwd=3, col=1:4, pch=c(15,17,NA,NA), cex=1.5)

