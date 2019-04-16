
library(MASS)

filename<-"/path/to/trees_sample.csv"
trees=read.csv(filename,row.names = 1)
trees[,2:11]=scale(trees[,2:11])


# Preparation
# Divide data into training (80%) and test (20%) by doing random sample without replacement
set.seed(10101)
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(trees), size = floor(.80*nrow(trees)), replace = F)
trees_train <- trees[sample, ]
trees_test  <- trees[-sample, ]


# Build LDA model on scaled training data
# when CV = True
t.lda1<-lda(Type ~ .,data=trees_train[, -1],CV=T)
# test accuracy via the missclassification rate (MCR)
MCR=1-sum(diag(prop.table(table(trees_train$Type,t.lda1$class))))
# when CV = False
t.lda2<-lda(Type ~ .,data=trees_train[, -1])


# chi-sq test for overall significance of predicted classes
chisq.test(trees_train$Type,t.lda1$class)
# use MANOVA to get Wilks test result:
summary(manova(as.matrix(trees_train[2:10])~t.lda1$class),test="Wilks")
# and summary.aov() to get individual contributions ?
summary.aov(manova(as.matrix(trees_train[2:10])~trees_train$Type),test="Wilks")



# determine which LD components are important using barplot
barplot(t.lda2$means,beside=T)
print(t.lda2$means)
# plot LDA 1 vs 2 for actual classes vs predicted (stored in an lda() model object called lda.1.pred)
lda.1.pred <- predict(t.lda2,trees_train[, -1])
par(mfrow = c(2, 1))
plot(lda.1.pred$x[,1],lda.1.pred$x[,2],col=trees_train$Type,main="(a) Actual")
legend(-5,-5,legend=seq(1,7,1),col=seq(1,7,1),pch=20,horiz = T)

plot(lda.1.pred$x[,1],lda.1.pred$x[,2],col=lda.1.pred$class,main="(b) Predicted")
legend(-5,-5,legend=seq(1,7,1),col=seq(1,7,1),pch=20,horiz = T)
# return plotting page to normal
layout(1)

## effective variable
summary(t.lda$means)
## reduced model
lda.elev=lda(Type ~ Elevation, data=trees_train[, -1],CV=T)
MCR2=1-sum(diag(prop.table(table(trees_train$Type,lda.elev$class))))
print(MCR2)


# Prediction of test data:
# apply full model to test data and get MCR:
test.pred=lda(Type ~ .,data=trees_test[, -1],CV=T)
MCRT=1-sum(diag(prop.table(table(trees_test$Type,test.pred$class))))
print(MCRT)


# Clustering: find out how many distinct tree types we really have...
# tree diagram (work on a random sample of n=1000 to speed things up):
sam=sample(seq(1,80000,1),size=1000)
hc = hclust(dist(trees_train[sam,1:10]))
hcd=as.dendrogram(hc)
plot(hcd)
# very simple dendrogram, cut at h=10
plot(cut(hcd, h = 10)$upper, main = "Upper tree of cut at h=10")
 
# use EH Ch 9 method for determining how many clusters based on iterative within groups sum of squares
wss <- (nrow(trees_train[,2:10])-1)*sum(apply(trees_train[,2:10],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(trees_train[,2:10],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
 
#
# k-means fit with k = 6
fit <- kmeans(trees_train[,2:10], 6)

# Centroid Plot against 1st 2 discriminant functions (explain 95%+ variations)
library(fpc) 
plotfile=paste(path, "clusters.pdf", sep="")
pdf(file=plotfile)
plotcluster(trees_train[,2:10], fit$cluster)
graphics.off()
