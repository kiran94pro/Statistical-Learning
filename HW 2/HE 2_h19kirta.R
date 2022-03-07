rm(list = ls())
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)

a1 <-read_excel("Data_Cortex_Nuclear.xls")

# Removing the index
a1 <- a1[,c(2:82)]

# Replacing null values
for(i in 1:77){
  a1[is.na(a1[,i]), i] <- min(a1[,i], na.rm = TRUE)
}

#Check whether nullvalues removed or not
sum(is.na(a1))
View(a1)
summary(a1)
View(a1)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
a2 <- a1[,c(1:77,81)]
# Binary-class Dataset
a3 <- a1[,c(1:78)]
set.seed(201345)

# Sampling the dataset
train <- sample(1:nrow(a1),750)

# Multi-class Train and Test Dataset
a2_train <- a2[train,]
a2_test <- a2[-train,]
View(a2_train)

# Binary-class Train and Test Dataset
a3_train <- a3[train,]
a3_test <- a3[-train,]

# 1. a) Use the 77 proteins as predictors for decision trees and support vector machines 
# models to make binary and multiple class classification

################ Multi-class: Decision Tree #####################
tree.a2_train <- tree(as.factor(a2_train$class) ~., a2_train)
summary(tree.a2_train)

x11()
plot(tree.a2_train)
text(tree.a2_train ,pretty =0)
yhat.bag <- predict(tree.a2_train ,a2_test,type="class")
a2_tests <- as.factor(a2_test$class)
table(yhat.bag,a2_tests)
#table(yhat.bag,df1_test$class)

#  Mean: 42+23+40+28+19+25+38+31= 246/330 = 0.7454545

################ Binary-class: Decision Tree #####################
tree.a3_train <- tree(as.factor(a3_train$Genotype) ~., a3_train)
summary(tree.a3_train)
x11()
plot(tree.a3_train)
text(tree.a3_train ,pretty =0)
yhat.bag2 <- predict(tree.a3_train ,a3_test, type="class")
a3_tests <- as.factor(a3_test$Genotype)
table(yhat.bag2,a3_tests)
# > 149+134 = 283 -> 283/330= 0.8575758

########## Multi-class:support vector classifier-Linear ################
set.seed(24583)
tune.out1 <- tune(svm, as.factor(class) ~., data=a2_train, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 

#Number of Support Vectors:  327
#Best model 1 & Applying it
svmfit_linear <- svm(as.factor(class)~., data=a2_train, kernel ="linear", cost = 1,
                     scale =FALSE)

ypred <- predict(svmfit_linear ,a2_test)
table(predict = ypred , truth= a2_test$class)
#46+27+50+35+36+32+42+35 = 303/330= 0.91
############### Multi-class:support vector classifier-Radial #####################
set.seed(142342)
tune.outr1 <- tune(svm, as.factor(class) ~., data=a2_train, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost gamma
#100 0.001

#Number of Support Vectors:  361

svmfit_RBF1 <- svm(as.factor(class)~., data=a2_train, kernel ="radial", gamma = 0.001,
                   cost = 100)
ypredr1 <- predict(svmfit_RBF1 ,a2_test)
table(predict = ypredr1 , truth=a2_test$class)
#50+38+50+34+44+33+45+35 = 329/330 = 0.996
#####################Multi-class:support vector classifier-Polynomial######################

#Tune
set.seed(142342)
tune.outp1 <- tune(svm, as.factor(class) ~., data=a2_train, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  404
svmfit_poly2 <- svm(as.factor(class)~., data=a2_train, kernel="polynomial",degree = 3,
                    cost = 5)

ypredp2 <- predict(svmfit_poly2 ,a2_test)
table(predict = ypredp2 , truth=a2_test$class)
#50+38+50+35+44+33+45+35 = 330/330 = 100

################# Binary Class: support vector classifier- Linear###################

##Tune
set.seed(142342)
tune.outbl1 <- tune(svm, as.factor(Genotype) ~., data=a3_train, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.1 

#Number of Support Vectors:  152
svmfit_linearbl1 <- svm(as.factor(Genotype)~., data = a3_train, kernel ="linear", 
                        cost = 0.1, scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,a3_test)
table(predict = ypredbl1 , truth= as.factor(a3_test$Genotype))
#141+122 = 263/330 = 0.796
####Radial

set.seed(242442)
tune.outrbr1 <- tune(svm, as.factor(Genotype) ~., data=a3_train, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  5 

#Number of Support Vectors:  246

svmfit_RBFbr1 <- svm(as.factor(Genotype)~., data=a3_train, kernel ="radial", 
                     gamma = 0.01, cost = 5)
ypredrb1 <- predict(svmfit_RBFbr1 ,a3_test)
table(predict = ypredrb1 , truth= a3_test$Genotype)
#173+157 = 330/330 =  100

#####polynomial
tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=a3_train, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  280
svmfit_polypb2 <- svm(as.factor(Genotype)~., data=a3_train, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 5)
ypredpb2 <- predict(svmfit_polypb2 ,a3_test)
table(predict = ypredpb2 , truth= a3_test$Genotype)
#172+156 = 328/330 = 0.993

#b) Perform principal component analysis on the 77 numerical features. 
#Use an appropriate number of principal components as predictors and 
#perform the same classification task.

###PCA

pr.out <- prcomp(a1[,c(1:77)], scale = TRUE) #pca.train <- new_my_data[1:nrow(train), prin_comp <- prcomp(pca.train, scale. = T)
biplot(pr.out , scale = 0)
attach(a1)

pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of
         Variance Explained", ylim=c(0,1) ,type="b")

plot(cumsum (pve ), xlab="Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim=c(0,1),
     type="b")

comp <- data.frame(pr.out$x[,1:9])
View(comp)
attach(comp)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
a4 <- a1[81]
View(a4)

# Binary-class Dataset
a5 <- a1[78]
View(a5)
set.seed(201345)
################ Multi-class: Decision Tree #####################
a6 <- data.frame(comp,a4)
train <- sample(1:nrow(a6),750)
a1_trainm <- a6[train,]
a1_testm <- a6[-train,]
##
tree.mc = tree(as.factor(class)~.,a1_trainm)
summary(tree.mc)
x11()
plot(tree.mc)
text(tree.mc ,pretty =0)
tree.predm1=predict (tree.mc ,a1_testm ,type ="class")
table(tree.predm1,a1_testm$class)
#25+26+31+28+27+15+6+19 = 177/330 = 0.59
#####BINARY
a7 <- data.frame(comp,a5)
train <- sample(1:nrow(a7),750)
a1_trainb <- a7[train,]
a1_testb <- a7[-train,]
tree.bi =tree(as.factor(Genotype)~.,a1_trainb)
summary(tree.bi)
x11()
plot(tree.bi)
text(tree.bi ,pretty =0)
tree.predm1= predict(tree.bi ,a1_testb ,type ="class")
table(tree.predm1,a1_testb$Genotype)
#136+122 = 258/330 = 0.78


########## Multi-class:support vector classifier-Linear ################
set.seed(24583)
tune.out1 <- tune(svm, as.factor(class) ~., data=a1_trainm, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 

#Number of Support Vectors:  468
#Best model 1 & Applying it
svmfit_linear <- svm(as.factor(class)~., data=a1_trainm, kernel ="linear", cost = 5,
                     scale =FALSE)
#plot(svmfit_linear , a1_trainm)########
ypred <- predict(svmfit_linear ,a1_testm)
table(predict = ypred , truth= a1_testm$class)
#34+32+40+29+25+25+25+34 = 244/330= 0.73

############### Multi-class:support vector classifier-Radial #####################
set.seed(142342)
tune.outr1 <- tune(svm, as.factor(class) ~., data=a1_trainm, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost: 10   

#Number of Support Vectors:  454

svmfit_RBF1 <- svm(as.factor(class)~., data=a1_trainm, kernel ="radial", gamma = 0.1,
                   cost = 10)
ypredr1 <- predict(svmfit_RBF1 ,a1_testm)
table(predict = ypredr1 , truth=a1_testm$class)
##49+37+47+33+44+33+37+35 = 315/330 =0.955

#####################Multi-class:support vector classifier-Polynomial######################

#Tune
set.seed(142342)
tune.outp1 <- tune(svm, as.factor(class) ~., data=a1_trainm, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  10 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  402

svmfit_poly2 <- svm(as.factor(class)~., data=a1_trainm, kernel="polynomial",
                    degree = 3,cost = 10)

ypredp2 <- predict(svmfit_poly2 ,a1_testm)
table(predict = ypredp2 , truth=a1_testm$class)
#50+37+43+34+43+32+37+35 = 311/330 = 0.94

################# Binary Class: support vector classifier- Linear###################


##Tune
set.seed(142342)
tune.outbl1 <- tune(svm, as.factor(Genotype) ~., data=a1_trainb, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  5 

#Number of Support Vectors:  480
svmfit_linearbl1 <- svm(as.factor(Genotype)~., data = a1_trainb, kernel ="linear", 
                        cost = 5, scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,a1_testb)
table(predict = ypredbl1 , truth= as.factor(a1_testb$Genotype))
#140+101 = 241/330 = 0.73
####Radial

set.seed(242442)
tune.outrbr1 <- tune(svm, as.factor(Genotype) ~., data=a1_trainb, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost: 5     
#Number of Support Vectors:  603

svmfit_RBFbr1 <- svm(as.factor(Genotype)~., data=a1_trainb, kernel ="radial", 
                     gamma = 1, cost = 5)
ypredrb1 <- predict(svmfit_RBFbr1 ,a1_testb)
table(predict = ypredrb1 , truth= a1_testb$Genotype)
#165+161 = 326/330 =  0.98

#####polynomial
tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=a1_trainb, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  100 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  150
svmfit_polypb2 <- svm(as.factor(Genotype)~., data=a1_trainb, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 100)
ypredpb2 <- predict(svmfit_polypb2 ,a1_testb)
table(predict = ypredpb2 , truth= a1_testb$Genotype)
#159+150 = 309/330 = 0.93









##### c) bagging, random forest, and boosting perform 
###Random Forest--Multi Class

library(randomForest)
library(gbm)
ran.cor.multi <- randomForest(as.factor(class)~., data=a2_train, mtry=9, 
                              importance =TRUE)
yhat.ran.multi <- predict(ran.cor.multi ,newdata = a2_test)
ran.test.multi <- as.factor(a2_test$class)
table(predict=yhat.ran.multi, truth=ran.test.multi)
#50+37+50+35+42+33+43+35 = 325/330 = 0.98
varImpPlot(ran.cor.multi)


###Random Forest--Binary Class

ran.cor.bi <- randomForest(as.factor(Genotype)~., data=a3_train, mtry=9, 
                           importance =TRUE)
yhat.ran.bi <- predict(ran.cor.bi ,newdata = a3_test)
ran.test.bi <- as.factor(a3_test$Genotype)
table(predict=yhat.ran.bi, truth=ran.test.bi)
#173+151 = 324/330 = 0.98
varImpPlot(ran.cor.bi)

####### Bagging- Multi Class

bag.cor.multi <- randomForest(as.factor(class)~., data=a2_train,
                              mtry=77, importance =TRUE)
yhat.bag.multi <- predict(bag.cor.multi ,newdata = a2_test)
cor.test.multi <- as.factor(a2_test$class)
table(predict=yhat.bag.multi, truth=cor.test.multi)
#50+38+49+35+43+33+41+32 = 321/330 = 0.97
##### Bagging- Binary Class
bag.cor.bi <- randomForest(as.factor(Genotype)~., data=a3_train,
                           mtry=77, importance =TRUE)
yhat.bag.bi <- predict(bag.cor.bi ,newdata = a3_test)
cor.test.bi <- as.factor(a3_test$Genotype)
table(predict=yhat.bag.bi, truth=cor.test.bi)
#164+147 = 308/330 = 0.94

##############Boosting

#####################
boost.binary <- a3
boost.binary$Genotype <- ifelse(boost.binary$Genotype == "Control",0,1)
View(boost.binary)
bb_train <- boost.binary[train,]
bb_test <- boost.binary[-train,]
#####################Boost Binary
bbinary <- gbm(Genotype~., data=bb_train, distribution = "bernoulli",
               n.trees =5000, interaction.depth =4)
summary(bbinary)
yhat.bbinary <- predict(bbinary, newdata=bb_test,n.trees=5000)
yhat <- (yhat.bbinary - min(yhat.bbinary)) / (max(yhat.bbinary) - min(yhat.bbinary))
data_pred <- ifelse(yhat <= 0.5,0,1)
ran.test.binary <- bb_test$Genotype
table(predict=data_pred,truth=ran.test.binary)
#162+163 = 325/330 = 0.98
########## Boosting Multiclass
bmulti <- gbm(class~., data=a2_train, distribution = "multinomial",
              n.trees =5000, verbose = F, shrinkage = 0.01, interaction.depth =4)
summary(bmulti)
yhat.bmulti <- predict(bmulti, newdata=a2_test,n.trees=5000,type="response")
pred=as.matrix(yhat.bmulti[,,1])
p.pred <- apply(pred, 1, which.max)
ran.test.multi <- a2_test$class
table(p.pred, ran.test.multi)
#50+38+50+35+43+33+43+35 = 327/330 = 0.99






####### 2) Clustering
rm(list=ls())

loadPackages <- function(package0){
  package1 <- package0[!(package0 %in% installed.packages()[, "Package"])]
  if (length(package1))
    install.packages(package1, dependencies = TRUE)
  sapply(package0, require, character.only = TRUE)
}

Packages <- c("GGally","car", "MASS","gplots","rcompanion","outliers",
              "tseries",                                    
              "corrplot", "RColorBrewer",                   
              "dplyr","ggplot2") 
loadPackages(Packages)
install.packages("readxl")

library("readxl")


b<- read_excel("Data_Cortex_Nuclear.xls")



summary(b)

sum(is.na(b))

ncol(b)


for(i in 2:78){
  
  
  b1<-b[i]
  b1<-na.omit(b1)
  
  if(sum(is.na(b[i]))>0){
    
    b[i][is.na(b[i])] <- sum(b1)/nrow(b1)
    
  }
  
  
  
  
}

sum(is.na(b))
### elbow plot

k.max <- 10
data <- cc
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##
X = b[2:78]
cc<-scale(X)
dendrogram = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
plot(dendrogram)

summary(dendrogram)
n<-as.dendrogram(dendrogram)
nodePar<-list(lab.cex=0.9,pch=c(10,19),cex=0.7,col=c("green","yellow"))
plot(n,xlab="Height",nodePar=nodePar, main="cluster",edgePar=list(col=c("red","blue"),lwd=2:1),horiz=TRUE)




hc = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 2)

##
X = b[2:78]
cc<-scale(X)
dendrogram = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
plot(dendrogram)
summary(dendrogram)
n<-as.dendrogram(dendrogram)
nodePar<-list(lab.cex=0.9,pch=c(10,19),cex=0.7,col=c("green","yellow"))
plot(n,xlab="Height",nodePar=nodePar, main="cluster",edgePar=list(col=c("red","blue"),lwd=2:1),horiz=TRUE)




hc = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 6)
##
bb<-y_hc
b$cluster<-bb

plot(b$cluster)

table(b$cluster, b$Behavior)
table(b$cluster, b$Treatment)
table(b$cluster, b$Genotype)
##
library(cluster)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)

###############kmeans
library(factoextra)
cc<-scale(X)
distance <- get_dist(cc)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(cc, centers = 8, nstart = 25)
k2 <- kmeans(cc, centers = 2, nstart = 25)
k2 <- kmeans(cc, centers = 5, nstart = 25)
k2 <- kmeans(cc, centers = 6, nstart = 25)
k2 <- kmeans(cc, centers = 7, nstart = 25)
str(k2)
fviz_cluster(k2, data = cc)






















































































