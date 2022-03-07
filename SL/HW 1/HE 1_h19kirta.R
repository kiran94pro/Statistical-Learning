install.packages("class")
library(MASS)
library(glmnet)
library(class)

library(PerformanceAnalytics)
a <- read.csv("ANES2016.csv", header = TRUE)
data3 <- a
View(a)
a$Trump[a$Trump >0 & a$Trump < 4] <- "Liberal" 
a$Trump[a$Trump>=4 & a$Trump <= 7] <- "Conservative"
a$Trump
attach(a)
View(a)
subtrump <- subset(a,Trump=="Liberal" | Trump =="Conservative")
sum(is.na(subtrump))
subtrump<-na.omit(subtrump)
subtrump$Trump <- as.factor(subtrump$Trump)
summary(subtrump)

View(data3)
sum(is.na(data3))
data3 <- na.omit(data3)

my_data <- cor(data3)[, c(2,3,4,5,8,11,12,14,15,16)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data <- cor(data3)[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
chart.Correlation(my_data, histogram=TRUE, pch=19)



col<- colorRampPalette(c("black", "white", "red"))(10)
heatmap(x = cor(data3), col = col, symm = TRUE)
####
subtrump2 <- subtrump
subtrump3 <- subtrump


trump <- glm(Trump~., data=subtrump2, family = binomial)
summary(trump)

mypred <- predict(trump, type= "response")
subtrump2$Pred<-ifelse(mypred>0.5,"Liberal","Conservative")
xtabs(~Trump+Pred,data = subtrump2)
mean(subtrump2$Trump==subtrump2$Pred)
View(subtrump2)


trump1 <- glm(Trump~ Hilary+SpouseEdu+Dependent+Income+ Education2, data=subtrump2, family = binomial)
summary(trump1)
mypred <- predict(trump1, type= "response")
subtrump3$Pred<-ifelse(mypred>0.5,"Liberal","Conservative")
xtabs(~Trump+Pred,data = subtrump3)
mean(subtrump3$Trump==subtrump3$Pred)

#######2
data2 <- subtrump
View(data2)

sum(is.na(data2))
4041*0.7
samples<-sample(1:nrow(subtrump2),size=2828,replace = F)
train.data<- subtrump2[samples,]
test.data <- subtrump2[-samples,]

lda.fit <- lda(PartyID~., data= train.data)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$PartyID)
mean(lda.class == test.data$PartyID)
prop.table(xtabs(~PartyID+lda.class,data=test.data),1)

lda.fit <- lda(PartyID~ Hilary+SpouseEdu+Dependent+Income+ Education2, data= train.data)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$PartyID)
mean(lda.class == test.data$PartyID)
prop.table(xtabs(~PartyID+lda.class,data=test.data),1)

###
qda.fit = qda(PartyID~., data= train.data)
qda.fit
#plot(qda.fit)
qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$PartyID)
mean(qda.class==test.data$PartyID)

qda.fit = qda(PartyID~ Hilary+SpouseEdu+Dependent+Income+ Education2, data= train.data)
qda.fit
#plot(qda.fit)
qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$PartyID)
mean(qda.class==test.data$PartyID)



####
train.PartyID <-train.data$PartyID
train.data.knn1 <-cbind(train.data$Hilary,train.data$Education2,
                        train.data$Media,train.data$Age, train.data$Birthplace, 
                        train.data$Income, train.data$FamSize, train.data$Partner,
                        train.data$SpouseEdu, train.data$Dependent, train.data$Housing)
test.data.knn1 <- cbind(test.data$Hilary,test.data$Education2,test.data$Media,test.data$Age, test.data$Birthplace, 
                        test.data$Income, test.data$FamSize, test.data$Partner,
                        test.data$SpouseEdu, test.data$Dependent, test.data$Housing)

knn.pred1=knn(train=train.data.knn1,test=test.data.knn1,cl=train.PartyID,k=3)
prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
mean(knn.pred1==test.data$PartyID)

knn.pred1=knn(train=train.data.knn1,test=test.data.knn1,cl=train.PartyID,k=8)
prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
mean(knn.pred1==test.data$PartyID)

knn.pred1=knn(train=train.data.knn1,test=test.data.knn1,cl=train.PartyID,k=10)
prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
mean(knn.pred1==test.data$PartyID)

