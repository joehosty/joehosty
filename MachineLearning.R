- 👋 Hi, I’m @joehosty
This is some code from a machine learning project I had to complete. 
Where I was trying to best classify Hadron and Gamma particles. 
Using various classification methods. 


gam <- read.csv("gamma.csv")


%dDATA MULCHING
normalize = function(x){
  return((x-min(x, na.rm = TRUE))/((max(x, na.rm = TRUE)) - min(x, na.rm = TRUE)))
}

gamnorm <- as.data.frame(apply(gam[-11], 2, normalize))
as.data.frame(apply(gam,2,normalize))
gam[gam$class == 	"g",]$class <- as.numeric(0)
gam[gam$class == "h",]$class <- as.numeric(1)
ind<-sample(2,nrow(gam),replace=TRUE,prob=c(0.75,0.25)) 
gamtrain<-gam[ind==1,]
gamtest<-gam[ind==2,] 
traintarget <- gam[ind == 1, 11]
gam$Length <- (gam$Length - min(gam$Length))/(max(gam$Length)-min(gam$Length))
gam$Width <- (gam$Width - min(gam$Width))/(max(gam$Width)-min(gam$Width))
gam$Size <- (gam$Size - min(gam$Size))/(max(gam$Size)-min(gam$Size))
gam$Conc <- (gam$Conc - min(gam$Conc))/(max(gam$Conc)-min(gam$Conc))
gam$Conc1 <- (gam$Conc1 - min(gam$Conc1))/(max(gam$Conc1)-min(gam$Conc1))
gam$Asym <- (gam$Asym - min(gam$Asym))/(max(gam$Asym)-min(gam$Asym))
gam$M3Long <- (gam$M3Long - min(gam$M3Long))/(max(gam$M3Long)-min(gam$M3Long))
gam$M3Trans <- (gam$M3Trans - min(gam$M3Trans))/(max(gam$M3Trans)-min(gam$M3Trans))
gam$Alpha <- (gam$Alpha - min(gam$Alpha))/(max(gam$Alpha)-min(gam$Alpha))
gam$Dist <- (gam$Dist - min(gam$Dist))/(max(gam$Dist)-min(gam$Dist))
gamtraincopy <- gamtrain
gamtraincopy$class <- as.numeric(as.factor(gamtraincopy$class))
trainingtarget <- gam[ind == 1, 11]
testtarget <- gam[ind == 2, 11]
trainingtarget <- as.numeric(as.factor(trainingtarget))


%LOGERSTIC REGRESION%


gam.lr<-glm(as.factor(class)~.,data=gam,family=binomial) 
summary(gam.lr)
class.prob<-predict(gam.lr,gam,type="response")
gam.pred<-ifelse(class.prob>0.5,1,0)
table(gam$class,gam.pred)

gamtrain.lr<-glm(as.factor(class)~.,data=gamtrain,family=binomial) 
gamtest.prob<-predict(gamtrain.lr,gamtest,type="response") 
gamtest.pred<-ifelse(gamtest.prob>0.5,2,1) 
table(gamtest$class,gamtest.pred)
predicted.lr.rank$class <- 1:nrow(class.prob)


%LDA + QDA%


unclass(class)
gam.lda <- lda(class~., data = gam)
plot(gam.lda)
gam.lda.values <- predict(gam.lda)$class
gam.lda.values
table(gam.lda.values,gam$class)

gamtrain.lda<-lda(class~., data = gamtrain, type = 'class')
gamtest.prob1<-predict(gamtrain.lda, gamtest, type = "class")$class

table(gamtest$class,gamtest.prob1)
par(mar=c(2,2,2,2))
plot(gam.lda)


gam.ldagam.qda <- qda(class~., data = gam)
gam.qda
predict(gam.qda)$class

mean(predict(gam.qda$class == gam$class))
table(gam[-11],predict(gam.qda)$class)     
gam.pred2 <- predict(gam.qda)$class

class.prob2<-predict(gam.qda,gam,type="response")
class.prob2
gam.pred2<-ifelse(class.prob1>0.5,1,0)

plot(gam.qda)

gamtrain.qda<-qda(class~., data = gamtrain, type = 'class')
gamtest.prob3<-predict(gamtrain.qda, gamtest, type = "class")$class

table(gamtest$class,gamtest.prob3)

%DECISION TREES%

library(rpart)


gam.tr<-rpart(as.factor(class)~.,data=gamtrain, method="class")
printcp(gam.tr) 
gam.prune<-prune(gam.tr,cp=0.1) 
plot(gam.prune,branch=6,uniform=TRUE,margin=8) 
text(gam.prune)
x.hat<-predict(gam.tr, gamtest, type = 'class')
table(gamtest$class,x.hat)
rpart.plot(gam.tr, extra = 106)

gam.rf<-randomForest(as.factor(class)~.,data=gamtrain,ntree=100) 
y.hat<-predict(gam.rf,gamtest)
y.hat
table(gamtest$class,y.hat)
plot(randomForest(as.factor(class)~., gamtrain, keep.forest=FALSE, ntree=500), log="y", main = "Random Forest MSE")
