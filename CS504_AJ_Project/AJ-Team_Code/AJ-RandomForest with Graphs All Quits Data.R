# CS504 Working Version for Final Project
# Date 11-25-21
#########################
#########################
# Bill Thomson 
# Original code provided for RF Regression Lab M4A
# R Studio Version 1.4.1106
# R version 4,04
# Windows 10, i7 10th gen 64-bit operating system, x64-based processor 
# 64GB ram and 4TB SSD
########################  
# Random Forest Regression with Trees
# Based on stat515 Project Code and related Lab

library(randomForest)
library(rpart)
library(rpart.plot)
library(hexbin)
library(lattice)
library(gbm)
library(ISLR)

setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="QuitsIndustry_2001-2021r.csv", header = TRUE, 
               sep=',', row.names =1)
head(QI)

windows(width=9, height=9) ## For MS Windows
offDiag <- function(x,y,...){
   panel.grid(h=-1,v=-1,...)
   panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                    trans=function(x)x^1)
   panel.loess(x , y, ..., lwd=2,col='red')
}

onDiag <- function(x, ...){
   yrng <- current.panel.limits()$ylim
   d <- density(x, na.rm=TRUE)
   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
   panel.lines(d,col=rgb(.83,.66,1),lwd=2)
   diag.panel.splom(x, ...)
}
splom(QI[,c(1-12)],
      xlab='',main="Quits Industry 2016-2021 \n5 Year",
      pscale=0, varname.cex=0.7,axis.text.cex=0.8,
      axis.text.col="blue",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)

windows(width=9, height=9) ## For MS Windows
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
                   trans=function(x)x^1)
  panel.loess(x , y, ..., lwd=2,col='red')
}

onDiag <- function(x, ...){
  yrng <- current.panel.limits()$ylim
  d <- density(x, na.rm=TRUE)
  d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
  panel.lines(d,col=rgb(.83,.66,1),lwd=2)
  diag.panel.splom(x, ...)
}
splom(QI[,c(3,4,8,9,6)],
      xlab='',main="Quits Industry:Selected Variables   \n2016-2021 \n5 Year",
      pscale=0, varname.cex=0.7,axis.text.cex=0.8,
      axis.text.col="blue",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)
dev.off()
# Lets see what regression tree gives us for Manufacturing and Trade_Trans_Util
windows(width=9, height=9) ## For MS Windows

bostAll=rpart(Trad.tranutil~. ,data=QI) 
rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Trade Transportation Utilities Data")

bostAll=rpart(ManFact~. ,data=QI) 
rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Manufacturing Data")

bostAll=rpart(Leishospt~. ,data=QI) 
rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Leisure Data")

QI2<- read.csv(file="QuitsIndustry_2016-2021q.csv", header = TRUE, 
                   sep=',', row.names =1)
head(QI2)
windows(width=9, height=9) ## For MS Windows

#bostAll=rpart(Leisure_Hosp ~. ,data=QI2) 
#rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Leisure Data")

bostAll=rpart(Trade_trans_util~. ,data=QI2) 
rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Trade Transportation Utilities Data")

bostAll=rpart(ManFact~. ,data=QI2) 
rpart.plot(bostAll, faclen=12, extra=1, digits=3, main=" Regression Model \n Manufacturing Data")


# Lets set up our test data taking out the training data set
set.seed(137) # also try seed of 137
train5 = sample(1:nrow(QI), nrow(QI)/3)
QI.test5 = QI[-train5,]$Trad.tranutil

train6 = sample(1:nrow(QI), nrow(QI)/3)
QI.test6 = QI[-train6,]$Trad.tranutil

trainbst = sample(1:nrow(QI), nrow(QI)/4)
QI.testbst = QI[-trainbst,]$Trad.tranutil

# lets see how they perform with the same samples

#train5=train6=trainbst

## Bagging use many copies and then from those 
## together generate tree or their averages with 13 predictors 
## default ntree = 500
bag.QI=randomForest(Trad.tranutil~. , data = QI, subset = train5, mtry = 13, importance = TRUE)
bag.QI

yhat.bag = predict(bag.QI, newdata = QI[-train5, ])
# Test set and predictor test
testMSE5 <- mean((QI.test5-yhat.bag)^2)
testMSE5

den5 <- mean(QI.test5-mean(QI.test5)^2)
den5
pvar5 <- 100*(1-(testMSE5/den5))
pvar5

#prcVarden = sum(y_pred - mean(y_true)) ^ 2

##Random Forest using only 6 predictors with default ntree=500
rf.QI=randomForest(Trad.tranutil~., data = QI, subset = train6, mtry = 6, importance = TRUE)
rf.QI

yhat.rf = predict(rf.QI,newdata=QI[-train6,])
testMse6 <- mean((yhat.rf-QI.test6)^2)
testMse6

# percent of variance explained
den6 <- mean(QI.test6-mean(QI.test6)^2)
pvar6 <- 100*(1-testMse6/den6)
pvar6


###################################

boost.QI=gbm(Leishospt~.,data=QI[trainbst,], distribution = "gaussian", 
                 n.trees=5000,interaction.depth=5, shrinkage=.2,verbose=F)

yhat.boost = predict(boost.QI,newdata=QI[-trainbst,], n.trees=5000)
testmseboost=mean((yhat.boost-QI.testbst)^2)
testmseboost
denboost <- mean(QI.testbst - mean(QI.testbst))^2
pVarboost <- 100*(1-testmseboost/denboost)
pVarboost

windows(width=9,height=9)
summary(boost.QI)
boost.QI

## Lets get a compare what the yhat and test data for each looks like

### Variable Importance
windows(width=9,height=9)
par(mfrow = c(1, 3))
## 5.1- bagged
importance(bag.QI)
varImpPlot(bag.QI, type = 1)
## 6.1- random forest
importance(rf.QI)
varImpPlot(rf.QI, type = 1)
## ISLR Boosted
summary(boost.QI)


############################################# Random Forest using only 6 predictors with default ntree=25
rf.QI=randomForest(Trad.tranutil~., data = QI, subset = train6,ntree=25, mtry = 6, importance = TRUE)
yhat.rfs = predict(rf.QI,newdata=QI[-train6,])
meanrfs=mean((yhat.rfs-QI.test6)^2)
meanrfs
mse6s <- mean((yhat.rfs - QI.test6)^2)
den6s <- mean((QI.test6 - mean(QI.test6))^2)
pVar6s <- 100*(1-mse6s/den6s)
pVar6s
mse6s

