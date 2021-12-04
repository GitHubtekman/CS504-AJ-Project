# CS504 Working Version for Team AJ Final Project
# Date 12-3-21
#########################
# Bill Thomson 
# Original code provided for RF  Lab 
# R Studio Version 1.4.1106
# R version 4,04
# Windows 10, i7 10th gen 64-bit operating system, x64-based processor 
# 64GB ram and 4TB SSD
########################  
# Random forest regression for Quits Data
# Code adapted from STAT 515 Labs, Daniel Carr
########################

library(randomForest)
library(lattice)
library(hexbin)
library(devtools)


setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="Quit_2016_2021q.csv", header = TRUE, 
               sep=',', row.names =1)
QI2 <- read.csv(file="QuitsIndustry_2001-2021r.csv", header = TRUE, 
                sep=',', row.names =1)
View(QI)
head(QI)
head(QI2)

## Lets set up our sample sets 
# 70-30 splits for samples
set.seed(137)

sample_size = round(nrow(QI)*.70) # setting what is 70%
index <- sample(seq_len(nrow(QI)), size = sample_size)

train <- QI[index, ] # 70%
test <- QI[-index, ] # 30%

sample_size2 = round(nrow(QI2)*.70) # setting what is 70%
index2 <- sample(seq_len(nrow(QI2)), size = sample_size)

train2 <- QI2[index, ] # 70%
test2 <- QI2[-index, ] # 30%

# 60-30 splits for samples
set.seed(143)

sample_size = round(nrow(QI)*.60) # setting what is 60%
index <- sample(seq_len(nrow(QI)), size = sample_size)

train <- QI[index, ] # 60%
test <- QI[-index, ] # 40%

sample_size2 = round(nrow(QI2)*.60) # setting what is 60%
index2 <- sample(seq_len(nrow(QI2)), size = sample_size)

train2 <- QI2[index, ] # 60%
test2 <- QI2[-index, ] # 40%

set.seed(4543)  
head(train)

RfManuf <- randomForest(x = train[ ,1:11], y=train[, 3], mtry=8,
                      importance=TRUE,  proximity=FALSE, ntree=500, keepForest=TRUE)
RfManuf

plot(RfManuf) + title("\n\n Random Forest Manufacturing")

RfTrade <- randomForest(x = QI[ , 1:11], y=QI[, 4],
                   importance=TRUE,  proximity=FALSE, ntree=5000,
                   keepForest=TRUE)
RfTrade

RfEduc <- randomForest(x = QI[ , 1:11], y=QI[, 8],
                   importance=TRUE,  proximity=FALSE, ntree=5000,
                   keepForest=TRUE)
RfEduc

RfLeis <- randomForest(x = QI[ , 1:11], y=QI[, 9],
                    importance=TRUE,  proximity=FALSE, ntree=5000,
                    keepForest=TRUE)
RfLeis

impm <- importance(RfManuf)
impm
impt <- importance(RfTrade)
impt
impe <- importance(RfEduc)
impe
imple <- importance(RfLeis)
imple
varImpPlot(RfManuf,cex=.8)
length(impm)
varImpPlot(RfTrade,cex=.8)
length(impt)
varImpPlot(RfLeis,cex=.8)
length(imple)
n <- 500
ord1 <- order(imp[, 1],decreasing=TRUE)
nam1 <- row.names(imp[ord1,])[1:n]
nam1
length(nam1)
ord2 <- order(imp[, 2],decreasing=TRUE)
nam2 <- row.names(imp[ord2,])[1:n]
nam2
length(nam2)
varNam <- union(nam1,nam2) 
length(varNam)
varNam<-varNam[!is.na(varNam)]

view(varNam)

varNam < na.omit(varNam)

checkCor <- round( cor(QI[,varNam],
                       method="spearman"),2)
checkCor
set.seed(4543)
Focus2 <- randomForest(x = QI[,varNam], y=QI[,4],
                          importance=TRUE,  proximity=FALSE, ntree=500, keepForest=FALSE)
Focus2
impF1 <- importance(Focus2)
impF1
varImpPlot(Focus2,cex=.9)
varDep <- colnames(QI)[3]
varNamDep <- c(varNam,varDep )
head(QI)
windows(width=9, height=9) ## For MS Windows

splom(QI[,rev(varNamDep)],
      xlab='',cex=.5,as.matrix=TRUE,
      main=paste("Quits 5yr by Industry",
                 "and Explanatory Variables",sep="\n"),
      pscale=0, varname.cex=0.38
)

ord <- order(impF1[,1],decreasing=TRUE)
varNam2 <- row.names(impF1[ord,])[1:5]
varNam2

###################################################################
## 20 YRS
###################################################################
set.seed(4543)  

RfManuf <- randomForest(x = QI2[ ,1:11], y=QI2[, 3],
                        importance=TRUE,  proximity=FALSE, ntree=500, keepForest=TRUE)
RfManuf

RfTrade <- randomForest(x = QI2[ , 1:11], y=QI2[, 4],
                        importance=TRUE,  proximity=FALSE, ntree=500,
                        keepForest=TRUE)
RfTrade

RfEduc <- randomForest(x = QI2[ , 1:11], y=QI2[, 8],
                       importance=TRUE,  proximity=FALSE, ntree=500,
                       keepForest=TRUE)
RfEduc

RfLeis <- randomForest(x = QI2[ , 1:11], y=QI2[, 9],
                       importance=TRUE,  proximity=FALSE, ntree=5000,
                       keepForest=TRUE)
RfLeis

impm <- importance(RfManuf)
impm
impt <- importance(RfTrade)
impt
impe <- importance(RfEduc)
impe
imple <- importance(RfLeis)
imple
varImpPlot(RfManuf,cex=.8)
length(impm)
varImpPlot(RfTrade,cex=.8)
length(impt)
varImpPlot(RfLeis,cex=.8)
length(imple)
n <- 500
ord1 <- order(impm[, 1],decreasing=TRUE)
nam1 <- row.names(impm[ord1,])[1:n]
nam1
length(nam1)
ord2 <- order(impm[, 2],decreasing=TRUE)
nam2 <- row.names(impm[ord2,])[1:n]
nam2
length(nam2)
varNam <- union(nam1,nam2) 
length(varNam)
varNam<-varNam[!is.na(varNam)]
View(varNam)  
varNam < na.omit(varNam)

checkCor <- round( cor(QI2[,varNam],
                       method="spearman"),2)
checkCor
set.seed(4543)
Focus2 <- randomForest(x = QI2[,varNam], y=QI2[,4],
                       importance=TRUE,  proximity=FALSE, ntree=500, keepForest=FALSE)
Focus2
impF1 <- importance(Focus2)
impF1
varImpPlot(Focus2,cex=.9)
varDep <- colnames(QI2)[3]
varNamDep <- c(varNam,varDep )
head(QI)
windows(width=9, height=9) ## For MS Windows

splom(QI2[,rev(varNamDep)],
      xlab='',cex=.5,as.matrix=TRUE,
      main=paste("Quits 5yr by Industry",
                 "and Explanatory Variables",sep="\n"),
      pscale=0, varname.cex=0.38
)

ord <- order(impF1[,1],decreasing=TRUE)
varNam2 <- row.names(impF1[ord,])[1:5]
varNam2


