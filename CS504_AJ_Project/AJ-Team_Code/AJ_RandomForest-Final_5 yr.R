# CS504 Working Version for Final Project
# Date 11-25-21
#########################
# Bill Thomson 
# Original code provided for RF  Lab 
# R Studio Version 1.4.1106
# R version 4,04
# Windows 10, i7 10th gen 64-bit operating system, x64-based processor 
# 64GB ram and 4TB SSD
########################  
# Random forest regression for Quits Data
# Code adapted from STAT 515 Labs, Original By:       Daniel Carr
########################
library(randomForest)
library(lattice)
library(hexbin)
setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="Quit_2016_2021q.csv", header = TRUE, 
               sep=',', row.names =1)
View(QI)
head(QI)
set.seed(4543)  
set.seed(137)


RfManuf <- randomForest(x = QI[ ,1:11], y=QI[, 3],
                      importance=TRUE,  proximity=FALSE, ntree=5000, keepForest=TRUE)
RfManuf

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

imp <- importance(RfManuf)
imp
imp <- importance(RfTrade)
imp
imp <- importance(RfEduc)
imp
imp <- importance(RfLeis)
imp
varImpPlot(RfManuf,cex=.8)
length(imp)
varImpPlot(RfTrade,cex=.8)
length(imp)
varImpPlot(RfLeis,cex=.8)
length(imp)
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

