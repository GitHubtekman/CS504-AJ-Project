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
install.packages("ggcorrplot")
# Install
library(ggcorrplot)

setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="Quit_2015_2020q.csv", header = TRUE, 
                               sep=',', row.names =1)

par(mfrow=c(4,4))
view(QI)
corr <- round(cor(QI), 3)
head(corr[,1:6])
corr
windows(width=12, height=12) 
heatmap(corr)

ggcorrplot(corr,hc.order = FALSE,
           lab = TRUE, title = "      BLS QUITS by Industry 2016-2021\n                            5YR\n                 Correlation Matrix" ) 
 

dev.off()
par(mfrow=c(6,6))
QI2 <- read.csv(file="Quit_2015_2020m.csv", header = TRUE, 
               sep=',', row.names =1)

dd <- dist(scale(QI2), method="euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, labels = NULL, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")
plot(hc)
plot(hc, hang = -1, cex = 0.6)
hcd <- as.dendrogram(hc)
plot(hcd, xlim = c(1, 8), ylim = c(1,8))
summary(QuitsIndustry2019_2020)
