# CS504 Working Version for Final Project
# Date 12-3-21
#########################
# Bill Thomson 
# Original code provided for RF  Lab 
# R Studio Version 1.4.1106
# R version 4,04
# Windows 10, i7 10th gen 64-bit operating system, x64-based processor 
# 64GB ram and 4TB SSD
########################  
# Heatmap wit Dendrogram and Correlation Matrix Quits Data
########################

install.packages("ggcorrplot")
# Install
library(ggcorrplot)
library("tidyr")
library(gplots)
library("RColorBrewer")
setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="Quit_2016_2021q.csv", header = TRUE, 
                             sep=',', row.names =1)
QI2 <- read.csv(file="QuitsIndustry_2001-2021r.csv", header = TRUE, 
              sep=',', row.names =1)
display.brewer.all()
par(mfrow=c(4,4))
View(QI)
corr <- round(cor(QI), 3)
head(corr[,1:6])
corr
windows(width=8, height=10) 
#pdf(file="Heatmap_Dendrogram_2016_2021.pdf",width=8,height=10)
    
#Use Euclidean distance with Ward's linkage
heatmap(corr)
heatmap.2(corr, key= T, main = "\n\n\n   QUITS by Industry 2001-2021 \n     Correlation Matrix 
     Dendrogram\n    Euclidean Distances and Ward Clustering", margins=c(11,11),lwid = c(1.5,5),lhei = c(1.2,5),
          trace="none", col = rev(heat.colors(16)), scale = "column",
          density.info="none",dendrogram="row", ylab = ("Industry Groups"),
          xlab = "Industry Groups",
          distfun = function(x) dist(x, method="euclidean"),
          hclustfun = function(x) hclust(x, method="ward.D2"))

#Use minus Pearson correlation distance with average linkage
#lwid = c(1,4),lhei = c(1,4),

heatmap.2(corr, key= T, margins=c(11,11),trace="none", col = rev(heat.colors(24)), 
          density.info="none",dendrogram="row",
          distfun = function(x) as.dist(1-cor(t(x))),
          hclustfun = function(x) hclust(x, method="average"))


dev.off()

windows(width=12, height=12) 
ggcorrplot(corr,hc.order = FALSE,
           lab = TRUE, title = "                                        BLS QUITS by Industry 2001-2021
                                                               20YR                                                        
                                                      Correlation Matrix" ) 

dev.off() 

par(mfrow=c(6,6))
QI2 <- read.csv(file="QuitsIndustry_2001_2021m.csv", header = TRUE, 
               sep=',', row.names =1)

dd <- dist(scale(QI2), method="euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, labels = NULL, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")
plot(hc)
plot(hc, hang = -1, cex = 1)
hcd <- as.dendrogram(hc)
plot(hcd, cex=.1, xlim = c(1, 11), ylim = c(1,11))


