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

pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)

# Standardize the data
setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
df <- read.csv(file="QuitsIndustry_2001-2021r.csv", header = TRUE, 
               sep=',', row.names =1)
df <- read.csv(file="Quit_2016_2021q.csv", header = TRUE, 
               sep=',', row.names =1)
head(df)
View(df)
windows(width=12, height=12)
# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 15,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

#fviz_nbclust(df, FUNcluster, method = c("silhouette", "wss", "gap_stat"))

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

###########30 approaches together using nbclust @@@@@@@@@@@@@@@@@@@@
View(df)
NbClust(df, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")
dev.off()