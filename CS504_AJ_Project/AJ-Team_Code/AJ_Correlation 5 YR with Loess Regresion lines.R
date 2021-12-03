# CS504 Working Version for AJ Final Project
# Date 12-3-21
#########################
# Bill Thomson 
# Original code provided for RF  Lab 
# R Studio Version 1.4.1106
# R version 4,04
# Windows 10, i7 10th gen 64-bit operating system, x64-based processor 
# 64GB ram and 4TB SSD
########################  
# Correlation wit Loess Regression Lines Quits Data
# Code adapted from STAT 515 Labs, Original By: Daniel Carr
########################

library(GGally)
library(ggplot2)
setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QI <- read.csv(file="Quit_2016_2021q.csv", header = TRUE, 
               sep=',', row.names =1)
QI2 <- read.csv(file="QuitsIndustry_2001-2021r.csv", header = TRUE, 
                sep=',', row.names =1)
data <- QI
ggpairs(QI)
head(QI)
windows(width=9, height=9) ## For MS Windows
ggpair_fn <- function(data, mapping, ...){
  p <- ggplot(data=data, mapping = mapping) +
    geom_point() +
    geom_smooth(method='loess', fill="blue", color="blue", ...) +
    geom_smooth(method= 'lm', fill="red", color="red", ...)
  p
}
ggp1 = ggpairs(QI,columns = 1:11, title = "                                                                                                        Quits by Industy 2016-2021
                                                                                 Correlation Plots with Loess Regresion and Smoothing", lower = list(continuous = ggpair_fn))
ggp1 
ggp2 = ggpairs(QI,c(3,4,8,9),title = "                                                                                                        Quits by Industy 2016-2021
                                                                                 Correlation Plots with Loess Regresion and Smoothing", lower = list(continuous = ggpair_fn))
lower = list(continuous = ggpair_fn)
ggp2
