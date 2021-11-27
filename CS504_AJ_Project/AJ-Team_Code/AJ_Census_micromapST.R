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
# Census micromapST
########################

library(micromapST)
library(data.table)
library(tidyverse)

setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
Census2015_2020 <- read.csv(file= "Final_Census_2015-2020_Trimed_11_21_21.csv", header = TRUE, 
                            sep=',', row.names =1)

view(Census2015_2020)
head(Census2015_2020)


Census2015_2020 <-Census2015_2020[-c(1)]

#"map","mapcum","maptail"and"mapmedian".

head(Census2015_2020)

panelDesc = data.frame(                   
  type=c("map","id","arrow","arrow","arrow","arrow"), 
  lab1=c("","","Unemployed","Employed","Unemployed","Employed"),
  lab2=c("","","Females","Females","Males","Males"),  
  lab3=c("","","","","",""),
  col1=c(NA,NA,"X2015_Un_Female","X2015_Empl_Female","X2015_Un_Male","X2015_Empl_Male"),
  col2=c(NA,NA,"X2020_Un_Female","X2020_Empl_Female","X2020_Un_Male","X2020_Empl_Male")
)

pdf(file="Employment_Gender2015_2020.pdf",width=7.5,height=10)

micromapST(Census2015_2020,panelDesc,sortVar=7,ascend=TRUE,
           title=c("                 Total Employment by State 
                   Employees by Gender
                   2015 to 2020"),plotNames = "full",
           details=list(SNBar.Middle.Dot=FALSE,SNBar.varht=TRUE))  
dev.off()

head(Census2015_2020)
View(Census2015_2020)
summary(Census2015_2020)
