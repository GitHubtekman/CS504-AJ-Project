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
# MicromapST % Changes for Quits and Employment_Unemployment 2019-2020
########################library(micromapST)
library(micromapST)
library(data.table)
library(tidyverse)
setwd("C:/Users/billa/Desktop/CS504 Working Data Sets")
QuitsPerc2019_2020 <- read.csv(file= "Quits Changes with 2019-2020 Empl_Un Data_Cleaned_11_21_21_w_percent_change.csv", header = TRUE, 
                            sep=',', row.names =1)

view(QuitsPerc2019_2020)
head(QuitsPerc2019_2020)

QuitsPerc2019_2020 <-QuitsPerc2019_2020[-c(1)]

#"map","mapcum","maptail"and"mapmedian".

head(QuitsPerc2019_2020)

panelDesc = data.frame(                   
  type=c("map","id","bar","bar","bar"), 
  lab1=c("","","Quit","Unemployed","Employed"),
  lab2=c("","","% Change"," %Change","%Change"),  
  lab3=c("","","","",""),
  col1=c(NA,NA,"Quits_1yr_chg.","Un_1.yr_Chg.","Emply_1.yr_Chg."),
  col2=c(NA,NA,"X2020_Un_Female","X2020_Un_Male","X2020_Empl_Female")
)

pdf(file="ChangeQuitsEmp2019_2020.pdf",width=7.5,height=10)

micromapST(QuitsPerc2019_2020,panelDesc,sortVar=7,ascend=TRUE,
           title=c("                 % Change Comparision
                   2019 to 2020"),plotNames = "full",
           details=list(SNBar.Middle.Dot=FALSE,SNBar.varht=TRUE))  
dev.off()