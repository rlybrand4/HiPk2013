#Purpose: Combine data frames from High Park data to create datasets for analysis
#author: Dave Moore
#date 04.03.2015

#USEFUL data munging libraries
#install the dplyr package then install the libraries below
library(dplyr)
library(shiny)
library(ggplot2)
#Load the HiPK2013 dataset containing all the data frames

load (file="HiPK2013.rdata")
