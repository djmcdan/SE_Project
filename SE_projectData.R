#SE_projectData
suppressPackageStartupMessages(install.packages("ggmap"))

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(bit64))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggmap))

setwd("C:/Users/Darius/Documents/DF2015_Data/DF2015_revised") 

visitor <- fread("visitor.csv",stringsAsFactors=FALSE)
transactions <- fread("transactions.csv",stringsAsFactors=FALSE)
shopping <- fread("shopping.csv",stringsAsFactors=FALSE)
leads <- fread("leads.csv",stringsAsFactors=FALSE)
configuration <- fread("configuration.csv",stringsAsFactors=FALSE)
