library(RColorBrewer)
library(tm)
library(tidyverse)
library("readxl")
library(lattice) 
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(echarts4r)
library(reshape2)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(DT)
library(httr)    

set_config(use_proxy(url="10.3.100.207",port=8080))


#df <- read.csv("finaldataframe.csv")
df1 <- read.csv("finalUserdataframe.csv")

#Query1 <- read.csv("query12.csv") 
Queries <- read.csv("BothQueries.csv")

segments <- df1$MFWBRScores %>% unique()
