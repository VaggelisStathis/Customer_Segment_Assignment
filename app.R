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
library(httr)    
set_config(use_proxy(url="10.3.100.207",port=8080))

ui <- fluidPage(
  titlePanel(h2('This is my first RShiny app!'))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)