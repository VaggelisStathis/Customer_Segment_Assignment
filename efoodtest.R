library(echarts4r)
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(readxl)
library(zoo)
library(tidyverse)
library(tm)
library(lubridate)
library(Hmisc)
library(corrplot)


#Loading the original dataset of the assessment. Please put the file with the data in the same folder with the  
#df <- read.csv("Assessment exercise dataset - orders.csv", colClasses=c("order_id"="character"))

#Please navigate to the data file in your computer
df <- read.csv(file.choose(), colClasses=c("order_id"="character"))

#file.choose()
#Loading population data of 2011 
citiesPop <- read.csv("CitiesPop.csv") 
#Loading the data taken from the first query
Query1 <- read.csv("query12.csv") 
##Loading the data taken from the second query
Query2 <- read.csv("Query2.csv") 
#merging the latest 2 datasets
Queries <-  merge(x = Query1, y = Query2, by.x='city', by.y='city', all = TRUE)

#taking a first look 
str(df)
class(df$date)

#looking for correlations
cor(Queries[sapply(Queries, is.numeric)])
corrplot(cor(Queries[sapply(Queries, is.numeric)]))

#Taking a look at quadrants
describe(df)

#keeping only the cities with orders > 1000 in the original df
dfbig <- df%>%
  group_by(city ) %>%
  count(city) %>%
  filter(n >  1000)  

df <- df %>%
  filter(city  %in% dfbig$city) 

df$date <- as.Date(format(df$order_timestamp, format="%Y-%m-%d"))

#Adding a cities population column in the original dataset
dftemp <- merge(x = df, y = citiesPop, by.x='city', by.y='Πόλη', all = TRUE)


#finding the number of users per city
NumOfUsersPerCity <- df %>%
  group_by(city) %>%
  dplyr::summarize(NumOfUsers = n_distinct(user_id))

#creating a column  based on the Apps penetration for each city: AppPenet := number of users / population
dftemp <- merge(x = NumOfUsersPerCity, y = citiesPop, by.x='city', by.y='Πόλη', all = TRUE)
dftemp$Πληθυσμός.2011<-as.character(dftemp$Πληθυσμός.2011)
dftemp <- dftemp %>% mutate(Πληθυσμός.2011 = as.numeric(gsub("\\.", "", Πληθυσμός.2011)))

dftemp$AppPenet <- round(dftemp$NumOfUsers/dftemp$Πληθυσμός.2011*100, 2)

#Checking the quadrants in order to create a score column based on the latest AppPenet Column. Greater penetration => Bigger number 
summary(dftemp)


dftemp$PenScore <- 4
dftemp$PenScore[dftemp$AppPenet <= 3.955] <- 1
dftemp$PenScore[dftemp$AppPenet > 3.955 & dftemp$AppPenet <=8.550    ] <- 2
dftemp$PenScore[dftemp$AppPenet > 8.550   & dftemp$AppPenet <=13.045] <- 3

#Creatng a score column based on each city Population

dftemp$CitySize <- 1
dftemp$CitySize[dftemp$Πληθυσμός.2011 > 15944] <- 2
dftemp$CitySize[dftemp$Πληθυσμός.2011 > 21895] <- 3
dftemp$CitySize[dftemp$Πληθυσμός.2011 > 50774] <- 4

#removing useless columns from the temporary dataset and merging it with the original dataset
dftemp <- dftemp[,-(2:5)]   

df <- merge(x = df, y = dftemp, by.x='city', by.y='city', all = TRUE)


#first plot cuisine frequency. Just to take a look at the importance of the breakfast cuisine.

df2 <- 
  df %>%
  group_by(cuisine, date ) %>%
  count(cuisine)  

library(tidyr)
df2_wide <- dcast(df2, date ~ cuisine)

df2_wide %>%
  e_charts(x = date) %>%
  e_line(serie = Breakfast) %>%
  e_line(serie = Meat)%>%
  e_line(serie = Italian) %>%
  e_line(serie = `Street food`)%>%
  e_x_axis( name = 'Date', nameLocation = 'center', nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'bottom', lineHeight = -50) )  %>% 
  e_y_axis( name = 'Number Of Orders', nameLocation = 'center',  nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'top', lineHeight = -100) ) %>%
  e_tooltip(trigger = "axis")  %>%
  e_title(
    text = "Cuisine Order Frequency"
  )


#######################

# Creating a score column (FScore) based on the frequency of orders of each customer. Greater number of orders => bigger score 

dfloyal <- 
  df %>%
  group_by(user_id) %>%
    count(user_id) #%>%filter(n >  3) 

summary(dfloyal)

dfloyal$FScore <- 1
dfloyal$FScore[dfloyal$n > 1] <- 2
dfloyal$FScore[dfloyal$n > 2  ] <- 3
dfloyal$FScore[dfloyal$n > 4  ] <- 4

dfloyal <- dfloyal[,-2]   

# Creating a score column (FBRScore) based on the frequency of breakfast orders of each customer. Greater number of orders => bigger score.
# Customer with no breakfast orders get frequency 0 and are placed in the first group.

dfBRloyal <- 
  df %>%
  filter(cuisine  %in% c( "Breakfast" )) %>% 
  group_by(user_id) %>%
  count(user_id) 

summary(dfBRloyal)

dfFFBM <- merge(x = dfloyal, y = dfBRloyal, by.x='user_id', by.y='user_id', all = TRUE)
dfFFBM$n[is.na(dfFFBM$n)] <- 0

summary(dfFFBM)

dfFFBM$FBRScore <- 1
dfFFBM$FBRScore[dfFFBM$n == 1] <- 2
dfFFBM$FBRScore[dfFFBM$n == 2  ] <- 3
dfFFBM$FBRScore[dfFFBM$n > 2  ] <- 4
dfFFBM$FBRScore[dfFFBM$n > 3  ] <- 5

dfFFBM <- dfFFBM[,-3]   

# Creating a score column (MScore) based on the total amount of money that each customer spend. Bigger amount => bigger score 

dfMon <- df %>% 
  group_by(user_id) %>% 
  summarise( Monetary = sum(amount))

summary(dfMon)

dfMon$MScore <- 1
dfMon$MScore[dfMon$Monetary >= 11.60] <- 2
dfMon$MScore[dfMon$Monetary >= 23.30  ] <- 3
dfMon$MScore[dfMon$Monetary > 47.40  ] <- 4

dfMon <- dfMon[,-2]   


dfFFBM <- merge(x = dfFFBM, y = dfMon, by.x='user_id', by.y='user_id', all = TRUE)

dfsa <- merge(x = df, y = dfFFBM, by.x='user_id', by.y='user_id', all = TRUE)


################Trying to find out about cash payments################

#We will now visualize if there is difference between the way customers (in terms of orders frequency, breakfast frequency, and monetary scores) pay for their orders.

sample3 <- dfsa %>% 
  dplyr::group_by(FBRScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop")%>%
  filter(paid_cash=='true')

sample4 <- dfsa %>% 
  dplyr::group_by(FBRScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  group_by(FBRScore) %>%
  dplyr::summarise(sum = sum(Count) )

plotdf <- merge(x = sample3, y = sample4, by.x='FBRScore', by.y='FBRScore', all = TRUE)
plotdf <- plotdf %>% dplyr::mutate(Percent = round(Count/sum*100,2))

plotdf %>%
  e_charts(FBRScore) %>%
  e_bar(Percent, name = "Breakfast score levels") %>%
  e_title("Percentage of Cash Payments for each level",
          subtext = "FBRScore")%>%
  e_x_axis( name = 'Score level', nameLocation = 'center', nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'bottom', lineHeight = -50) )  %>% 
  e_y_axis( name = 'Percentage', nameLocation = 'center',  nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'top', lineHeight = -100) ) %>%
  e_legend(show = FALSE)


sample3 <- dfsa %>% 
  dplyr::group_by(FScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop")%>%
  filter(paid_cash=='true')

sample4 <- dfsa %>% 
  dplyr::group_by(FScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  group_by(FScore) %>%
  dplyr::summarise(sum = sum(Count) )

plotdf1 <- merge(x = sample3, y = sample4, by.x='FScore', by.y='FScore', all = TRUE)
plotdf1 <- plotdf1 %>% dplyr::mutate(Percent = round(Count/sum*100,2))

plotdf1 %>%
  e_charts(FScore) %>%
  e_bar(Percent) %>%
  e_title("Percentage of Cash Payments for each level",
          subtext = "FScore")%>%
  e_x_axis( name = 'Score level', nameLocation = 'center', nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'bottom', lineHeight = -50) )  %>% 
  e_y_axis( name = 'Percentage', nameLocation = 'center',  nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'top', lineHeight = -100) ) %>%
  e_legend(show = FALSE)


sample3 <- dfsa %>% 
  dplyr::group_by(MScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop")%>%
  filter(paid_cash=='true')

sample4 <- dfsa %>% 
  dplyr::group_by(MScore, paid_cash) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  group_by(MScore) %>%
  dplyr::summarise(sum = sum(Count) )

plotdf2 <- merge(x = sample3, y = sample4, by.x='MScore', by.y='MScore', all = TRUE)
plotdf2 <- plotdf2 %>% dplyr::mutate(Percent = round(Count/sum*100,2))



plotdf2 %>%
  e_charts(MScore) %>%
  e_bar(Percent) %>%
  e_title("Percentage of Cash Payments for each level",
          subtext = "Mcore")%>%
  e_x_axis( name = 'Score level', nameLocation = 'center', nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'bottom', lineHeight = -50) )  %>% 
  e_y_axis( name = 'Percentage', nameLocation = 'center',  nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'top', lineHeight = -100) ) %>%
  e_legend(show = FALSE)


#######################We will look for differences in the way customers with FBRScore = 5 pay################################

dfexp <- dfsa %>%
  filter(FBRScore == 5) 


ggplot() + 
  geom_boxplot(data = dfexp, 
               aes(x = as.factor(paid_cash), y = amount)) + 
 # geom_point(data = greecesuic2, aes(x = as.factor(amount), y = Percentage, color = "Greece", group = 1), size = 2)+
  labs(title = "Boxplots for Difeerence between payment Ammount for loyal Breakfast Customers.",
       subtitle = "With respect to payment Method. Digital vs Cash",
       x = "Cash",
       y = "Amount Paid") +
  coord_cartesian(ylim = c(0, 25)) # In order to remove some of the outliers that mess the plot

#Because we cannot see an obvious difference we will conduct a t-test

dftrue <- dfexp %>%
  filter(paid_cash == 'true')

dffalse <- dfexp %>%
  filter(paid_cash == 'false')

t.test(dftrue$amount, dffalse$amount)   #p-value < 2.2e-16. small p-value. we can reject the null hypothesis. 
#The alternative hypothesis stands. alternative hypothesis: true difference in means is not equal to 0
#Thus, the two datasets differ. 
#mean of x mean of y 
#6.306749  6.512272 

# We will do the same for order frequency

dfgrouped <- dfexp %>%
  dplyr::group_by(user_id,paid_cash ) %>%
  dplyr::summarise(Count = n(), .groups = "drop") 
  
  ggplot() + 
  geom_boxplot(data = dfgrouped, 
               aes(x = as.factor(paid_cash), y = Count)) + 
  labs(title = "Boxplots for Number of Orders for loyal Breakfast Customers.",
       subtitle = "With respect to payment Method. Digital vs Cash",
       x = "Cash",
       y = "Number of Orders") 

  dftrue <- dfgrouped %>%
    filter(paid_cash == 'true')
  
  dffalse <- dfgrouped %>%
    filter(paid_cash == 'false')
  
  t.test(dftrue$Count, dffalse$Count)  #p-value < 5.071e-06. small p-value. we can reject the null hypothesis. 
  #The alternative hypothesis stands. alternative hypothesis: true difference in means is not equal to 0
  #Thus, the two datasets differ. 
  #mean of x mean of y 
  #8.843650  8.412487   
  
  
# And for FScore as well
  
dfexp <- dfsa %>%
  filter(FScore == 4) 

boxplot(amount~paid_cash,
        data=dfexp,
        main="Boxplots for Difeerence between payments.",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown")
        
        ggplot() + 
          geom_boxplot(data = dfexp, 
                       aes(x = as.factor(paid_cash), y = amount)) + 
          # geom_point(data = greecesuic2, aes(x = as.factor(amount), y = Percentage, color = "Greece", group = 1), size = 2)+
          labs(title = "Boxplots for EU Suicide Rates Per Year",
               subtitle = "Greece vs EU",
               x = "Year",
               y = "Rate") +
          coord_cartesian(ylim = c(0, 25)) # 

dftrue <- dfexp %>%
  filter(paid_cash == 'true')
        
dffalse <- dfexp %>%
  filter(paid_cash == 'false')
        
t.test(dftrue$amount, dffalse$amount)   #p-value = 0.2222. Can't conclude that there is a significant difference between the two datasets 

        
####################Lets try to analyse  the number of orders#############################

#I will now create a similar score (FWBRScore, frequency without breakfast) for the users, 
#that will express the frequency of orders despite the breakfast  orders of each user.
#For example, a user with 20 orders of which 15 are breakfast orders, will be assessed as he has 5 orders.
#I took this approach because a great FBScore => great FScore.

dfexp1 <- dfsa %>%
  filter(cuisine  %in% c( "Italian","Meat",  "Street food" ))%>%
  dplyr::group_by(user_id) %>%
  dplyr::summarise(NonBreakfastOrders = n(), .groups = "drop")


dfFFWBFBM <- merge(x = dfFFBM, y = dfexp1, by.x='user_id', by.y='user_id', all = TRUE)
dfFFWBFBM$NonBreakfastOrders[is.na(dfFFWBFBM$NonBreakfastOrders)] <- 0
summary(dfFFWBFBM)

dfFFWBFBM$FWBRScore <- 1
dfFFWBFBM$FWBRScore[dfFFWBFBM$NonBreakfastOrders == 1] <- 2
dfFFWBFBM$FWBRScore[dfFFWBFBM$NonBreakfastOrders == 2  ] <- 3
dfFFWBFBM$FWBRScore[dfFFWBFBM$NonBreakfastOrders > 2  ] <- 4
dfFFWBFBM$FWBRScore[dfFFWBFBM$NonBreakfastOrders > 3  ] <- 5 # The extra level is to match your business model
dfFFWBFBM <- dfFFWBFBM[,-c(5)] 

dfsa <- dfsa[,-c(11,12,13)] 
dfsa <- merge(x = dfsa, y = dfFFWBFBM, by.x='user_id', by.y='user_id', all = TRUE)

#We will now create 20 segments by concatenating FWBRScore and Mscore. For instance, a user 
dfFFWBFBM$MFWBRScores <- paste(dfFFWBFBM$FWBRScore, dfFFWBFBM$MScore)

finalChart<- dfFFWBFBM %>%
  filter(FBRScore == 5) %>%
  group_by(MFWBRScores)%>%
  dplyr::summarise(Count = n(), .groups = "drop")

finalChart %>%  e_charts(MFWBRScores) %>%
  e_bar(Count, names = "Users") %>%
  e_tooltip(trigger = "axis")  %>%
  e_title("Frequency of Users Per Segment",
          subtext = "FWBRScore, Mscore")%>%
  e_x_axis( name = 'User Segments', nameLocation = 'center', nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'bottom', lineHeight = -50) )  %>% 
  e_y_axis( name = 'Frequency', nameLocation = 'center',  nameTextStyle = list(fontSize  = 14, align = 'center', verticalAlign = 'top', lineHeight = -100) ) #%>% e_legend(show = FALSE)

finalChart %>%  e_charts(MFWBRScores) %>%
  e_pie(Count, names = "Users") %>%
  e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br/>Total: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  ")) %>%
  e_title("Frequency of Users Per Segment",
          subtext = "FWBRScore, Mscore")%>%
  e_legend(show = FALSE)


#As we can see, most of the users that are loyal (with respect to  breakfast frequency score), have the highest FWBRScore and MScore and they clearly belong in the loyal group. 
#For the same group, we can consider the segments that sum to seven or more. Thus, 34, 43, 44, 52 and 53. Those that sum to six (24,42,33) we will place them in the 'Potential Loyalists' group
#Those that sum to four and five (13,14,23,32,22) we will place them in the 'Promising' group. 
#Those that sum to two and three (13,14,23,32,22) we will place them in the 'Hesitant' group.

#dfShiny < dfsa[,-c(3,4,7,8,11,12,13)] 

#write.csv(dfsa,"finaldataframe.csv", row.names = FALSE)
#write.csv(dfFFWBFBM,"finalUserdataframe.csv", row.names = FALSE)
write.csv(Queries,"BothQueries.csv", row.names = FALSE)
