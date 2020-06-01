##Required Libraries
library(forecast)
library(zoo)
library(ggplot2)

#Getting the Data
salesdata <- read.csv("C:/thesis/tabiri_biz/data/template.csv")



#plot sales and Purchases

#Sales

ggplot(data = salesdata, aes(x = Date, y = Sales)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Sales (Quantities)",
       title = "SME Sales Data",
       subtitle = "Kenya 2020")



ggplot(data = salesdata, aes(x = Date, y = Sales)) +
  geom_line()
