# Install related packages
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Import the datasets 
electric <- read.csv('../data/Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water <- read.csv('../data/Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas <- read.csv('../data/Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')

# Select data from 2019-2022
electric2 <- electric
water2 <- water
gas2 <- gas

electric2$Revenue.Year <- as.integer(substr(electric2$Revenue.Month,1,4))
water2$Revenue.Year <- as.integer(substr(water2$Revenue.Month,1,4))
gas2$Revenue.Year <- as.integer(substr(gas2$Revenue.Month,1,4))

electric2 <- electric2[electric2$Revenue.Year>=2019,]
water2 <- water2[water2$Revenue.Year>=2019,]
gas2 <- gas2[gas2$Revenue.Year>=2019,]

# Select key columns from original datasets
electric2 <- electric2[,c('Borough','Revenue.Month','Consumption..KWH.')]
water2 <- water2[,c('Borough','Revenue.Month','Consumption..HCF.')]
gas2 <- gas2[,c('Borough','Revenue.Month','Consumption..Therms.')]

# Sort data
electric2 <- electric2[order(electric2$Borough,electric2$Revenue.Month),]
water2 <- water2[order(water2$Borough,water2$Revenue.Month),]
gas2 <- gas2[order(gas2$Borough,gas2$Revenue.Month),]

# Group by Borough and date, take average 
electric_groupby <- electric2 %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..KWH.), list(name = mean))

water_groupby <- water2 %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..HCF.), list(name = mean))

gas_groupby <- gas2 %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..Therms.), list(name = mean))

# Merge three energy
electric_groupby$Energy <-  'Electric'
water_groupby$Energy <- 'Water'
gas_groupby$Energy <- 'Heating.Gas'
gas_groupby <- gas_groupby[gas_groupby$Revenue.Month!='2022-04',]
energy <- rbind(electric_groupby,water_groupby,gas_groupby)
colnames(energy)[3] <- 'Avg.Consumption'
