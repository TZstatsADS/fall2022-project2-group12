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
electric$Revenue.Year <- as.integer(substr(electric$Revenue.Month,1,4))
water$Revenue.Year <- as.integer(substr(water$Revenue.Month,1,4))
gas$Revenue.Year <- as.integer(substr(gas$Revenue.Month,1,4))

electric <- electric[electric$Revenue.Year>=2019,]
water <- water[water$Revenue.Year>=2019,]
gas <- gas[gas$Revenue.Year>=2019,]

# Select key columns from original datasets
electric <- electric[,c('Borough','Revenue.Month','Consumption..KWH.')]
water <- water[,c('Borough','Revenue.Month','Consumption..HCF.')]
gas <- gas[,c('Borough','Revenue.Month','Consumption..Therms.')]

# Sort data
electric <- electric[order(electric$Borough,electric$Revenue.Month),]
water <- water[order(water$Borough,water$Revenue.Month),]
gas <- gas[order(gas$Borough,gas$Revenue.Month),]

# Group by Borough and date, take average 
electric_groupby <- electric %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..KWH.), list(name = mean))

water_groupby <- water %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..HCF.), list(name = mean))

gas_groupby <- gas %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..Therms.), list(name = mean))

# Merge three energy
electric_groupby$Energy <-  'Electric'
water_groupby$Energy <- 'Water'
gas_groupby$Energy <- 'Heating.Gas'
gas_groupby <- gas_groupby[gas_groupby$Revenue.Month!='2022-04',]
energy <- rbind(electric_groupby,water_groupby,gas_groupby)
colnames(energy)[3] <- 'Avg.Consumption'
