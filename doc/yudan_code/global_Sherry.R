
# Set up ----
## load packages ----
library(shiny) # for shiny app
# for edit data set
library(tidyverse) 
library(dplyr)
library(data.table)
# for map_plot & bar_plot
library(sf)
library(leaflet)
library(ggplot2)

## import dataset ----
# setwd("/Users/zhangyudan/Documents/Github/fall2022-project2-group12/data")

electric_Sherry <- read.csv('../data/Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water_Sherry <- read.csv('../data/Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas_Sherry <- read.csv('../data/Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')
covid_Sherry <- read.csv('../data/cases-by-day.csv')
location_data_Sherry <- st_read("../data/Borough_Boundaries/geo_export_a59a7c07-bd70-4b11-af9f-011b5ad2a963.shp")

# Clean & Edit ----
## Energy dataset ----
### select cols ----
electric_Sherry <- electric_Sherry[,c('Borough','Revenue.Month','Consumption..KWH.')]
water_Sherry <- water_Sherry[,c('Borough','Revenue.Month','Consumption..HCF.')]
gas_Sherry <- gas_Sherry[,c('Borough','Revenue.Month','Consumption..Therms.')]

### filter data by date ----
electric_Sherry <- subset(electric_Sherry, Revenue.Month > 2019)
water_Sherry <- subset(water_Sherry, Revenue.Month > 2019)
gas_Sherry <- subset(gas_Sherry, Revenue.Month > 2019)

### group by Borough & date to get mean(Consumption..KWH.) ----
electric_groupby_Sherry <- electric_Sherry %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..KWH.), list(name = mean))

water_groupby_Sherry <- water_Sherry %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..HCF.), list(name = mean))

gas_groupby_Sherry <- gas_Sherry %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..Therms.), list(name = mean))

### combine energy dataset ----
electric_groupby_Sherry$Energy <-  'Electric'
water_groupby_Sherry$Energy <- 'Water'
gas_groupby_Sherry$Energy <- 'Heating.Gas'
gas_groupby_Sherry <- gas_groupby_Sherry[gas_groupby_Sherry$Revenue.Month != '2022-04', ]
energy_Sherry <- rbind(electric_groupby_Sherry, water_groupby_Sherry, gas_groupby_Sherry)
colnames(energy_Sherry)[3] <- 'Avg.Consumption'
energy_Sherry$Revenue.Month <- anytime::anydate(energy_Sherry$Revenue.Month) # convert to Date

## Covid dataset ----
### clean & edit covid datset ----
covid_Sherry <- covid_Sherry[,c("date_of_interest", "BX_CASE_COUNT", "BK_CASE_COUNT", "MN_CASE_COUNT","QN_CASE_COUNT","SI_CASE_COUNT")]
covid_Sherry$date_of_interest <- anytime::anydate(covid_Sherry$date_of_interest) # convert type to Date

# sum by month 
covid_Sherry <- setDT(covid_Sherry)[, lapply(.SD, sum), by = lubridate::floor_date(date_of_interest, "month")]

BRONX_Sherry <- covid_Sherry[,c('lubridate','BX_CASE_COUNT')] %>% rename('covid_case_count' = 'BX_CASE_COUNT') %>% mutate('Borough' = 'BRONX')
BROOKLYN_Sherry <- covid_Sherry[,c('lubridate','BK_CASE_COUNT')] %>% rename('covid_case_count' = 'BK_CASE_COUNT') %>% mutate('Borough' = 'BROOKLYN')
MANHATTAN_Sherry <- covid_Sherry[,c('lubridate','MN_CASE_COUNT')] %>% rename('covid_case_count' = 'MN_CASE_COUNT') %>% mutate('Borough' = 'MANHATTAN')
QUEENS_Sherry <- covid_Sherry[,c('lubridate','QN_CASE_COUNT')] %>% rename('covid_case_count' = 'QN_CASE_COUNT') %>% mutate('Borough' = 'QUEENS')
STATEN_ISLAND_Sherry <- covid_Sherry[,c('lubridate','SI_CASE_COUNT')] %>% rename('covid_case_count' = 'SI_CASE_COUNT') %>% mutate('Borough' = 'STATEN ISLAND')

### combine all boroughs ----
covid_Sherry <- do.call("rbind", list(BRONX_Sherry, BROOKLYN_Sherry, MANHATTAN_Sherry, QUEENS_Sherry, STATEN_ISLAND_Sherry))

## Location Info. dataset ----
### combine location info.----
location_data_Sherry <- location_data_Sherry %>% rename('Borough' = 'boroname')
location_data_Sherry$Borough <-  toupper(location_data_Sherry$Borough)
test_Sherry <- merge(energy_Sherry, location_data_Sherry, by='Borough') 
test_Sherry <- na.omit(test_Sherry)

### combine covid ----
test_Sherry <- merge(test_Sherry, covid_Sherry, by.x=c("Borough", "Revenue.Month"), by.y=c("Borough", "lubridate"))
# export final dataset for future use
saveRDS(test_Sherry, "../data/final_dataset_Sherry.rds")

