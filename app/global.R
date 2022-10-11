# Install related packages ----
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

if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}

if (!require('stringr')) {
  install.packages("stringr")  # Install & load stringr
  library("stringr")
}


# Energy consumption ----
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

# Antibody test ----
antibody_ZIP = read.csv('../data/DOHMH_COVID-19_Antibody-by-Modified_ZIP_Code_Tabulation_Area.csv')
antibody_poverty= read.csv('../data/DOHMH_COVID-19_Antibody-by-Neighborhood_Poverty.csv')
antibody_poverty <- antibody_poverty %>% 
  select(demo_variable, PERCENT_POSITIVE, NUM_PEOP_TEST) %>%
  rename(poverty_level=demo_variable) %>%
  mutate(PERCENT_POSITIVE=PERCENT_POSITIVE*100) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyA_Low', 'low')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyB_Medium', 'medium')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyC_High', 'high')) %>%
  mutate(poverty_level = replace(poverty_level, poverty_level=='PovertyD_Very_High', 'very high'))
antibody_week <- read.csv('../data/DOHMH_COVID-19_Antibody-by-Week.csv')
antibody_week[c('month', 'day','year')] <- str_split_fixed(antibody_week$WEEKDATE, '/', 3)

###----Donglai---#
df_daily_case <- read.csv('../data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv')

# Comparison ----
### import additional dataset ----
electric <- read.csv('../data/Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water <- read.csv('../data/Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas <- read.csv('../data/Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')
covid <- read.csv('../data/cases-by-day.csv')
data <- st_read("../data/Borough_Boundaries/geo_export_a59a7c07-bd70-4b11-af9f-011b5ad2a963.shp")

### edit &clean 
##### energy consumption ----
electric <- electric[,c('Borough','Revenue.Month','Consumption..KWH.')]
water <- water[,c('Borough','Revenue.Month','Consumption..HCF.')]
gas <- gas[,c('Borough','Revenue.Month','Consumption..Therms.')]

electric <- subset(electric, Revenue.Month > 2019)
water <- subset(water, Revenue.Month > 2019)
gas <- subset(gas, Revenue.Month > 2019)

electric_groupby <- electric %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..KWH.), list(name = mean))

water_groupby <- water %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..HCF.), list(name = mean))

gas_groupby <- gas %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise_at(vars(Consumption..Therms.), list(name = mean))

# combine energy dataset 
electric_groupby$Energy <-  'Electric'
water_groupby$Energy <- 'Water'
gas_groupby$Energy <- 'Heating.Gas'
gas_groupby <- gas_groupby[gas_groupby$Revenue.Month!='2022-04',]
energy <- rbind(electric_groupby,water_groupby,gas_groupby)
colnames(energy)[3] <- 'Avg.Consumption'
energy$Revenue.Month <- anytime::anydate(energy$Revenue.Month) # convert to Date

##### covid ----
# clean & edit covid datset 
covid <- covid[,c("date_of_interest", "BX_CASE_COUNT", "BK_CASE_COUNT", "MN_CASE_COUNT","QN_CASE_COUNT","SI_CASE_COUNT")]
covid$date_of_interest <- anytime::anydate(covid$date_of_interest) # convert type to Date

# sum by month 
covid <- setDT(covid)[, lapply(.SD, sum), by = lubridate::floor_date(date_of_interest, "month")]

BRONX <- covid[,c('lubridate','BX_CASE_COUNT')] %>% rename('covid_case_count' = 'BX_CASE_COUNT') %>% mutate('Borough' = 'BRONX')
BROOKLYN <- covid[,c('lubridate','BK_CASE_COUNT')] %>% rename('covid_case_count' = 'BK_CASE_COUNT') %>% mutate('Borough' = 'BROOKLYN')
MANHATTAN <- covid[,c('lubridate','MN_CASE_COUNT')] %>% rename('covid_case_count' = 'MN_CASE_COUNT') %>% mutate('Borough' = 'MANHATTAN')
QUEENS <- covid[,c('lubridate','QN_CASE_COUNT')] %>% rename('covid_case_count' = 'QN_CASE_COUNT') %>% mutate('Borough' = 'QUEENS')
STATEN_ISLAND <- covid[,c('lubridate','SI_CASE_COUNT')] %>% rename('covid_case_count' = 'SI_CASE_COUNT') %>% mutate('Borough' = 'STATEN ISLAND')

# combine all boroughs 
covid <- do.call("rbind", list(BRONX, BROOKLYN, MANHATTAN, QUEENS, STATEN_ISLAND))

##### location info. ----
data <- data %>% rename('Borough' = 'boroname')
data$Borough <-  toupper(data$Borough)
test <- merge(energy, data, by='Borough') 
test <- na.omit(test)
### final dataset for comparison ----
test <- merge(test, covid, by.x=c("Borough", "Revenue.Month"), by.y=c("Borough", "lubridate"))









