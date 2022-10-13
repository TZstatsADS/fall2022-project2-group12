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

if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}

if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}

if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}

if (!require("sf")) {
  install.packages("sf")
  library(sf)
}

if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}

if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

if (!require('stringr')) {
  install.packages("stringr")  # Install & load stringr
  library("stringr")
}

#----Yuli----#
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
  summarise(mean=sum(Consumption..KWH.))

water_groupby <- water2 %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise(mean = sum(Consumption..HCF.))

gas_groupby <- gas2 %>% 
  group_by(Borough,Revenue.Month) %>% 
  summarise(mean = sum(Consumption..Therms.))

# Merge three energy
electric_groupby$Energy <-  'Electric'
water_groupby$Energy <- 'Water'
gas_groupby$Energy <- 'Heating.Gas'
gas_groupby <- gas_groupby[gas_groupby$Revenue.Month!='2022-04',]

energy_yuli <- rbind(electric_groupby,water_groupby,gas_groupby)
colnames(energy_yuli)[3] <- 'Sum.Consumption'

# Xilin ----
####data processing
# Import the datasets 
electric_Xilin <- read.csv('../data/Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water_Xilin <- read.csv('../data/Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas_Xilin <- read.csv('../data/Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')

#load address data
res_address_Xilin<-read.csv("../data/NYCHA_Residential_Addresses.csv")
address_data_Xilin<-res_address_Xilin%>%select(TDS..,ZIP.CODE)
address_data_Xilin<-distinct(address_data_Xilin,TDS..,.keep_all = TRUE)

####electricity####
electri_comsumption_Xilin<-electric_Xilin%>%
  subset(Revenue.Month>=2019-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..KWH.)

#merge two datasets
electricity_data_Xilin<-merge(x=address_data_Xilin,y=electri_comsumption_Xilin,by="TDS..")

#sum of each month consumption
electricity_data_Xilin<-electricity_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..KWH.))

electricity_data_Xilin$Revenue.Month<-as.Date(paste(electricity_data_Xilin$Revenue.Month,"-01",sep=""))
electricity_data_Xilin$ZIP.CODE<-as.character(electricity_data_Xilin$ZIP.CODE)
electricity_data_Xilin<-electricity_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

#### water ####
water_comsumption_Xilin<-water_Xilin%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..HCF.)

#merge two datasets
water_data_Xilin<-merge(x=address_data_Xilin,y=water_comsumption_Xilin,by="TDS..")

#sum of each month consumption
water_data_Xilin<-water_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..HCF.))

water_data_Xilin$Revenue.Month<-as.Date(paste(water_data_Xilin$Revenue.Month,"-01",sep=""))
water_data_Xilin$ZIP.CODE<-as.character(water_data_Xilin$ZIP.CODE)
water_data_Xilin<-water_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

#### gas ####
heat_comsumption_Xilin<-gas_Xilin%>%
  subset(Revenue.Month>=2020-01)%>%
  select(Development.Name,Borough,TDS..,Revenue.Month,X..days,Consumption..Therms.)

#merge two datasets
heat_data_Xilin<-merge(x=address_data_Xilin,y=heat_comsumption_Xilin,by="TDS..")

#sum of each month consumption
heat_data_Xilin<-heat_data_Xilin%>%
  group_by(Revenue.Month,ZIP.CODE)%>%
  summarise(month_consumption=sum(Consumption..Therms.))

heat_data_Xilin$Revenue.Month<-as.Date(paste(heat_data_Xilin$Revenue.Month,"-01",sep=""))
heat_data_Xilin$ZIP.CODE<-as.character(heat_data_Xilin$ZIP.CODE)
heat_data_Xilin<-heat_data_Xilin %>%
  dplyr::mutate(Reve_Year = lubridate::year(Revenue.Month), 
                Reve_Month = lubridate::month(Revenue.Month), )

# Sherry ----
electric_Sherry <- read.csv('../data/Electric_Consumption_And_Cost__2010_-_Feb_2022_.csv')
water_Sherry <- read.csv('../data/Water_Consumption_And_Cost__2013_-_Feb_2022_.csv')
gas_Sherry <- read.csv('../data/Heating_Gas_Consumption_And_Cost__2010_-__Feb_2022_.csv')
covid_Sherry <- read.csv('../data/cases-by-day.csv')
location_data_Sherry <- st_read("../data/Borough_Boundaries/geo_export_a59a7c07-bd70-4b11-af9f-011b5ad2a963.shp")

# Clean & Edit 

## Energy dataset 
electric_Sherry <- electric_Sherry[,c('Borough','Revenue.Month','Consumption..KWH.')]
water_Sherry <- water_Sherry[,c('Borough','Revenue.Month','Consumption..HCF.')]
gas_Sherry <- gas_Sherry[,c('Borough','Revenue.Month','Consumption..Therms.')]

# ### filter data by date ----
electric_Sherry <- subset(electric_Sherry, Revenue.Month > 2019)
water_Sherry <- subset(water_Sherry, Revenue.Month > 2019)
gas_Sherry <- subset(gas_Sherry, Revenue.Month > 2019)
# add row info of electric at 2022-02 for 5 boroughs
add_electric_Sherry <- data.frame("Borough" = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND"),
                                  "Revenue.Month" = c('2022-02', '2022-02', '2022-02','2022-02','2022-02'),
                                  "Consumption..KWH." = c(0,0,0,0,0))
electric_Sherry <- rbind(electric_Sherry, add_electric_Sherry)

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

# ### combine energy dataset ----
electric_groupby_Sherry$Energy <-  'Electric'
water_groupby_Sherry$Energy <- 'Water'
gas_groupby_Sherry$Energy <- 'Heating.Gas'
gas_groupby_Sherry <- gas_groupby_Sherry[gas_groupby_Sherry$Revenue.Month != '2022-04', ]
energy_Sherry <- rbind(electric_groupby_Sherry, water_groupby_Sherry, gas_groupby_Sherry)
colnames(energy_Sherry)[3] <- 'Avg.Consumption'
energy_Sherry$Revenue.Month <- anytime::anydate(energy_Sherry$Revenue.Month) # convert to Date
## Covid dataset 
### clean & edit covid datset ----
covid_Sherry <- covid_Sherry[,c("date_of_interest", "BX_CASE_COUNT", "BK_CASE_COUNT", "MN_CASE_COUNT","QN_CASE_COUNT","SI_CASE_COUNT")]
covid_Sherry$date_of_interest <- anytime::anydate(covid_Sherry$date_of_interest) # convert type to Date
covid_Sherry <- setDT(covid_Sherry)[, lapply(.SD, sum), by = lubridate::floor_date(date_of_interest, "month")]
BRONX_Sherry <- covid_Sherry[,c('lubridate','BX_CASE_COUNT')] %>% rename('covid_case_count' = 'BX_CASE_COUNT') %>% mutate('Borough' = 'BRONX')
BROOKLYN_Sherry <- covid_Sherry[,c('lubridate','BK_CASE_COUNT')] %>% rename('covid_case_count' = 'BK_CASE_COUNT') %>% mutate('Borough' = 'BROOKLYN')
MANHATTAN_Sherry <- covid_Sherry[,c('lubridate','MN_CASE_COUNT')] %>% rename('covid_case_count' = 'MN_CASE_COUNT') %>% mutate('Borough' = 'MANHATTAN')
QUEENS_Sherry <- covid_Sherry[,c('lubridate','QN_CASE_COUNT')] %>% rename('covid_case_count' = 'QN_CASE_COUNT') %>% mutate('Borough' = 'QUEENS')
STATEN_ISLAND_Sherry <- covid_Sherry[,c('lubridate','SI_CASE_COUNT')] %>% rename('covid_case_count' = 'SI_CASE_COUNT') %>% mutate('Borough' = 'STATEN ISLAND')
covid_Sherry <- do.call("rbind", list(BRONX_Sherry, BROOKLYN_Sherry, MANHATTAN_Sherry, QUEENS_Sherry, STATEN_ISLAND_Sherry))

### combine covid & energy dataset  
test_Sherry  <- merge(energy_Sherry, covid_Sherry, by.x=c("Borough", "Revenue.Month"), by.y=c("Borough", "lubridate"))

location_data_Sherry$boroname <-  toupper(location_data_Sherry$boroname)
test_Sherry <- geo_join(spatial_data = location_data_Sherry, data_frame = test_Sherry, by_sp = 'boroname', by_df = 'Borough', how = 'inner')
test_Sherry <- test_Sherry[,c('Revenue.Month','Energy','Avg.Consumption','covid_case_count','geometry','boroname')] %>% rename('Borough' = 'boroname')
# saveRDS(test_Sherry, "final_dataset_Sherry.rds")
# test_Sherry <- readRDS("../data/final_dataset_Sherry.rds")

#---Nour---#
###import datasets for antibody tests#####
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

#----Donglai----#
df_daily_case_donglai=read.csv("../data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
df_daily_case_donglai$date_of_interest<-as.Date(df_daily_case_donglai$date_of_interest,"%m/%d/%Y")