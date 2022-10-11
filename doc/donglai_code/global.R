df_daily_case=read.csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
df_daily_case$date_of_interest<-as.Date(df_daily_case$date_of_interest,"%m/%d/%Y")