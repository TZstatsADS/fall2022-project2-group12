
server_donglai <- function(input_donglai, output_donglai) {
  output_donglai$tsPlot_donglai <-renderPlot({
    ggplot(data=df_daily_case_donglai,aes_string(x=input_donglai$x_donglai, y=input_donglai$y_donglai))+
      geom_bar(stat="identity",fill="#6baed6")+
      theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))+
      labs(title = "Covid-19 Statistics from 2020 to 2022")+
      ylab("Count")+
      xlab("Date")+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
    
  })
}