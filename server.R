library(gmailr)
library(ReporteRs)
library(stringr)
library(lubridate)

source('data/sili2.R')
gmail_auth('data/silisales1.json')

shinyServer(function(input, output) {
  output$first <- renderText({
    as.character(input$dates[1])
  })
  output$second <- renderText({
    as.character(input$dates[2])
  })
  
  dataInput <- reactive({
  first <- input$dates[1]
  second <- input$dates[2]
  days = seq(from = as.POSIXct(first, format='%Y-%m-%d')+days(1), to=as.POSIXct(second,format='%Y-%m-%d')+days(1),by='days')
  days <- as.character(days)
  month <- gsub('-','',gsub('-0','',substr(days,5,7)))
  day <- gsub('-','',gsub('-0','',substr(days,8,10)))
  days <- paste(month, day,substr(test,1,4),sep='/')
  days <- gsub('/0','/',days)
  days <- paste('petkey',days,sep=' ')
  sili2(days)
  })
 
  output$n <- renderText({
    nrow(dataInput())
  })
  
 output$styles <- renderTable({
   data <- dataInput()
   df <- data.frame(table(data$style))
   df[order(-df$Freq),]
 })
 output$location <- renderTable({
   dataInput()
#    data <- dataInput()
#    df <- data.frame(table(data$custstate))
#    df[order(-df$Freq),]
 })
 


})
  