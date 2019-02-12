library(tidyverse)
library(shiny)
library(shinydashboard)
library (DT)
library(plotly)
library(reshape2)
library(data.table)
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(scales)
library(gridExtra)
library(forecast)
library(TTR)
library(xts)
library(dygraphs)
library(datasets)
library(rpivotTable)
library(googlesheets)
####################################################################

dati<-gs_title("mortioc")
dati <-gs_read(dati, ws="dati")



dati$datap<-mdy(dati$datap)

dati$data<-as.Date(dati$datap)
dati<- mutate(dati, mese=paste(year(datap),'-',month(datap),sep=''))
dati$mese<-as.Date((paste(dati$mese,"-01",sep="")))


#dati$anno<-as.Date((paste(dati$anno,"-01","-01",sep="")))
dati$codaz<-casefold(dati$codaz, upper=TRUE)

dati<-dati %>% 
  group_by(specie,mese) %>% 
  summarise("morti"=sum(ncamp))


#output$dygraph <- renderDygraph({
graph<- xts(dati$morti, order.by = as.Date(dati$mese))
  dygraph(graph,ylab = "n.morti") %>% 
    dyAxis("y", label = "n.morti/mese",valueRange = c(0, 100)) %>% 
    dySeries("V1", label = "n.morti/mese") %>% 
    dyOptions(drawPoints = TRUE, pointSize = 2) 

  p <- predict(dati, n.ahead = 72, prediction.interval = TRUE)
  
  dygraph(p, main = "Predicted Lung Deaths (UK)") %>%
    dySeries(c("lwr", "fit", "upr"), label = "Deaths")

  
  library(dygraphs)
  lungDeaths <- cbind(mdeaths, fdeaths)
  dygraph(lungDeaths)
  
  

 