#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  library(ggplot2)
  library(dplyr)
  read_dataSource <- function() {
    ventas <- read.csv("https://raw.githubusercontent.com/javis156/Bedu_R_code/master/VENTAS.csv")
    ventas$Fecha <- as.Date(ventas$Fecha ,format='%m/%d/%Y')
    return(ventas)
  }
  
  getOriginComposition <-function(){
    ventas <- read_dataSource()
    ventasPorOrigen <- filter(ventas,ventas$Marca=="General Motors")
    ventasPorOrigen <- filter(ventasPorOrigen,ventasPorOrigen$Segmento =="Compactos")
    ventasPorOrigen <- filter(ventasPorOrigen,ventasPorOrigen$Año==2020)
    ventasPorOrigen <- select(ventasPorOrigen,Marca,Origen,Cantidad)
    ventasPorOrigen <- aggregate(x = ventasPorOrigen$Cantidad,
                                 by= list(ventasPorOrigen$Marca,ventasPorOrigen$Origen),
                                 FUN=sum)
    names(ventasPorOrigen) <- c("Marca","Origen", "Cantidad")
    ventasTotales <- filter(ventas,ventas$Marca=="General Motors")
    ventasTotales <- filter(ventasTotales,ventasTotales$Segmento =="Compactos")
    ventasTotales <- filter(ventasTotales,ventasTotales$Año==2020)
    ventasTotales <- select(ventasTotales,Marca,Cantidad)
    ventasTotales <- aggregate(x = ventasTotales$Cantidad,
                               by= list(ventasTotales$Marca),
                               FUN=sum)
    names(ventasTotales) <- c("Marca", "CantidadTotal")
    ventasPorOrigen <- cbind(ventasPorOrigen,ventasTotales)
    ventasPorOrigen <- subset (ventasPorOrigen, select = -Marca)
    ventasPorOrigen <- subset (ventasPorOrigen, select = -Marca)
    ventasPorOrigen$Composicion <- (ventasPorOrigen$Cantidad/ventasPorOrigen$CantidadTotal)*100
    return(ventasPorOrigen)
  }
  
  output$plotYearRank <- renderPlot({
    ventas <- read_dataSource()
    ventasAnuales <- filter(ventas,ventas$Año== input$RankYear)
    
    ventasAnuales<-aggregate(x= ventasAnuales$Cantidad,
                                     by= list(ventasAnuales$Marca),
                                     FUN=sum)
    names(ventasAnuales) <- c("Marca", "Cantidad")
    
    ventasAnuales$Marca <- factor(ventasAnuales$Marca,levels = ventasAnuales$Marca[order(ventasAnuales$Cantidad, decreasing = FALSE)])
    
    ggplot(data=ventasAnuales, aes(x=Marca, y=Cantidad)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()+
      coord_flip()+
      labs(x='Marca', y='Ventas', title=paste('Ranking de Ventas'))
  })
  
  output$plotYearRankSegment <- renderPlot({
    ventas <- read_dataSource()
    ventasAnuales <- filter(ventas,ventas$Año== input$RankYear)
    ventasAnuales <- filter(ventas,ventas$Segmento == input$RankSegmento)
    
    VentasAnualesPorMarca<-aggregate(x= ventasAnuales$Cantidad,
                                     by= list(ventasAnuales$Marca),
                                     FUN=sum)
    names(VentasAnualesPorMarca) <- c("Marca", "Cantidad")
    
    VentasAnualesPorMarca$Marca <- factor(VentasAnualesPorMarca$Marca,levels = VentasAnualesPorMarca$Marca[order(VentasAnualesPorMarca$Cantidad, decreasing = FALSE)])
    
    ggplot(data=VentasAnualesPorMarca, aes(x=Marca, y=Cantidad)) +
      geom_bar(stat="identity", fill="steelblue")+
      #theme(axis.text.x = element_text(angle = 90))+
      theme_minimal()+
      coord_flip()+
      #facet_wrap("Segmento",ncol = 1,scales = "free")+
      labs(x='Marca', y='Ventas', title=paste('Ranking de Ventas por Segmento '))    
    
  })
  
  output$plotSpecificSegmentSales <- renderPlot({
    ventas <- read_dataSource()
    ventasPorSegmento <- filter(ventas,ventas$Marca== input$Marca)
    ventasPorSegmento <- filter(ventas,ventas$Segmento== input$Segmento)
    ventasPorSegmento <- filter(ventasPorSegmento,ventasPorSegmento$Año >= input$BeginYear)
    ventasPorSegmento <-select(ventasPorSegmento,Fecha,Segmento,Cantidad)
    ventasPorSegmento$Cantidad <-as.integer(ventasPorSegmento$Cantidad)
    ventasPorSegmento<-aggregate(x = ventasPorSegmento$Cantidad,
                                 by= list(ventasPorSegmento$Fecha, ventasPorSegmento$Segmento),
                                 FUN=sum)
    names(ventasPorSegmento) <- c("Fecha","Segmento", "Cantidad")
    ggplot(data=ventasPorSegmento, aes(x=Fecha, y=Cantidad, group=Segmento)) +
      geom_line(aes(color=Segmento))+
      geom_point(aes(color=Segmento))+
      geom_smooth(method='lm',color='grey',formula = y ~ x)+
      labs(x='Año', y='Ventas', title=paste('Comportamiento de Ventas '))+
      theme_minimal()+
      facet_wrap("Segmento",ncol = 1,scales = "free")   
  })
  
  output$plotSegmentSales <- renderPlot({
    setwd("/Users/javi/Documents/GitHub/BEDU_R_Final")
    getwd()
    ventas <- read.csv("VENTAS.csv")
    ventas$Fecha <- as.Date(ventas$Fecha ,format='%m/%d/%Y')
    #ventasPorSegmento <- filter(ventas,ventas$Marca=="General Motors")
    ventasPorSegmento <- filter(ventas,ventas$Marca== input$Marca)
    #ventasPorSegmento <- filter(ventas,ventas$Segmento== input$Segmento)
    ventasPorSegmento <- filter(ventasPorSegmento,ventasPorSegmento$Año >= input$BeginYear)
    ventasPorSegmento <-select(ventasPorSegmento,Fecha,Segmento,Cantidad)
    ventasPorSegmento$Cantidad <-as.integer(ventasPorSegmento$Cantidad)
    ventasPorSegmento<-aggregate(x = ventasPorSegmento$Cantidad,
                                 by= list(ventasPorSegmento$Fecha, ventasPorSegmento$Segmento),
                                 FUN=sum)
    names(ventasPorSegmento) <- c("Fecha","Segmento", "Cantidad")
    ggplot(data=ventasPorSegmento, aes(x=Fecha, y=Cantidad, group=Segmento)) +
      geom_line(aes(color=Segmento))+
      geom_point(aes(color=Segmento))+
      geom_smooth(method='lm',color='grey',formula = y ~ x)+
      labs(x='Año', y='Ventas', title=paste('Comportamiento de Ventas '))+
      theme_minimal()+
      facet_wrap("Segmento",ncol = 1,scales = "free")   
  })
  
  output$plotSalesPerYearBrand <- renderPlot({
    ventas <- read_dataSource()
    ventasGM <- filter(ventas,ventas$Marca==input$Marca)
    ventasGM <- filter(ventasGM,ventasGM$Año>= input$BeginYear)
    ventasGM<-select(ventasGM,Fecha,Marca,Cantidad)
    ventasGM$Cantidad <-as.integer(ventasGM$Cantidad)
    str(ventasGM)
    ventasGM<-aggregate(x = ventasGM$Cantidad,
                        by= list(ventasGM$Fecha, ventasGM$Marca),
                        FUN=sum)
    names(ventasGM) <- c("Fecha","Marca", "Cantidad")
    
    
    ggplot(data=ventasGM, aes(x=Fecha, y=Cantidad, group=Marca)) +
      geom_line(aes(color=Marca))+
      geom_point(aes(color=Marca))+
      geom_smooth(method='lm',color='grey',formula = y ~ x)+
      labs(x='Año', y='Ventas', title=paste('Comportamiento de Ventas '))+
      theme_minimal()  
  })
  
  output$plotModelSalesPerYearBrand <- renderPlot({
    ventas <- read_dataSource()
    ventasPorModelo <- filter(ventas,ventas$Marca==input$Marca)
    ventasPorModelo <- filter(ventasPorModelo,ventasPorModelo$Segmento ==input$Segmento)
    ventasPorModelo <- filter(ventasPorModelo,ventasPorModelo$Año==input$Year)
    ventasPorModelo <- select(ventasPorModelo,Fecha,Modelo,Cantidad)
    ventasPorModelo <- aggregate(x = ventasPorModelo$Cantidad,
                                 by= list(ventasPorModelo$Modelo),
                                 FUN=sum)
    names(ventasPorModelo) <- c("Modelo", "Cantidad")
    
    ventasPorModelo$Modelo <- factor(ventasPorModelo$Modelo,levels = ventasPorModelo$Modelo[order(ventasPorModelo$Cantidad, decreasing = FALSE)])
    
    ggplot(data=ventasPorModelo, aes(x=reorder(Modelo,Modelo,(function(x) length(x))), y=Cantidad)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()+
      coord_flip()+
      labs(x='Modelo', y='Ventas')
  })
  
  output$table <- renderTable({ 
    data.frame(getOriginComposition())
  })

  
})
