#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


ventas <- read.csv("https://raw.githubusercontent.com/javis156/Bedu_R_code/master/VENTAS.csv")
ventas$Fecha <- as.Date(ventas$Fecha ,format='%m/%d/%Y')


# Define UI for application that draws a histogram
shinyUI
(
  fluidPage
  (
    titlePanel("Análisis de ventas de autos nuevos en México"),
    tabsetPanel
    (
      tabPanel
      (
        "Ranking Anual por Marca",
        fluidRow
        (
          column
          (
            6,
            wellPanel(
              selectInput(inputId = "RankYear", label = strong("Año"),
                          choices = unique(ventas$Año))
            )       
          ),
          column
          (
            6,
            wellPanel(
              selectInput(inputId = "RankSegmento", label = strong("Segmento"),
                          choices = unique(ventas$Segmento))
            )       
          )
          
        ),
        fluidRow
        (
          
          column
          (6,
            plotOutput("plotYearRank")
          ),
          column
          (6,
            plotOutput("plotYearRankSegment")
          )
        )
        
      ),
      
      
      tabPanel
      (
        "Comportamiento de ventas",
        fluidRow
        (
          column
          (
            6,
            wellPanel
            (
              selectInput(inputId = "BeginYear", label = strong("Desde"),
                          choices = unique(ventas$Año))
            ) 
          ),
          column
          (
            6,
            wellPanel
            (
              selectInput(inputId = "Marca", label = strong("Marca"),
                          choices = unique(ventas$Marca))
            ) 
          )
        ),
        fluidRow
        (
          column
          (
            12,
            plotOutput("plotSalesPerYearBrand")
          )
        ),
        fluidRow
        (
          column
          (
            12,
            plotOutput("plotSegmentSales")
          )
        ),
        fluidRow
        (
          column
          (
            4,
            wellPanel(
              selectInput(inputId = "Year", label = strong("Año"),
                          choices = unique(ventas$Año))
            )       
          ),
          column
          (
            4,
            wellPanel(
              selectInput(inputId = "Marca", label = strong("Marca"),
                          choices = unique(ventas$Marca))
            )       
          ),
           column
           (
             4,
             wellPanel
             (
               selectInput(inputId = "Segmento", label = strong("Segmento"),
                           choices = unique(ventas$Segmento))
             ) 
           )
          
        ),
        fluidRow
        (
          column
          (
            6,
            wellPanel
            (
              tableOutput("table")
            )   
          ),
          column
          (
            6,
            plotOutput("plotModelSalesPerYearBrand")
          )
        )
        
      )
        
    )
  )
)
