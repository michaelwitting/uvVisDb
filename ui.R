#load required libraries
library(shiny)
library(DT)
library(plotly)

#include functions from other sources
source("ui/gui.R", local = TRUE)

#create Shiny UI
shinyUI(fluidPage(
  
  #main panel with submit button
  #Application title
  headerPanel(
    list(HTML('<img src="hmgu.png" height="20"/>'), HTML('<img src="tum.png" height="20"/>'), "UV-VIS-DB")
  ),
  
  fluidRow(
    
    #side bar with inputs
    column(2,
           textareaInput("spectrum", "Enter Spectrum:", rows=15),

           textareaInput("maxima", "Enter lambda max", rows = 5),

           actionButton("button", "Go!")
    ),
    
    #main panel with results
    column(10,
           #plot spectra
           plotOutput("plot"),
           
           #slider input for spec similarity
           sliderInput("specSim", "Range for comparison:",
                       min = 100, max = 1000, value= c(150,500), step=5, width = '100%'),
           
           #plot spectra
           verbatimTextOutput("similarity"),
           
           #create table with data
           dataTableOutput("table")
    )
  ),
  fluid=T
))