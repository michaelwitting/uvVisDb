#load required libraries
library(shiny)
library(DT)

#include functions from other sources
source("server/dbFunctions.R", local = TRUE)
source("server/plots.R", local = TRUE)
source("server/uvFunctions.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  ############################################################################################################
  #get data values from text fields and convert to data frames
  spectrum <- reactive({
    if(input$spectrum == "") {
      return(NA)
    } else {
      #parse input by spliting into lines
      spectrumList <- strsplit(input$spectrum, split = "\n")
      spectrumdf <- data.frame(matrix(unlist(spectrumList), byrow=T), stringsAsFactors = F)
      out <- strsplit(as.character(spectrumdf[,1]), split=" ")
      spectrumdf <- data.frame(matrix(unlist(out), ncol=2, byrow=T), stringsAsFactors = F)
      spectrumdf$X1 <- as.numeric(spectrumdf$X1)
      spectrumdf$X2 <- as.numeric(spectrumdf$X2)
      colnames(spectrumdf)<-c("V1", "V2")
      spectrumdf <- normalizeUvSpectrum(spectrumdf)
      
      #return specturm
      spectrumdf
    }
  })
  
  ############################################################################################################
  #get data from lambda max search
  maxima <- reactive ({
    if(input$maxima == "") {
      return(NA)
    } else {
      lambdamaxList <- strsplit(input$maxima, split = "\n")
      lambdamaxdf <- data.frame(matrix(unlist(lambdamaxList), byrow=T), stringsAsFactors = F)
      colnames(lambdamaxdf)<-c("X1")
      lambdamaxdf$X1<-as.numeric(lambdamaxdf$X1)
      
      #return lambdamax values
      lambdamaxdf
    }
  })
  
  ############################################################################################################
  #get DB spectrum
  dbSpectrum <- reactive({
    i<-input$table_rows_selected
    if(!is.null(i)) {
      dbSpectrum <- searchSpectrum(my_data()$spectraId[i])
    } else {
      dbSpectrum <- NA
    }
    
    dbSpectrum
  })

  ############################################################################################################
  #function for ploting spectra
  spectrumPlot <- reactive({
        spectraPlot(spectrum(), dbSpectrum(), input$specSim[1],input$specSim[2])
  })
  
  ############################################################################################################
  #function for ploting spectra
  spectraSimilarity <- reactive({
    spectralSimilarity(spectrum(), dbSpectrum(), start = input$specSim[1], end= input$specSim[2])
  })
  
  ############################################################################################################
  #retrieve data from MySQL DB
  my_data <- reactive({
    if(input$button == 0) {
      return()
    }
    isolate({
      input$button
      if(!is.na(maxima()) && is.na(spectrum())) {
        searchLambdaMax(maxima())
      } else if(is.na(maxima()) && !is.na(spectrum())) {
        searchLambdaMax(getMaxima(spectrum()))
      } else if(!is.na(maxima()) && !is.na(spectrum())){
        searchLambdaMax(getMaxima(spectrum()))
      } else {
        return()
      }
    })
  })
  
  ############################################################################################################
  #data table 
  output$table <- DT::renderDataTable(my_data(), selection = "single")

  #create plot
  output$plot<-renderPlot({spectrumPlot()})
  
  #create textoutput
  output$similarity <- renderText({spectraSimilarity()})
})