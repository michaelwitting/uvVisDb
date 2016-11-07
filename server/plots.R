#load required libraries
library(ggplot2)

#load required scripts
source("server/uvFunctions.R", local = TRUE)

#separate output and generation of plot
spectraPlot<-function(spectrumdf, dbSpectrum, min, max){
  
  if(!is.na(spectrumdf) && is.na(dbSpectrum)) {

    #get lambdamax values
    lambdaMaxSpectrum <- getMaxima(spectrumdf)
    
    #plot only query spectrum
    ggplot()+
      geom_line(data = spectrumdf, aes(x=V1, y=V2), colour="green") +
      geom_vline(xintercept = lambdaMaxSpectrum$X1, colour = "green") +
      geom_label(data = lambdaMaxSpectrum, aes(x=lambdaMaxSpectrum$X1, y=110, label=as.character(round(lambdaMaxSpectrum$X1))), fill="green")+
      scale_y_continuous(limits = c(0,125))+
      scale_x_continuous(limits = c(100, 1000), expand=c(0,0))+
      annotate("rect", xmin=100, xmax=min, ymin=0, ymax=125, colour="grey", alpha=.2)+
      annotate("rect", xmin=max, xmax=1000, ymin=0, ymax=125, colour="grey", alpha=.2)+
      theme_bw()+xlab("Wavelength (nm)")+
      theme(panel.border = element_blank(),
            axis.line=element_line(colour="black"),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_text("Wavelength (nm)"))

  } else if(!is.na(spectrumdf) && !is.na(dbSpectrum)) {
    
    #get lambdamax values
    lambdaMaxSpectrum <- getMaxima(spectrumdf)
    lambdaMaxDbSpectrum <- getMaxima(dbSpectrum)
    
    #plot both spectrum
    ggplot()+
      geom_line(data = spectrumdf, aes(x=V1, y=V2), colour="green")+
      geom_vline(xintercept = lambdaMaxSpectrum$X1, colour = "green")+
      geom_label(data = lambdaMaxSpectrum, aes(x=lambdaMaxSpectrum$X1, y=110, label=as.character(round(lambdaMaxSpectrum$X1))), fill="green")+
      geom_line(data = dbSpectrum,aes(x=V1, y=V2), colour="red")+
      geom_vline(xintercept = lambdaMaxDbSpectrum$X1, colour = "red")+
      geom_label(data = lambdaMaxDbSpectrum, aes(x=lambdaMaxDbSpectrum$X1, y=120, label=as.character(round(lambdaMaxDbSpectrum$X1))), fill="red")+
      scale_y_continuous(limits = c(0,125))+
      scale_x_continuous(limits = c(100, 1000), expand=c(0,0))+
      annotate("rect", xmin=100, xmax=min, ymin=0, ymax=125, colour="grey", alpha=.2)+
      annotate("rect", xmin=max, xmax=1000, ymin=0, ymax=125, colour="grey", alpha=.2)+
      theme_bw()+xlab("Wavelength (nm)")+
      theme(panel.border = element_blank(),
            axis.line=element_line(colour="black"),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_text("Wavelength (nm)"))
    
  } else if(is.na(spectrumdf) && !is.na(dbSpectrum)) {
    
    #get lambdamax values
    lambdaMaxDbSpectrum <- getMaxima(dbSpectrum)
    
    #plot only DB spectrum
    ggplot()+
      geom_line(data = dbSpectrum,aes(x=V1, y=V2), colour="red")+
      geom_line(data = dbSpectrum,aes(x=V1, y=V2), colour="red")+
      geom_vline(xintercept = lambdaMaxDbSpectrum$X1, colour = "red")+
      geom_label(data = lambdaMaxDbSpectrum, aes(x=lambdaMaxDbSpectrum$X1, y=120, label=as.character(round(lambdaMaxDbSpectrum$X1))), fill="red")+
      scale_y_continuous(limits = c(0,125))+
      scale_x_continuous(limits = c(100, 1000), expand=c(0,0))+
      annotate("rect", xmin=100, xmax=min, ymin=0, ymax=125, colour="grey", alpha=.2)+
      annotate("rect", xmin=max, xmax=1000, ymin=0, ymax=125, colour="grey", alpha=.2)+
      theme_bw()+xlab("Wavelength (nm)")+
      theme(panel.border = element_blank(),
            axis.line=element_line(colour="black"),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank())
    
  } else {
    return()
  }
}