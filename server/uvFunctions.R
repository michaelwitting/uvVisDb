#################################################################################################
#                                        functions                                              #
#################################################################################################

#load required libraries
library(features)

#function to serialize UV-Vis Spectrum for storage in DB
serializeUvSpectrum <- function(spectrum) {
  
  #initialize serial spectrum
  serialSpectrum <- ""
  
  #iterate trough spectrum
  for(i in 1:nrow(spectrum)) {
    if(!is.na(spectrum[i,2])) {
      clipboard <- paste(spectrum[i,1], spectrum[i,2], sep="_")
      print(clipboard)
      serialSpectrum <- paste(serialSpectrum, clipboard, sep="#")
    }
  }
  return(serialSpectrum)
}

#function to de-serialize UV-Vis Spectrum
deSerializeUvSpectrum <- function(serialSpectrum) {
  
  #parse input by spliting into lines
  spectrumList <- strsplit(serialSpectrum, split = "#")
  spectrumdf <- data.frame(matrix(unlist(spectrumList), byrow=T), stringsAsFactors = F)
  
  out <- strsplit(as.character(spectrumdf[,1]), split="_")
  
  spectrumdf <- data.frame(matrix(unlist(out), ncol=2, byrow=T), stringsAsFactors = F)
  spectrumdf$X1 <- as.numeric(spectrumdf$X1)
  spectrumdf$X2 <- as.numeric(spectrumdf$X2)
  colnames(spectrumdf)<-c("V1", "V2")

  return(spectrumdf)
}

#normalize spectrum
normalizeUvSpectrum <- function(spectrum) {
  
  #normalize spectrum to highest peak
  maxAbs <- max(spectrum$V2)
  spectrum$V2 <- spectrum$V2 / maxAbs *100
  
  #remove values below treshold
  spectrum$V2[spectrum$V2<0.1]<-0.1
  
  return(spectrum)
}


#get maxima
getMaxima <- function(spectrum) {
  
  test<-features(spectrum$V1, spectrum$V2)
  df<-data.frame(cbind(test$cpts, test$curvature))
  maxima<-data.frame(df$X1[which(df$X2<0)])
  colnames(maxima)<-c("X1")
  return(maxima)
  
}


#compare spectra
spectralSimilarity <- function(spectrum1, spectrum2, start=150, end=600, binwidth=5) {
  
  if(!is.na(spectrum1) && !is.na(spectrum2)) {
    #set bins for wavelength comparison
    binnedWavelength <- seq(start, end, by=binwidth)
    
    #bin spectrum 1
    binnedSpectrum1<-transform(spectrum1, group=cut(spectrum1$V1, breaks = binnedWavelength))
    binnedSpectrum1 <- aggregate(binnedSpectrum1["V2"], by=binnedSpectrum1["group"], FUN = sum)
    
    #bin spectrum 2
    binnedSpectrum2<-transform(spectrum2, group=cut(spectrum2$V1, breaks = binnedWavelength))
    binnedSpectrum2 <- aggregate(binnedSpectrum2["V2"], by=binnedSpectrum2["group"], FUN = sum)
    
    #combinde the two dataframe
    spectra <- merge(binnedSpectrum1, binnedSpectrum2, by=c("group"))
    
    similarity <- cor(spectra$V2.x, spectra$V2.y)
    
    #return result
    return(paste("Spectra Similiarty is:\n",similarity, sep=""))
  } else {
    return()
  }
  
  
}