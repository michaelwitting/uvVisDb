#move files to specific folder
uploadFolder <- "other/TestData"

#DB string
DBLOCATION <- "db/uvvisdb.db"

#read uploadList.txt
#format for uploadlist.txt
#file \t name \t smiles \t solvent \t instrument
uploadList <- list.files(uploadFolder, pattern = ".txt", full.names = T)
metaData <- data.frame(read.table(uploadList, header=T, sep="\t"))

#get all .xy files
spectraList <- list.files(uploadFolder, pattern = ".xy", full.names = F)

#iterate over all .xy files
for(i in 1:length(spectraList)) {
  
  #file path
  filePath <- paste(uploadFolder, spectraList[i], sep="/")
  
  #read .xy file
  spectrum <- data.frame(read.table(filePath))
  
  #Filename, compound name, smiles, solvent, instrument
  name <- metaData$name[which(metaData$file == spectraList[i])]
  smiles <- metaData$smiles[which(metaData$file == spectraList[i])]
  solvent <- metaData$solvent[which(metaData$file == spectraList[i])]
  instrument <- metaData$instrument[which(metaData$file == spectraList[i])]

  #upload spectrum
  uploadSpectrum(spectrum, name, smiles, solvent, instrument)

}

#################################################################################################
#                                     upload functions                                          #
#################################################################################################
#private functions used for upload
#load required libraries
library(reshape2)
library(RSQLite)

#load functions for different file
source("server/uvFunctions.R", local = TRUE)

#function for uploading a single spectrum
uploadSpectrum <- function(spectrum, name, smiles, solvent, instrument) {
  
  #load required MySQL connection. Outside of shiny server connection is only loaded once
  con <- dbConnect(RSQLite::SQLite(), DBLOCATION)
  
  #normalize spectrum
  spectrum <- normalizeUvSpectrum(spectrum)
  
  #get lambda max values
  lambdamaxdf <- getMaxima(spectrum)
  
  #serialize spectrum
  serialSpectrum <- serializeUvSpectrum(spectrum)
  
  #upload spectrum
  sql <- sprintf("INSERT INTO spectra(name, spectrum) VALUES('%s', '%s');", name, serialSpectrum)
  dbSendQuery(con, sql)
  
  #get id of new uploaded spectrum for lambdamax table
  id <- dbFetch(dbSendQuery(con, "select last_insert_rowid();"))$`last_insert_rowid()`
  
  #upload to lambdmax table
  for(i in 1:nrow(lambdamaxdf)) {
    sql <- sprintf("INSERT INTO lambdamaxtable(spectraId, wavelength) VALUES('%s', '%s');", id, lambdamaxdf$X1[i])
    dbSendQuery(con, sql)
  }
  
  #upload meta data
  sql <- sprintf("INSERT INTO metadata (spectraid, smiles, solvent, instrument, date, submission) VALUES ('%s', '%s', '%s', '%s', '%s', '%s');", id, smiles, solvent, instrument, Sys.Date(), "Michael Witting")
  dbSendQuery(con, sql)
  
  #close connection
  dbDisconnect(con)
}

