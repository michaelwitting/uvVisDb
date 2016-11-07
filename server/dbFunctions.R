#load required libraries
library(reshape2)
library(RSQLite)
 
#read required additional scripts
source("server/uvFunctions.R", local = TRUE)

#DB string
DBLOCATION <- "db/uvvisdb.db"
 
#################################################################################################
#                                     search functions                                          #
#################################################################################################
#search for specific lambda max values
searchLambdaMax <- function(lambdamaxdf, searchRange=5, ...) {

  #load required MySQL connection. Outside of shiny server connection is only loaded once
  con <- dbConnect(RSQLite::SQLite(), DBLOCATION)
  
  #initialize data frame
  df<-data.frame()

  #loop over differnt lamda max values
  for(i in 1:nrow(lambdamaxdf)) {

    print(lambdamaxdf$X1[i])

    #calculate lower and upper search range
    lower <- lambdamaxdf$X1[i] - searchRange/2
    upper <- lambdamaxdf$X1[i] + searchRange/2

    #create query string
    sql <- sprintf("SELECT * FROM lambdamaxtable WHERE wavelength BETWEEN '%s' AND '%s'", lower, upper)

    #execute query
    clipboard <- data.frame(fetch(dbSendQuery(con, sql)))
    df<-rbind.data.frame(df, clipboard)
  }

  #aggregate table
  df2 <- aggregate(df["wavelength"], by = df["spectraId"], FUN = length)
  df <- aggregate(df["wavelength"], by = df["spectraId"], FUN = paste, collapse = " | ")
  colnames(df2) <- c("id", "count")
  df <- cbind.data.frame(df, df2$count)

  #initialize dataframe for counts
  counts <- data.frame()
  names <- data.frame()

  #get spectra counts
  for(i in 1:nrow(df)) {

    #get occurence of spectra id for lambdamax
    sql <- sprintf("SELECT spectraId,COUNT(*) as count FROM lambdamaxtable WHERE spectraId='%s' GROUP BY spectraId ORDER BY count DESC;", df$spectraId[i])
    clipboard <- data.frame(fetch(dbSendQuery(con, sql)))
    counts <- rbind.data.frame(counts, clipboard)

    #get name of molecule
    sql <- sprintf("SELECT idspectra,name FROM spectra WHERE idspectra='%s';", df$spectraId[i])
    clipboard <- data.frame(fetch(dbSendQuery(con, sql)))

    #get metadata for spectrum
    sql <- sprintf("SELECT smiles,solvent,instrument FROM metadata WHERE spectraid='%s';", df$spectraId[i])
    clipboard2 <- data.frame(fetch(dbSendQuery(con, sql)))

    #bind data frames
    names <- rbind.data.frame(names, cbind.data.frame(clipboard, clipboard2))
    #colnames(names) <- c("spectraId", "fittingLamdaMax", "NoOfFitting", "NoOfDB", "Name", "SMILES", "Solvent", "Instrument")

  }

  #combinde the two dataframe
  df <- merge(df, counts, by=c("spectraId"))
  df <- merge(df, names, by.x = c("spectraId"), by.y=c("idspectra"))

  #close DB connection
  dbDisconnect(con)

  #return data frame with query results
  return(df)
}

#search spectrum by id
searchSpectrum <- function(id) {

  #load required MySQL connection. Outside of shiny server connection is only loaded once
  con <- dbConnect(RSQLite::SQLite(), DBLOCATION)
  
  sql <- sprintf("SELECT spectrum FROM spectra WHERE idspectra = '%s';", id)
  rs <- data.frame(fetch(dbSendQuery(con, sql)), header=T)
  spectrum <- deSerializeUvSpectrum(rs[1,1])
  print(spectrum)

  #close DB connection
  dbDisconnect(con)

  colnames(spectrum)<-c("V1", "V2")

  return(spectrum)

}
