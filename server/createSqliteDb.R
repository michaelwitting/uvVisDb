#load required libraries
library(RSQLite)

#################################################################
# IMPORTANT:                                                    #
# This script is only needed to create a new SQLite Template    #
# for the used database                                         #
#################################################################

#connect to Sqlite DB
con <- dbConnect(RSQLite::SQLite(), "db/uvvisdb.db")

#only for fresh creation
#create tables
dbSendQuery(con, 'CREATE TABLE lambdamaxtable(
                    idlambdaMaxTable INTEGER PRIMARY KEY AUTOINCREMENT,
                    spectraId INT NOT NULL,
                    wavelength REAL NOT NULL
                  );')
dbSendQuery(con, 'CREATE TABLE metadata(
                    idmetadata INTEGER PRIMARY KEY AUTOINCREMENT,
                    spectraid INT NOT NULL,
                    smiles TEXT,
                    solvent TEXT,
                    instrument TEXT,
                    date NUMERIC,
                    submission TEXT
                  );')
dbSendQuery(con, 'CREATE TABLE spectra(
                    idspectra INTEGER PRIMARY KEY AUTOINCREMENT,
                    name TEXT,
                    spectrum TEXT
                  );')
#list created databases
dbListTables(con)

#close connection
dbDisconnect(con)
