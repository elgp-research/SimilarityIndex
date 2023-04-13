## ---- libraries
rm(list = ls())
library(RSQLite)
library(DBI)

## ---- sourcing R file
source("1.data_clean.R")

## ---- connecting to SQL database
con <- dbConnect(RSQLite::SQLite(), "similarity_index.db")

dbWriteTable(con, "similarity_index_table", master_dta, overwrite = TRUE)

## ---- SQL database queries
dbGetQuery(con, "SELECT * FROM similarity_index_table")

## ---- disconnecting from SQL servers
dbDisconnect(con)
