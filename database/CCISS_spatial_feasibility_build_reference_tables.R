library(data.table)
library(RPostgres)
library(dplyr)
library(pool)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

## PURGE AND LOAD SPECIES TABLE
sql <- "DELETE FROM species"
RPostgres::dbExecute(pool, sql)
uniquespecies <- sort(unique(S1$Spp))
species <- data.frame(species_id = as.integer(c(1:length(uniquespecies))),
                      species = uniquespecies)
RPostgres::dbAppendTable(pool, "species", species)


## PURGE AND LOAD EDATOPE TABLE
sql <- "DELETE FROM edatope"
RPostgres::dbExecute(pool, sql)
uniqueedatopes <- sort(unique(E1$Edatopic))
uniqueedatopes <- data.frame(edatope_id = as.integer(c(1:length(uniqueedatopes))),
                             edatope = uniqueedatopes)
RPostgres::dbAppendTable(pool, "edatope", uniqueedatopes)