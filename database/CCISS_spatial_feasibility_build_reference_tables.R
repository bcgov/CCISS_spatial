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

##bybec db connection
sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

## LOAD TABLES FROM DATABASE
#####load feas table from database
S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

## PURGE AND LOAD SPECIES TABLE
sql <- "DELETE FROM species"
RPostgres::dbExecute(pool, sql)
uniquespecies <- sort(unique(S1$Spp))
species <- data.frame(species_id = as.integer(c(1:length(uniquespecies))),
                      species = uniquespecies)
RPostgres::dbAppendTable(pool, "species", species)


## PURGE AND LOAD EDATOPE TABLE
load("new_module/data/E1.rda")
sql <- "DELETE FROM edatope"
RPostgres::dbExecute(pool, sql)
uniqueedatopes <- sort(unique(E1$Edatopic))
uniqueedatopes <- data.frame(edatope_id = as.integer(c(1:length(uniqueedatopes))),
                             edatope = uniqueedatopes)
RPostgres::dbAppendTable(pool, "edatope", uniqueedatopes)
