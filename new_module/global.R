#load packages and connect to database

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras2)
#library(leaflet.opacity)
library(sf)
library(dplyr)
library(dbplyr)
library(rpostgis)
library(DBI)
library(RPostgres)
library(pool)
library(plotly)
library(ggplot2)
library(shinyjs)
library(raster)
library(data.table)



#database connection
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  #dbname = 'postgres',
  #host = Sys.getenv("AWS_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

#database for climate variables
pool2 <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "bgc_climate_data",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

#load options for selectInput(s)
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")[,1]
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")[,1]
#periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")[,1]
periodOpts <- c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")
districts <- dbGetQuery(pool, "select distinct district, dist_code from grid_dist")[,2]
climvars <- dbGetQuery(pool2, "select distinct climvar from szsum_fut")[,1]


#load color scheme for BCG prediction
bgc_colors <- read.csv('WNA_v12_HexCols.csv')
subzones <- bgc_colors[bgc_colors$BGC != "(None)",]
load('Dist_MapBoundaries.Rdata')

#load BC raster layer
bc_raster <- pgGetRast(dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
), name = "bc_raster")

bc_raster <- raster::setValues(bc_raster,NA)



#load util functions:
dbGetCCISSRaw <- function(con, siteno, gcm, scenario, period){
  cciss_sql <- paste0("
    SELECT cciss_future12_array.siteno,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc_attribution.bgc,
         bgc.bgc bgc_pred
  FROM cciss_future12_array
  JOIN bgc_attribution
    ON (cciss_future12_array.siteno = bgc_attribution.siteno),
       unnest(bgc_pred_id) WITH ordinality as source(bgc_pred_id, row_idx)
  JOIN (SELECT ROW_NUMBER() OVER(ORDER BY gcm_id, scenario_id, futureperiod_id) row_idx,
               gcm,
               scenario,
               futureperiod
        FROM gcm 
        CROSS JOIN scenario
        CROSS JOIN futureperiod) labels
    ON labels.row_idx = source.row_idx
  JOIN bgc
    ON bgc.bgc_id = source.bgc_pred_id
  WHERE cciss_future12_array.siteno IN (", paste(unique(siteno), collapse = ","), ")
  AND futureperiod IN ('",paste(unique(period), collapse = "','"), "')
  AND scenario IN ('",paste(unique(scenario), collapse = "','"), "')
  AND gcm IN ('",paste(unique(gcm), collapse = "','"), "')
  ")
  
  dat <- setDT(RPostgres::dbGetQuery(con, cciss_sql))
  dat <- unique(dat)
  return(dat)
}


dbGetbgc <- function(con, period, scn, gcm, dist){
  
  sql_query <- paste0("select bgc_pred, geom from aggregated_bcg
          where futureperiod = '",period, "'
          and   scenario = '", scn , "'
          and   gcm = '", gcm, "'
          and dist_code IN ('", paste(unique(dist), collapse = "','"), "')
          ")
  
  st_read(con, query = sql_query)
}

dbGetbgc_raster <- function(con, period, scn, gcm){
  
  sql_query <- paste0("select rast_id, bgc_pred from pts2km_future
          where futureperiod = '",period, "'
          and   scenario = '", scn , "'
          and   gcm = '", gcm, "'
          ")
  dat <- dbGetQuery(con, sql_query)
  
  setDT(dat)
  
  dat
}

