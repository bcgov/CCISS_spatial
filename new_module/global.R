#load packages and connect to database

library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(dbplyr)
library(leaflet)
library(rpostgis)
library(DBI)
library(RPostgres)
library(pool)
library(plotly)
library(ggplot2)
library(shinyjs)



#database connection
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

#load options for selectInput(s)
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")
periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")
districts <- dbGetQuery(pool, "select distinct district, dist_code from grid_dist")


#load color scheme for BCG prediction
bcg_colors <- read.csv('WNA_v12_HexCols.csv')



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
