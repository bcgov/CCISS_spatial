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
library(DT)
library(RColorBrewer)
library(colourvalues)
library(bcmaps)


source("mod_user_upload.R")
source("utils.R")

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


pool_dev <- dbPool(
  drv = RPostgres::Postgres(),
  #dbname = Sys.getenv("BCGOV_DB"),
  #host = Sys.getenv("BCGOV_HOST"),
  dbname = 'postgres',
  host = Sys.getenv("AWS_HOST"),
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

#TODO expand list of climate variables
climvars <- c('MAT','MCMT','TD','EMT')
climvars_label <- read.csv("data/Variables_ClimateBC.csv", stringsAsFactors = F)

spp <-c("Bl","Fd","Lw","Pl","Py","Sx")
spp <- dbGetQuery(pool_dev, "select * from species")%>%filter(species %in% spp)

#load color scheme for BCG prediction
bgc_colors <- read.csv('data/WNA_v12_HexCols.csv')
bgc_colors <- bgc_colors%>% filter(BGC!="(None)")

#load E1, S1 for feasibility
#load("data/E1.rda")
#load("data/S1.rda")


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

#load sf for districts
bc_districts <- nr_districts() %>%
                dplyr::select(ORG_UNIT, geometry)

#load model data
modelMetadata <- read.csv("data/ModelList.csv")
