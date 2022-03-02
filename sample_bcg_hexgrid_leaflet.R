library(data.table)
library(sf)
library(dplyr)
library(dbplyr)
library(leaflet)
library(rpostgis)
library(profvis)

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

profvis({

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

#plot hexgrid by district: Cassiar Subdistrict

ids <- dbGetQuery(pool, "select siteno, district from grid_dist
                         where district = 'Cassiar Subdistrict'")

hex_sf <- st_read(pool, query = paste0("select * from hex_grid where siteno IN (", paste(ids$siteno[1:1000], collapse = ' , '),")"))


#join prediction results
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")[1,1]
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")[1,1]
periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")[1,1]

##eg to retrieve all projections for hex cell # 6:
bcg_predictions <- dbGetCCISSRaw(pool,ids$siteno[1:1000],gcmOpts,scenarioOpts,periodOpts)

hex_map <- merge(hex_sf, bcg_predictions[, c('siteno', 'bgc_pred')])
  
# join prediction color schema
colors <- read.csv('WNA_v12_HexCols.csv')
hex_map <- hex_map %>%
           left_join(colors, by = c('bgc_pred' = 'BGC'))


#apply correct projection
hex <- st_transform(hex_map, 4326)
#plot(hex)

#overlap hex grid on leaflet
hex %>%
leaflet() %>%
  setView(lng = -126.5, lat = 54.5, zoom = 5) %>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(
    fillColor = ~ Col,
    fillOpacity = 0.7,
    color = ~ Col
  )


})