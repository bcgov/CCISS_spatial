library(data.table)
library(sf)
library(dplyr)
library(dbplyr)
library(leaflet)
library(rpostgis)
library(RPostgres)
#library(profvis)
library(mapview)
library(mapdeck)
library(pool)

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



pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)


#retrieve grid id by bound box coordinates:
lon <- c(-131.28, -131.5)
lat <- c(52.40, 52.60)

poly_df <- data.frame(lon, lat)

poly <- st_as_sf(poly_df, coords = c("lon", "lat"), crs = 4326)%>%
        st_transform(3005)%>%
        st_bbox() %>% 
        st_as_sfc()%>%
        st_as_text()

poly <- "POLYGON ((949328.2 1039597, 985443.5 1039597, 985443.5 1070499, 949328.2 1070499, 949328.2 1039597))"
#look up hex_grid table from boundbox:
dat <- st_read(pool,
                          query = paste0("select siteno, district, geom
                                   from grid_dist 
                                   where st_intersects(geom, 'SRID=3005;",poly,"');"))

bcg_predictions <- dbGetCCISSRaw(pool,dat$siteno,"ACCESS-ESM1-5", "ssp126","2001")




dat <- dat %>%
  left_join(bcg_predictions[, c('siteno', 'bgc_pred')])%>%
  left_join(bgc_colors, by = c('bgc_pred' = 'BGC'))%>%
  st_transform(crs = 4326)


dat %>%
  leaflet() %>%
  #setView(lng = -131.28, lat = 52.40, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(
    fillColor = ~ Col,
    fillOpacity = 0.7,
    color = ~ Col
  )



#plot hexgrid by district: Haida Gwaii Natural Resource District
#check projection
dbGetQuery(pool, "SELECT ST_SRID(geom) from grid_dist limit 1")

hex_sf <- st_read(pool,
                  query = "select a.siteno, a.district, a.geom
                         from grid_dist a
                         where a.district = 'Haida Gwaii Natural Resource District' 
                         limit 500")

#join prediction results
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")[1,1]
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")[1,1]
periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")[2,1]

##eg to retrieve all projections for hex cell # 6:
bcg_predictions <- dbGetCCISSRaw(pool,hex_sf$siteno,gcmOpts,scenarioOpts,periodOpts)


#dissolve geometries to reduce number of polygons for plotting:
hex_map <- hex_sf %>%
           left_join(bcg_predictions[, c('siteno', 'bgc_pred')])

#hex_map <- aggregate(hex_map, by = list(hex_map$bgc_pred), FUN = mean)

# join prediction color schema
colors <- read.csv('WNA_v12_HexCols.csv')

hex_map <- hex_map %>%
           #left_join(colors, by = c('Group.1' = 'BGC'))
           left_join(colors, by = c('bgc_pred' = 'BGC'))


#apply correct projection
hex <- st_transform(hex_map, 4326)


#hex <- ms_simplify(hex)


system.time(

#overlap hex grid on leaflet
hex %>%
leaflet() %>%
  setView(lng = -131.28, lat = 52.40, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(
    fillColor = ~ Col,
    fillOpacity = 0.7,
    color = ~ Col
  )



)


leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addGlPolygons(data = st_cast(hex,"POLYGON"), 
                fillColor = "Col", 
                popup = "bgc_pred") %>%
  setView(lng = -126.5, lat = 54.5, zoom = 5)


system.time(
  
  #hex <- st_simplify(hex)
  #try mapview for plotting
  mapview(hex, zcol = "Group.1", col.regions = hex$Col ,legend = F, alpha.region = 0.7)
  
)

#use mapdeck (mapbox)
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")


system.time(
mapdeck(token = mapbox_token, style = mapdeck_style("light")) %>%
  add_polygon(
    data = hex
    , layer = "polygon_layer"
    , fill_colour = "Col"
  
  )
)



#use google javascript map API
library(googleway)

google_map(data = hex, key = Sys.getenv("googleAPI"))%>%
  add_polygons(fill_colour = "Col")

