library(data.table)
library(sf)
library(dplyr)
library(rpostgis)
library(RPostgres)
library(rasterVis)
library(raster)


con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

bc_raster <- pgGetRast(con, name = "bc_raster")

#reset values 
bc_raster <- raster::setValues(bc_raster,NA)


##make projected bgc maps
scn <- "ssp245"
futureperiod <- "2021-2040"
gcm <- "MIROC6" 
dat <- dbGetQuery(con,paste0("select rast_id, bgc_pred from pts2km_future 
                             where gcm = '",gcm,"' 
                             and scenario = '",scn,"' 
                             and futureperiod = '",futureperiod,"'"))

#convert to data table for faster merge
setDT(dat)
bgcs <- unique(dat$bgc_pred)
bgcID <- data.table(bgc = bgcs, id = 1:length(bgcs))
cols <- read.csv('WNA_v12_HexCols.csv')
dat[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
dat[bgcID,bgcID := i.id, on = c(bgc_pred = "bgc")]

#assign bgcID to cells in raster
bc_raster[dat$rast_id] <- dat$bgcID
bc_raster <- ratify(bc_raster)
crs(bc_raster) <- "EPSG:4326"

#rat <- as.data.table(levels(bc_raster)[[1]])
#rat[dat,`:=`(bgc = i.bgc_pred, col = i.Col), on = c(ID = "bgcID")]
bgcID[cols,Col := i.Col, on = c(bgc = "BGC")]


leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")%>%
  #addProviderTiles("OpenStreetMap.Mapnik", group= "OpenStreetMap")%>%
  #addProviderTiles("Esri.WorldStreetMap", group= "Esri")%>%
  addScaleBar(position = "bottomleft") %>%
  # addLayersControl(
  #   baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri"),
  #   options = layersControlOptions(collapsed = TRUE)
  # )%>%
  addRasterImage(bc_raster, colors = bgcID$Col, opacity = 0.8)
