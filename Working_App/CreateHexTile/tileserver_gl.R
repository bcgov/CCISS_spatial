# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.
# Here I'm leveraging bcgov existing packages to obtain map data.
library(sf)
library(analogsea)
library(bccciss)

Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
out_dir <- "./data-raw/BCMap"
shp_name <- "BCHex.shp"
layer <- "BCHex"
system("rm -R ./data-raw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dat <- st_read("~/BC_HexGrd400m.gpkg")
st_crs(dat) <- 3005
grd2 <- st_make_grid(dat,cellsize = 1000,square = F)
grd2 <- st_as_sf(data.frame(ID = 1:length(grd2)),geom = grd2)
st_write(grd2,"BC_BiggerHex.gpkg")

grd2 <- st_read("BC_BiggerHex.gpkg")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "jujL6cB3Wm9y", host = "138.197.168.220", 
                 port = 5432, dbname = "cciss")
tileserver <- droplets()[["VerbalColleague"]]
dists <- dbGetQuery(con,"select distinct dist_code from historic_sf")
dists <- dists$dist_code
dists <- dists[dists != "CAS"]
for(dist in dists){
  system("rm -R ./data-raw")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  dat <- dbGetQuery(con,paste0("select siteno from historic_sf where dist_code = '",dist,"' and period = 'Current91'"))
  dat <- as.data.table(dat)
  dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
  newIDs <- unique(dat$NewID)
  grdTemp <- grd2[grd2$ID %in% newIDs,]
  grdTemp <- st_transform(grdTemp,4326)
  st_write(grdTemp,dsn = out_dir,layer = dist, driver = "ESRI Shapefile")
  
  remote_shp_tiles_kd(tileserver,
                      paste0("/mapdata/",dist,".mbtiles -z13 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders"),
                      source_dir = out_dir, skip_upload = F)
}

for(dist in dists){
  temp <- paste0("'",dist,"': {'mbtiles' : '",dist,".mbtiles'}")
  dput(temp)
}


launch_tileserver_kd(tileserver, config = "./config/tileserver/config.json")

