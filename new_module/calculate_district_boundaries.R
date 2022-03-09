library(bcmaps)

districts <- nr_districts("sf")

districts <- st_transform(districts,4326)

for(i in 1:23){
  districts$bb[i] <- list(st_bbox(districts$geometry[i]))
}


dist_bbox <- st_drop_geometry(districts[,c("ORG_UNIT","bb")])
save(dist_bbox,file = "Dist_MapBoundaries.Rdata")