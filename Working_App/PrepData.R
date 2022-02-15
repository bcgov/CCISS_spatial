library(data.table)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "jujL6cB3Wm9y", host = "138.197.168.220", 
                 port = 5432, dbname = "cciss")

dat <- dbGetQuery(con,"select siteno,bgc_pred from historic_sf where dist_code = 'DPG' and period = 'Current91'")
dat <- dbGetQuery(con,"select period,siteno,bgc,bgc_pred from historic_sf where dist_code = 'DPG'")
dat <- as.data.table(dat)

dat <- as.data.table(dat)
cw <- fread("./inputs/BigGrid_Crosswalk.csv")
dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
BULid <- data.table(ID = unique(dat$NewID),Type = "Base")
fwrite(BULid,"./inputs/DPG_AllID.csv")

dat1 <- dat[,.N,by = .(NewID,period,bgc)][order(-N), .SD[1], by = .(NewID,period)]
dat2 <- dat[,.N,by = .(NewID,period,bgc_pred)][order(-N), .SD[1], by = .(NewID,period)]
dat2[dat1,bgc := i.bgc, on = "NewID"]
#dat2 <- dat[,.N,by = .(NewID,period,bgc_pred)][order(-N), .SD[1], by = .(NewID,period)]
dat2[,N := NULL]     

ss <- fread("./CreateSSDat/SiteSeries_Use_CCISSpaper.csv")
dat3 <- ss[dat2, on = c(MergedBGC = "bgc_pred"),allow.cartesian = T]
dat3 <- dat3[,.(NewID,MergedBGC,SS_NoSpace,period,Edatopic)]
setnames(dat3,old = "SS_NoSpace", new = "SSPred")
dat4 <- ss[dat2, on = c(MergedBGC = "bgc"),allow.cartesian = T]
dat4 <- dat4[,.(NewID,MergedBGC,SS_NoSpace,period,Edatopic)]
dat3[dat4,SS_NoSpace := i.SS_NoSpace, on = c("period","NewID","Edatopic")]

setnames(dat3,c("SiteNo","BGC.pred","SSPred","Period","Edatopic","SS_NoSpace"))
fwrite(dat3,"./inputs/DPG_Historic_SS.csv")
# cols <- fread("WNA_v12_HexCols.csv")
# dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
# dat2[,bgc_pred := NULL]
# fwrite(dat2,"TestColScheme.csv")
##future
dat <- dbGetQuery(con,"select siteno,gcm,scenario,futureperiod,bgc,bgc_pred from future_sf where dist_code = 'DPG'")

dat <- as.data.table(dat)
dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
dat1 <- dat[,.N,by = .(NewID,scenario,gcm,futureperiod,bgc)][order(-N), .SD[1], by = .(NewID,scenario,gcm,futureperiod)]
dat2 <- dat[,.N,by = .(NewID,scenario,gcm,futureperiod,bgc_pred)][order(-N), .SD[1], by = .(NewID,scenario,gcm,futureperiod)]
dat2[dat1,bgc := i.bgc, on = "NewID"]
dat2[,N := NULL]
fwrite(dat2,"./inputs/DPG_HexTest.csv")

dat <- st_transform(dat,4326)
for(i in 1:24){
  dat$bb[i] <- list(st_bbox(dat$geom[i]))
}
dist_bbox <- st_drop_geometry(dat[,c("ORG_UNIT","bb")])
save(dist_bbox,file = "Dist_MapBoundaries.Rdata")
