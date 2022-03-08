#data preprocessing ETL for BGC prediction

library(data.table)
library(sf)
library(dplyr)
library(dbplyr)
library(rpostgis)
library(RPostgres)
library(pool)
library(DBI)


aggregate_geom_bcg_pred <- function(pool, dist_code, period, scenario, gcm){
  
#use st_join in PostGIS 2x faster that st_join from sf package

cciss_sql <- paste0("
 WITH 
 main as (
  SELECT cciss_future12_array.siteno,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc_attribution.bgc,
         bgc.bgc bgc_pred,
         grid_dist.dist_code,
         grid_dist.geom
  FROM cciss_future12_array
  JOIN grid_dist 
    ON grid_dist.siteno = cciss_future12_array.siteno
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
  WHERE grid_dist.dist_code IN ('", paste(unique(dist_code), collapse = "','"), "')
  AND futureperiod IN ('",paste(unique(period), collapse = "','"), "')
  AND scenario IN ('",paste(unique(scenario), collapse = "','"), "')
  AND gcm IN ('",paste(unique(gcm), collapse = "','"), "')
 )
 
 SELECT a.futureperiod, 
        a.gcm, 
        a.scenario, 
        a.dist_code, 
        a.bgc_pred,
        ST_UNION(a.geom) as geom
 FROM main a 
 GROUP BY a.futureperiod, a.gcm, a.scenario, a.dist_code, a.bgc_pred;
  
  ")


dat <- st_read(pool, query = cciss_sql)
dat

}


# dat <- aggregate_geom_bcg_pred(pool, 
#                                dist_code = "DQC", 
#                                period = "2001", 
#                                scenario = "ssp126", 
#                                gcm = "CanESM5")

dists <- dbGetQuery(pool, "select * from dist_codes")[,1]
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")[,1]
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")[,1]
periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")[,1]



#--- parallel loop through combinations ------
library(doFuture)
n.cores <- parallel::detectCores() - 1


#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)


Sys.time()


results <- foreach(i = 1,
                   #.combine = rbind,
                   .packages = c('sf','rpostgis','RPostgres','DBI','pool')) %dopar% {
                     
    pool <- dbPool(
                       drv = RPostgres::Postgres(),
                       dbname = "postgres",
                       host = Sys.getenv("AWS_HOST"),
                       port = 5432, 
                       user = Sys.getenv("BCGOV_USR"),
                       password = Sys.getenv("BCGOV_PWD")
                     )
    
  dat <- aggregate_geom_bcg_pred(pool, 
                                 dist_code = dists[i], 
                                 period = periodOpts, 
                                 scenario = scenarioOpts, 
                                 gcm = gcmOpts)
  
  #write results (in case database connection fail)
  saveRDS(dat, paste0('aggregated_bcg_data/bcg_',i,'.rds'))
  
}


Sys.time()