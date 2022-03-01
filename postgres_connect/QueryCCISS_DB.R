## Kiri Daust, Bruno Tremblay, 2022

##this script contains code to connect to the CCISS database
##and query projections from it. The first function (dbGetCCISS) returns projections
##summarised by proportion - this is used for the feasibility calculations.
##The second function (dbGetCCISSRaw) returns raw bgc projections for selected
##hex cells and parameters. Something like this is what you'll use for creating the 
##BGC maps (i.e., users will select a single period, scenario, gcm, and then this function
## will return a single bgc projection for each hex cell specified in siteno).

library(RPostgres)
library(pool)
library(data.table)

# connect to cciss database
# need .Renviron file
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##this is the function we use in the cciss tool. It returns a data.table
##with proportions for all predicted ecosystem types

dbGetCCISS <- function(con, siteno, avg, modWeights){
  
  # Declare binding for checks
  if (FALSE) {
    comb <- gcm <- rcp <- weight <- NULL
  }
  
  groupby = "siteno"
  if (isTRUE(avg)) {
    groupby = "bgc"
  }
  modWeights[,comb := paste0("('",gcm,"','",rcp,"',",weight,")")]
  weights <- paste(modWeights$comb,collapse = ",")
  
  ##cciss_future is now test_future  
  cciss_sql <- paste0("
  WITH cciss AS (
    SELECT cciss_future12_array.siteno,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc_attribution.bgc,
         bgc.bgc bgc_pred,
         w.weight
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
    JOIN (values ",weights,") 
    AS w(gcm,scenario,weight)
    ON labels.gcm = w.gcm AND labels.scenario = w.scenario
  JOIN bgc
    ON bgc.bgc_id = source.bgc_pred_id
  WHERE cciss_future12_array.siteno IN (", paste(unique(siteno), collapse = ","), ")
  AND futureperiod IN ('2021','2041','2061','2081')
  
  ), cciss_count_den AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           SUM(weight) w
    FROM cciss
    GROUP BY ", groupby, ", futureperiod
  
  ), cciss_count_num AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           bgc,
           bgc_pred,
           SUM(weight) w
    FROM cciss
    GROUP BY ", groupby, ", futureperiod, bgc, bgc_pred
  
  ), cciss_curr AS (
      SELECT cciss_prob12.siteno,
      period,
      bgc_attribution.bgc,
      bgc_pred,
      prob
      FROM cciss_prob12
      JOIN bgc_attribution
      ON (cciss_prob12.siteno = bgc_attribution.siteno)
      WHERE cciss_prob12.siteno IN (", paste(unique(siteno), collapse = ","), ")
      
  ), curr_temp AS (
    SELECT ", groupby, " siteref,
           COUNT(distinct siteno) n
    FROM cciss_curr
    GROUP BY ", groupby, "
  )
  
  SELECT cast(a.siteref as text) siteref,
         a.futureperiod,
         a.bgc,
         a.bgc_pred,
         a.w/cast(b.w as float) bgc_prop
  FROM cciss_count_num a
  JOIN cciss_count_den b
    ON a.siteref = b.siteref
   AND a.futureperiod = b.futureperiod
   WHERE a.w <> 0
  
  UNION ALL

  SELECT cast(", groupby, " as text) siteref,
          period as futureperiod,
          bgc,
          bgc_pred,
          SUM(prob)/b.n bgc_prop
  FROM cciss_curr a
  JOIN curr_temp b
    ON a.",groupby," = b.siteref
  WHERE siteno in (", paste(unique(siteno), collapse = ","), ")
  GROUP BY ", groupby, ",period,b.n, bgc, bgc_pred
  
  UNION ALL

  SELECT DISTINCT 
            cast(", groupby, " as text) siteref,
            '1961' as futureperiod,
            bgc,
            bgc as bgc_pred,
            cast(1 as numeric) bgc_prop
    FROM cciss_curr
    WHERE siteno IN (", paste(unique(siteno), collapse = ","), ")
  ")
  
  dat <- setDT(RPostgres::dbGetQuery(con, cciss_sql))
  
  setnames(dat, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  dat <- unique(dat) 
  #print(dat)
  return(dat)
}

### create parameter input charts: this part's a bit clunkly, but it was 
### the easiest way to set up the ccissdev app to weight different options differently - you can mainly ignore it
gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,1,0,0,1,1,1,0,1,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]

##for example to return proportion data for hex cell 6:
dat <- dbGetCCISS(pool,6,F,all_weight)

##function to return raw predictions for specified parameters
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

##all possible parameter options
gcmOpts <- dbGetQuery(pool, "select gcm from gcm")[,1]
scenarioOpts <- dbGetQuery(pool, "select scenario from scenario")[,1]
periodOpts <- dbGetQuery(pool, "select futureperiod from futureperiod")[,1]

##eg to retrieve all projections for hex cell # 6:
dat <- dbGetCCISSRaw(pool,6,gcmOpts,scenarioOpts,periodOpts)
