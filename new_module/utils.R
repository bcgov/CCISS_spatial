#functions used in the app

#' function to retrieve raw BGC projection in 400m grid by user provided polygon
#' function derived from dbGetCCISSRaw
#' @param con database connection
#' @param poly polygon geometry in string from st_as_text()
#' @param gcm GCM model
#' @param scenario Scenario
#' @param period Future time period


dbGetCCISSRaw2 <- function(con, poly, gcm, scenario, period){
  cciss_sql <- paste0("
    SELECT cciss_future12_array.siteno,
         hex_points.geom,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc_attribution.bgc,
         bgc.bgc bgc_pred
  FROM cciss_future12_array
  JOIN hex_points
    ON cciss_future12_array.siteno = hex_points.siteno
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
  WHERE st_intersects(geom, 'SRID=3005;",poly,"')
  AND futureperiod IN ('",paste(unique(period), collapse = "','"), "')
  AND scenario IN ('",paste(unique(scenario), collapse = "','"), "')
  AND gcm IN ('",paste(unique(gcm), collapse = "','"), "')
  ")
  
  dat <- setDT(st_read(con, query = cciss_sql))
  #dat <- unique(dat)
  return(dat)
}


#' function to retrieve species feasibility based on raw BGC projection in 400m grid from user provided polygon
#' function derived from dbGetCCISSRaw
#' @param con database connection
#' @param poly polygon geometry in string from st_as_text()
#' @param gcm GCM model
#' @param scenario Scenario
#' @param period Future time period


dbGetFeas400m <- function(con, poly, period, spp, edatope){
  
  cciss_sql <- paste0("
    SELECT b.geom,
           a.curr,
           a.newsuit
  FROM pts400m_feas a
  JOIN hex_points b
  ON a.siteno = b.siteno
  WHERE st_intersects(geom, 'SRID=3005;",poly,"')
  AND a.futureperiod_id IN (",paste(unique(period), collapse = ","), ")
  AND a.species_id IN (",paste(unique(spp), collapse = ","), ")
  AND a.edatope_id IN (",paste(unique(edatope), collapse = ","), ")
  ")
  
  RPostgres::dbExecute(con,  "set enable_seqscan = off")
  dat <- setDT(st_read(con, query = cciss_sql))
  
  RPostgres::dbExecute(con,  "set enable_seqscan = on")
  
  #dat <- unique(dat)
  return(dat)
}

#' function to retrieve BGC projection and ID from 2Km grid raster data
#' @param con database connection
#' @param period Future time period
#' @param scn Scenario
#' @param gcm GCM model

dbGetbgc_raster <- function(con, period, scn, gcm){
  
  sql_query <- paste0("select rast_id, bgc_pred from pts2km_future
          where futureperiod = '",period, "'
          and   scenario = '", scn , "'
          and   gcm = '", gcm, "'
          ")
  dat <- dbGetQuery(con, sql_query)
  
  setDT(dat)
  
  dat
}

#' function to retrieve count of BGC projections in 400m grid in user provided polygon
#' @param con database connection
#' @param polygon polygon geometry in string from st_as_text()

dbGetbgc_count <- function(con, polygon){
  cciss_sql <- paste0("
    SELECT count(*) as n,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc.bgc bgc_pred
  FROM cciss_future12_array
  JOIN hex_points
    ON cciss_future12_array.siteno = hex_points.siteno
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
  WHERE st_intersects(hex_points.geom, 'SRID=3005;",polygon,"')
  GROUP BY labels.gcm,labels.scenario,labels.futureperiod,bgc.bgc
  ")
  
  
  dat <- setDT(dbGetQuery(con, cciss_sql))
  return(dat)
}

#' function to retrieve raw BGC projection in 400m grid by user provided polygon
#' function derived from dbGetCCISSRaw
#' @param con database connection
#' @param poly polygon geometry in string from st_as_text()


dbGetCCISSRaw3 <- function(con, poly){
  cciss_sql <- paste0("
    SELECT cciss_future12_array.siteno,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc_attribution.bgc,
         bgc.bgc bgc_pred,
         pts400m_feas.species_id,
         pts400m_feas.edatope_id
  FROM cciss_future12_array
  JOIN hex_points
    ON cciss_future12_array.siteno = hex_points.siteno
  JOIN pts400m_feas
    ON cciss_future12_array.siteno = pts400m_feas.siteno
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
  WHERE st_intersects(geom, 'SRID=3005;",poly,"')
  ")
  
  dat <- setDT(dbGetQuery(con, cciss_sql))
  dat <- unique(dat)
  return(dat)
}

#' function to retrieve species feasibility, used for graphic summary 
#' @param bgc BGC projection output from dbGetCSSISRaw function
#' @param E1 E1 edatope table
#' @param S1 S1 table

feasCal <- function(bgc, E1, S1) {
  
  #edatope overlay by Edatope (reduce RAM consumption each step)
  E1 <- E1[is.na(SpecialCode),]
  E1[,HasPos := if(any(Edatopic == "B2")) T else F, by = .(SS_NoSpace)]
  edaZonal <- E1[(HasPos),]
  edaZonal[,HasPos := NULL]
  
  ##regular site series edatopes
  temp <- edaZonal[,.(BGC, SS_NoSpace, Edatopic)]
  CurrBGC <- temp[bgc, on = .(BGC == bgc), allow.cartesian = TRUE]
  
  gc()
  CurrBGC <- CurrBGC[!duplicated(CurrBGC),]
  FutBGC <- temp[bgc,on = .(BGC == bgc_pred),allow.cartesian = T]
  
  gc()
  FutBGC <- FutBGC[!duplicated(FutBGC),] 
  
  
  setnames(FutBGC, old = c("BGC","SS_NoSpace"), 
           new = c("bgc_pred","SS.pred"))
  FutBGC <- na.omit(FutBGC)
  
  setkey(CurrBGC,siteno,futureperiod,scenario, gcm, BGC, bgc_pred, Edatopic)
  setkey(FutBGC, siteno,futureperiod,scenario, gcm, bgc, bgc_pred, Edatopic)
  
  gc()
  SSPred <- FutBGC[CurrBGC, allow.cartesian = T]
  
  #clean object and free up memory
  rm(FutBGC, CurrBGC)
  gc()
  
  SSPred <- na.omit(SSPred, cols = "SS.pred")
  
  #join with S1 to retrieve feasibility by species
  SSPred <- SSPred[,.(siteno, gcm, scenario, futureperiod, bgc_pred, SS.pred )]
  SSPred <- unique(SSPred)
  suitMerge <- S1[SSPred,on = .(SS_NoSpace = SS.pred), allow.cartesian = T]
  
  #take average feasibility by siteno, Spp, futureperiod, gcm, scenario and bgc_pred
  sspFeas <- suitMerge[, keyby = .(Spp, siteno, gcm, scenario, futureperiod, bgc_pred), .(Feas = mean(Feasible))]
  sspFeas[, edatope := "B2"]
  
  return(sspFeas)  
  
}

