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







#calculate bgc prop from pts2Km_future table (equivalent to dbGetCCISS)
dbGetCCISS_4km <- function(con, period = "2041-2060", modWeights){
  
  modWeights[,comb := paste0("('",gcm,"','",rcp,"',",weight,")")]
  weights <- paste(modWeights$comb,collapse = ",")
  groupby = "rast_id"
  ##cciss_future is now test_future  
  cciss_sql <- paste0("
  WITH cciss AS (
    SELECT rast_id,
           futureperiod,
           bgc,
           bgc_pred,
           w.weight
    FROM pts2km_future
    JOIN (values ",weights,") 
    AS w(gcm,scenario,weight)
    ON pts2km_future.gcm = w.gcm AND pts2km_future.scenario = w.scenario
    WHERE futureperiod IN ('",period,"')
  
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
  ")
  
  dat <- setDT(RPostgres::dbGetQuery(con, cciss_sql))
  
  setnames(dat, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  dat <- unique(dat) ##should fix database so not necessary
  #print(dat)
  return(dat)
}






#EDA Topic Overlap
#' @param BGC BGC
#' @param E1 A data.table?
#' @param E1_Phase A phase?
#' @details What the function does
#' @return What the function returns
#' @import data.table
#' @importFrom dplyr left_join distinct
#' @importFrom stats complete.cases na.omit
#' @export
edatopicOverlap <- function(BGC,E1,E1_Phase,onlyRegular = FALSE){
  
  # Declare binding for checks
  if (FALSE) {
    SS_NoSpace <- 
      Edatopic <- 
      SpecialCode <- 
      BGC.pred <- 
      SS.pred <- 
      SiteRef <- 
      FuturePeriod <- 
      BGC.prop <- 
      SS.Curr <- 
      i.NumEdas <- 
      SSProb <- 
      SS.prob <- 
      SSProbRev <- 
      allOverlap <- 
      MainUnit <- 
      Phase <- 
      i.BGC <- 
      PhasePred <- 
      MainUnit.y <- 
      PhaseSum <- 
      phaseOverlap <- 
      allOverlap.y <- 
      Flag <- 
      SSratio <- 
      SSprob <- 
      NULL
  }
  
  SS <- E1[,.(BGC,SS_NoSpace,Edatopic)]
  edaPhase <- E1_Phase
  SS <- unique(SS)
  BGC <- unique(BGC)
  SSsp <- E1[!is.na(SpecialCode),list(BGC,SS_NoSpace,SpecialCode)]
  SSsp <- unique(SSsp)
  SSsp_phase <- edaPhase[!is.na(SpecialCode),list(BGC,SS_NoSpace,SpecialCode)]
  edaPhase <- edaPhase[is.na(SpecialCode),!"SpecialCode"]
  SSsp <- rbind(SSsp,SSsp_phase)
  
  ##Special site series edatopes
  CurrBGC <- SSsp[BGC, on = "BGC", allow.cartesian = T] 
  setkey(BGC, BGC.pred)
  setkey(SSsp, BGC)
  FutBGC <- SSsp[BGC, allow.cartesian = T]
  setnames(FutBGC, old = c("BGC","SS_NoSpace","i.BGC"), 
           new = c("BGC.pred","SS.pred","BGC"))
  FutBGC <- FutBGC[!is.na(SS.pred),]
  setkey(FutBGC, SiteRef, FuturePeriod, BGC,BGC.pred, SpecialCode)
  setkey(CurrBGC,SiteRef,FuturePeriod, BGC,BGC.pred, SpecialCode)
  # "CWHvh2" BGC gives out Join results in 258 rows; more than 135 = nrow(x)+nrow(i), I'm
  # setting it to allow.cartesian, might need to investigate.
  new <- CurrBGC[FutBGC, allow.cartesian=TRUE] #big table
  #new <- new[!is.na(SS_NoSpace),] ##this removes special site series that don't align
  new <- na.omit(new, cols = "SS_NoSpace")
  SSsp.out <- new[,list(allOverlap = 1/.N,SS.pred,BGC.prop), keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace)]
  
  #clean object list to free up memory
  rm(FutBGC, CurrBGC, new)
  gc()
  
  
  ##regular site series edatopes
  temp <- rbind(SS,E1_Phase[is.na(SpecialCode),list(BGC,SS_NoSpace,Edatopic)])
  CurrBGC <- temp[BGC, on = "BGC", allow.cartesian = T]
  CurrBGC <- CurrBGC[!duplicated(CurrBGC),]
  setkey(BGC, BGC.pred)
  setkey(SS, BGC)
  FutBGC <- SS[BGC, allow.cartesian = T]
  FutBGC <- FutBGC[!duplicated(FutBGC),] 
  setnames(FutBGC, old = c("BGC","SS_NoSpace","i.BGC"), 
           new = c("BGC.pred","SS.pred","BGC"))
  FutBGC <- na.omit(FutBGC)
  
  setkey(FutBGC, SiteRef, FuturePeriod, BGC,BGC.pred, Edatopic)
  FutBGC[,BGC.prop := NULL]
  
  setkey(CurrBGC,SiteRef,FuturePeriod, BGC,BGC.pred, Edatopic)
  new <- FutBGC[CurrBGC, allow.cartesian = T]
  
  #clean object and free up memory
  rm(FutBGC, CurrBGC)
  gc()
  
  new <- na.omit(new, cols = "SS_NoSpace")
  setkey(new, SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  ##new <- new[complete.cases(new),]
  
  numEda <- E1[,list(NumEdas = .N), by = list(BGC,SS_NoSpace)]
  
  ###forwards overlap
  SS.out <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)]
  SS.out[numEda,SS.Curr := i.NumEdas, on = c(SS_NoSpace = "SS_NoSpace"), allow.cartesian = T]
  SS.out[,SSProb := SS.prob/SS.Curr]
  
  ###reverse overlap
  SS.out.rev <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                    keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS.pred,SS_NoSpace)]
  SS.out.rev[numEda,SS.Curr := i.NumEdas, on = c(SS.pred = "SS_NoSpace")]
  SS.out.rev[,SSProbRev := SS.prob/SS.Curr]
  
  
  
  #clean object list to free up memory
  rm(new)
  gc()
  
  
  
  ##combine them
  combAll <- merge(SS.out,SS.out.rev,by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"))
  combAll[,allOverlap := SSProb*SSProbRev]
  setnames(combAll, old = "BGC.prop.x",new = "BGC.prop")
  combAll <- combAll[,list(SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace, 
                           allOverlap, SS.pred, BGC.prop)]
  combAll <- na.omit(combAll)
  combAllSave <- combAll
  
  gc()
  
  combAll <- merge(combAll,SSsp.out,
                   by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"), all = T)
  temp <- combAll[!is.na(allOverlap.y),]
  temp[,c("allOverlap.x","BGC.prop.x") := NULL]
  setnames(temp,old = c("allOverlap.y","BGC.prop.y"), new = c("allOverlap","BGC.prop"))
  combAll[,Flag := if(all(is.na(allOverlap.y))) T else F, by = list(SiteRef,FuturePeriod,BGC,SS_NoSpace,BGC.pred)]
  combAll <- combAll[(Flag),!"Flag"]
  combAll[,c("allOverlap.y","BGC.prop.y") := NULL]
  setnames(combAll,old = c("allOverlap.x","BGC.prop.x"), new = c("allOverlap","BGC.prop"))
  combAll <- rbind(combAll,temp)
  combAll[,MainUnit := gsub("[a-c]$|\\.[1-9]$","",SS.pred)]
  combAll <- combAll[!(BGC == BGC.pred  &  SS_NoSpace != MainUnit),] ### removes overlap where past BGC = future BGC
  combAll[,MainUnit := NULL]
  combAll <- unique(combAll[!is.na(SS_NoSpace),])
  
  
  ##add in BGC probability
  combAll <- na.omit(combAll)
  combAll[,SSratio := allOverlap/sum(allOverlap), by = list(SiteRef, FuturePeriod, BGC, BGC.pred,SS_NoSpace)] ##should check this?
  setorder(combAll, SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace)
  
  setkey(combAll, SiteRef, FuturePeriod, BGC,BGC.pred)
  temp <- unique(combAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,BGC.prop)])
  temp[,BGC.prop := BGC.prop/sum(BGC.prop), by = list(SiteRef,FuturePeriod,BGC)]
  temp <- unique(temp)
  combAll[,BGC.prop := NULL]
  combAll <- temp[combAll]
  combAll[,SSprob := SSratio*BGC.prop]
  combAll <- combAll[!duplicated(combAll),]
  noPhase <- combAll
  if(onlyRegular){
    return(noPhase)
  }
  
  rm(SS.out, SS.out.rev, temp)
  gc()
  
  ###########################################################################
  ##now redo for phases
  numEdaPh <- E1_Phase[,list(NumEdas = .N), by = list(SS_NoSpace)]
  phaseSmall <- unique(edaPhase[,list(BGC,MainUnit,Phase = SS_NoSpace)])
  combPhase <- phaseSmall[combAllSave, on = c(MainUnit = "SS.pred"), allow.cartesian = T]
  justPhase <- combPhase[!is.na(Phase),]
  curr <- unique(justPhase[,list(SiteRef, FuturePeriod, BGC = i.BGC, BGC.pred, SS_NoSpace)])
  fut <- unique(justPhase[,list(SiteRef, FuturePeriod, BGC = i.BGC, BGC.pred, MainUnit, Phase)])
  phaseTemp <- E1_Phase[,list(SS_NoSpace,Edatopic)]
  curr <- E1[curr, on = "SS_NoSpace", allow.cartesian = T] 
  fut <- phaseTemp[fut, on = c(SS_NoSpace = "Phase"),allow.cartesian = T]
  setnames(fut,old = "SS_NoSpace", new = "PhasePred")
  setkey(curr, SiteRef, FuturePeriod, BGC, BGC.pred,Edatopic)
  setkey(fut,SiteRef,FuturePeriod,BGC,BGC.pred,Edatopic)
  new <- fut[curr, allow.cartesian = T]
  new <- new[!is.na(PhasePred),]
  
  ###forwards overlap
  SS.out <- new[,list(SS.prob = .N,MainUnit = MainUnit[1]), 
                keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,PhasePred)]
  SS.out[numEda,SS.Curr := i.NumEdas, on = c(SS_NoSpace = "SS_NoSpace"), allow.cartesian = T]
  SS.out[,SSProb := SS.prob/SS.Curr]
  
  ###reverse overlap
  SS.out.rev <- new[,list(SS.prob = .N,MainUnit = MainUnit[1]), 
                    keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,PhasePred,SS_NoSpace)]
  SS.out.rev[numEdaPh,SS.Curr := i.NumEdas, on = c(PhasePred = "SS_NoSpace"), allow.cartesian = T]
  SS.out.rev[,SSProbRev := SS.prob/SS.Curr]
  
  ##combine them
  combPhaseAll <- merge(SS.out,SS.out.rev,by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","PhasePred"))
  combPhaseAll[,allOverlap := SSProb*SSProbRev]
  combPhaseAll <- combPhaseAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,
                                     SS.pred = MainUnit.y, PhasePred,phaseOverlap = allOverlap)]
  combPhaseAll <- na.omit(combPhaseAll)
  setkey(combPhaseAll,SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  setkey(combAllSave,SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  combAll <- combPhaseAll[combAllSave]
  ####add phase to non-phase###
  combAll[,PhaseSum := sum(phaseOverlap), by = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)]
  combAll[,phaseOverlap := phaseOverlap/PhaseSum]
  combAll[!is.na(phaseOverlap),allOverlap := allOverlap * phaseOverlap]
  combAll[!is.na(phaseOverlap),SS.pred := PhasePred]
  combAll[,c("phaseOverlap","PhaseSum","PhasePred") := NULL]
  ###done phases
  
  combAll <- merge(combAll,SSsp.out,
                   by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"), all = T)
  temp <- combAll[!is.na(allOverlap.y),]
  temp[,c("allOverlap.x","BGC.prop.x") := NULL]
  setnames(temp,old = c("allOverlap.y","BGC.prop.y"), new = c("allOverlap","BGC.prop"))
  combAll[,Flag := if(all(is.na(allOverlap.y))) T else F, by = list(SiteRef,FuturePeriod,BGC,SS_NoSpace,BGC.pred)]
  combAll <- combAll[(Flag),!"Flag"]
  combAll[,c("allOverlap.y","BGC.prop.y") := NULL]
  setnames(combAll,old = c("allOverlap.x","BGC.prop.x"), new = c("allOverlap","BGC.prop"))
  combAll <- rbind(combAll,temp)
  combAll[,MainUnit := gsub("[a-c]$|\\.[1-9]$","",SS.pred)]
  combAll <- combAll[!(BGC == BGC.pred  &  SS_NoSpace != MainUnit),] ### removes overlap where past BGC = future BGC
  combAll[,MainUnit := NULL]
  combAll <- unique(combAll[!is.na(SS_NoSpace),])
  
  
  ##add in BGC probability
  combAll <- na.omit(combAll)
  combAll[,SSratio := allOverlap/sum(allOverlap), by = list(SiteRef, FuturePeriod, BGC, BGC.pred,SS_NoSpace)] ##should check this?
  setorder(combAll, SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace)
  
  setkey(combAll, SiteRef, FuturePeriod, BGC,BGC.pred)
  temp <- unique(combAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,BGC.prop)])
  temp[,BGC.prop := BGC.prop/sum(BGC.prop), by = list(SiteRef,FuturePeriod,BGC)]
  temp <- unique(temp)
  combAll[,BGC.prop := NULL]
  combAll <- temp[combAll]
  combAll[,SSprob := SSratio*BGC.prop]
  combAll <- combAll[!duplicated(combAll),]
  print("Done EDA")
  return(list(phase = combAll, NoPhase = noPhase))
}

##calculate species feasibility based on edatopic overlap 
ccissMap <- function(SSPred,suit,spp_select){
  ### generate raw feasibility ratios
  
  suit <- suit[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
  # Fill with 0 if columns does not exist, encountered the error at SiteRef 3104856 
  set(suitVotes, j = as.character(1:5)[!as.character(1:5) %in% names(suitVotes)], value = 0)
  suitVotes[,VoteSum := `1`+`2`+`3`+`4`+`5`]
  suitVotes[,X := 1 - VoteSum]
  suitVotes[,VoteSum := NULL]
  suitVotes[,X := X + `5` + `4`]
  suitVotes[,`:=`(`5` = NULL, `4` = NULL)]
  setkey(suitVotes, SS_NoSpace, Spp)
  setkey(suit, SS_NoSpace, Spp)
  suitVotes[suit, Curr := i.Feasible]
  suitVotes[is.na(Curr), Curr := 5]
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[Curr > 3.5, Curr := 4]
  colNms <- c("1","2","3","X")
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitRes <- suitVotes[,.(Curr = mean(Curr),NewSuit = mean(NewSuit)), by = .(SiteRef,FuturePeriod)]
  return(suitRes)
}