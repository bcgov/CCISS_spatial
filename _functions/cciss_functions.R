##functions for calculating feasibility metrics from CCISS data
##Kiri Daust, Colin Mahony 2022


library(data.table)
library(sf)
library(RPostgres)
library(dplyr)
library(foreach)
library(rmapshaper)
library(tictoc)
library(rasterVis)
library(raster)
library(ccissdev)
library(RPostgreSQL)
library(sf)
library(pool)


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
  suitRes <- suitVotes[,.(Curr = mean(Curr),NewSuit = mean(NewSuit)), by = .(SiteRef)]
  return(suitRes)
}


##Proportion of models predicting additions or retreat
add_retreat <- function(SSPred,suit,spp_select){
  suit <- suit[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  #suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suit2 <- suit[,.(SS_NoSpace,Feasible)]
  setnames(suit2, old = "Feasible",new = "OrigFeas")
  suitMerge <- suit2[suitMerge, on = "SS_NoSpace"]
  suitMerge[OrigFeas > 3.5, OrigFeas := NA]
  suitMerge[Feasible > 3.5, Feasible := NA]
  suitMerge[,HasValue := if(any(!is.na(OrigFeas))|any(!is.na(Feasible))) T else F, by = .(SiteRef)]
  suitMerge <- suitMerge[(HasValue),]
  suitMerge <- suitMerge[,.(SiteRef,FuturePeriod,SS_NoSpace,OrigFeas,SS.pred,Feasible,SSprob)]
  setnames(suitMerge, old = "Feasible", new = "PredFeas")
  suitMerge[,Flag := NA_character_]
  suitMerge[is.na(OrigFeas) & !is.na(PredFeas),Flag := "Expand"]
  suitMerge[!is.na(OrigFeas) & is.na(PredFeas),Flag := "Retreat"]
  suitMerge[!is.na(OrigFeas) & !is.na(PredFeas),Flag := "Same"]
  suitMerge[is.na(OrigFeas) & is.na(PredFeas),Flag := "Same"]
  suitMerge[,PropMod := sum(SSprob), by = .(SiteRef,FuturePeriod, Flag)]
  suitMerge[,PropAll := sum(SSprob), by = .(SiteRef,FuturePeriod)]
  suitMerge[,PropMod := PropMod/PropAll]
  suitRes <- unique(suitMerge[,.(SiteRef,Flag,PropMod)])
  suitRes[,SiteRef := as.integer(SiteRef)]
  setkey(suitRes,SiteRef)
  suitRes[Flag == "Retreat",PropMod := PropMod * -1]
}
