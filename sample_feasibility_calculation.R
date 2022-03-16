#calculate species feasibility for raster grids
library(data.table)
library(RPostgres)
library(dplyr)
library(pool)

source('new_module/feasibility_functions.R')
load('new_module/data/E1.rda')
load('new_module/data/E1_Phase.rda')
load('new_module/data/S1.rda')


pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)


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


#retrieve bgc projection and probability
BGC <- dbGetCCISS_4km(pool, period = "2041-2060", all_weight)

#long process and use up memory
SSPred <- edatopicOverlap(BGC, E1,E1_Phase,onlyRegular = T)##create site series overlap

##the "newSuit" column in sppFeas is the predicted feasibility value
## for a specific time period that should be shown on the map
sppFeas <- ccissMap(SSPred,S1, spp_select = "Fd")
