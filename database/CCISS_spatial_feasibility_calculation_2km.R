#calculate species feasibility for raster grids
library(data.table)
library(RPostgres)
library(RPostgreSQL)
library(dplyr)
library(pool)

source('feasibility_functions.R')
load('new_module/data/E1.rda')
load('new_module/data/E1_Phase.rda')
#load('new_module/data/S1.rda')
##bybec db connection
sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

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


## Pre-bake the future period and species parameters for the inner loops
# futureperiods <- data.frame(futureperiod_id = as.integer(c(1:5)),
#                             futureperiod = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))




### NOW GET THE IDs BY PARAM FROM THE UI

futureperiod_param <- c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

sql <- paste0("SELECT futureperiod_id, fp_full as futureperiod FROM futureperiod WHERE fp_full IN ('", 
              paste(unique(futureperiod_param), collapse = "','"), 
              "') ")
futureperiods <- setDT(RPostgres::dbGetQuery(pool, sql))


species_param <- c("Pl","Sx","Fd","Py","Lw","Bl")
sql <- paste0("SELECT species_id, species FROM species WHERE species IN ('", 
              paste(unique(species_param), collapse = "','"), 
              "') ")
species <- setDT(dbGetQuery(pool, sql))



edatope_param <- c("B2","C4","E6")
sql <- paste0("SELECT edatope_id, edatope FROM edatope WHERE edatope IN ('", 
              paste(unique(edatope_param), collapse = "','"), 
              "') ")
edatopes <- setDT(dbGetQuery(pool, sql))



## PURGE AND MAINTENANCE OF pts2km_feas
sql <- "DELETE FROM pts2km_feas"
RPostgres::dbExecute(pool, sql)
sql <- "VACUUM FULL pts2km_feas"
RPostgres::dbExecute(pool, sql)
## NOW LOOP AND EXECUTE ALL PERMUTATIONS OF FuturePeriod + Species
total_bags <- nrow(futureperiods) * nrow(species) * nrow(edatopes)  
bag_no <- 0;

for (fp_index in 1:nrow(futureperiods))
{
  
  fp_start = Sys.time()
  
  print(paste("Working on CCISS 4KM for", futureperiods[fp_index,"futureperiod"]))
  
  ######  BEGIN ORIGINAL SINGLE EXECUTION - reuse for each execution by species
  #retrieve bgc projection and probability
  
  BGC <- dbGetCCISS_4km(pool, period = futureperiods[fp_index,"futureperiod"], all_weight)
  
  print("   ... dbGetCCISS_4km Done")
  
  
  #adding loop through edatope
  for (edatope_index in 1:nrow(edatopes)){
    print(paste("  Working on Edatope ", edatopes$edatope[edatope_index], "(", edatope_index, "of", nrow(edatopes),")"))
    
    #subset E1 by edatope into edaZonal
    E1 <- E1[is.na(SpecialCode),]
    E1[,HasPos := if(any(Edatopic == edatopes$edatope[edatope_index])) T else F, by = .(SS_NoSpace)]
    edaZonal <- E1[(HasPos),]
    edaZonal[,HasPos := NULL]
    

    #long process and use up memory
    SSPred <- edatopicOverlap(BGC,edaZonal,E1_Phase,onlyRegular = T) ##create site series overlap
    print("   ... edatopicOverlap Done")
    
    ######  END ORIGINAL SINGLE EXECUTION
    
    fp_end = Sys.time()
    
    print(paste0("   ... duration ", difftime(fp_end, fp_start, units="secs"), " seconds"))
    
    
    for (spp_index in 1:nrow(species))
    {
      spp_start =  Sys.time()
      
      bag <- paste(edatopes$edatope[edatope_index], futureperiods[fp_index,"futureperiod"], species[spp_index, "species"], "(", spp_index, "of", nrow(species),")")
      print(paste("    Working on bag", bag))
      
      
      out <- tryCatch(
        {
          ######  BEGIN ORIGINAL SINGLE EXECUTION -- CALL TO dbGetCCISS_4km done in outer loop
          
          ##the "newSuit" column in sppFeas is the predicted feasibility value
          ## for a specific time period that should be shown on the map
          sppFeas <- ccissMap(SSPred,S1, spp_select = species[spp_index, "species"])
          
          print("     ... ccissMap Done")
          
          
          ######  END ORIGINAL SINGLE EXECUTION
          
          
          if (nrow(sppFeas)>0)
          {
            ## Preprocess dataframe for upload to PostgreSQL table pts2km_feas
            df <- sppFeas
            
            df$futureperiod_id <- futureperiods$futureperiod_id[fp_index]
            df$edatope_id <- edatopes$edatope_id[edatope_index] 
            df$species_id <- species$species_id[spp_index]
            
            df$FuturePeriod <- NULL
            df <- df %>% relocate(futureperiod_id, .before = Curr)
            df <- df %>% relocate(species_id, .before = Curr)
            df <- df %>% relocate(edatope_id, .before = Curr)
            colnames(df) <- c("siteno", "futureperiod_id", "species_id","edatope_id", "curr", "newsuit")
            
            df$siteno <- as.integer(df$siteno)
            df$futureperiod_id <- as.integer(df$futureperiod_id)
            df$newsuit <- as.numeric(df$newsuit)
            
            
            ## Persist the DF into the DB
            dbWriteTable(pool, "pts2km_feas", df, append = T, row.names = F)
            ##RPostgres::dbAppendTable(pool, "pts2km_feas", df)
            
            df <- NULL
            
            print("     ... dbAppendTable Done")
          }
          else 
          {
            print("     ... bag is empty")  
          }
        },
        error=function(cond) {
          print(paste("     ... bag FAILED with ", cond)  )
        }
      )
      
      bag_no<- bag_no + 1;
      progress <- round(bag_no * 100.0 / total_bags, digits=2)
      
      
      spp_end =  Sys.time()
      
      print(paste0("     DONE (",  progress, "%) sp time ", difftime(spp_end, spp_start, units="secs"), " seconds"))
      
    }
  } 
}
gc()
print("COMPLETED")
###############  ONE TIME Populate Species table with Species in S1    