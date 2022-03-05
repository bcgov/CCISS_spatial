library(raster)
library(sf)
library(data.table)
library(RPostgreSQL)
Rcpp::sourceCpp("~/CCISS_ver2020/0CCISS_Cfn.cpp")
source("~/CCISS_ver2020/2CCISS_EdaOverlap.R")
source("~/CCISS_ver2020/3CCISS_Suit.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", host = "localhost",password = "Kiriliny41", 
                 port = 5432, dbname = "cciss_data")

dat <- dbGetQuery(con, "select gcm,scenario,futureperiod,siteno,bgc,bgc_pred from dat_comb where scenario = 'rcp45' and futureperiod = '2055' and dist_code = 'DSC'")
sno <- unique(dat$siteno)
snoSmall <- sno[1:68000]
dat <- as.data.table(dat)
dat <- dat[siteno %in% snoSmall,]

dat <- fread("./inputs/DPG_HexTest.csv")
setnames(dat,c("SiteNo","Scenario", "GCM","FuturePeriod","BGC.pred","BGC"))
#dat[,Scenario := "rcp45"]
setcolorder(dat,c("GCM","Scenario","FuturePeriod","SiteNo","BGC","BGC.pred"))

dat[,TotNum := .N, by = .(SiteNo,FuturePeriod,BGC)]
dat2 <- dat[,.(BGC.prop = .N/TotNum), keyby = .(SiteNo,FuturePeriod,BGC,BGC.pred)]
dat2 <- unique(dat2)

eda <- fread(file.choose())
ssUse <- fread("./CreateSSDat/SiteSeries_Use_CCISSpaper.csv")
setnames(ssUse, old = "MergedBGC","BGC")
library(tictoc)
tic()
SSPreds <- edatopicOverlap(dat2,Edatope = ssUse)
SSPreds[ssUse,Eda := i.Edatopic, on = "SS_NoSpace"]
fwrite(SSPreds,"./inputs/DPG_SSPreds.csv")
toc()

suit <- fread(file.choose())
suit <- suit[,.(BGC,SS_NoSpace,Spp,Feasible)]
suit <- suit[Spp == "Sx",]
suit <- unique(suit)
SSPred <- SSPreds[,.(SiteNo,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
SSPred <- SSPred[grep("01",SS_NoSpace),]

tic()
Site_BGC <- unique(SSPred[,.(SiteNo,BGC)])
setkey(SSPred,SS.pred)
setkey(suit,SS_NoSpace)
suitMerge <- suit[SSPred]
setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
suitMerge <- suitMerge[!is.na(Spp),]
suitVotes <- data.table::dcast(suitMerge, SiteNo + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                               value.var = "SSprob", fun.aggregate = sum)
suitVotes[,VoteSum := `1`+`2`+`3`+`4`]
suitVotes[,X := 1 - VoteSum]
suitVotes[,VoteSum := NULL]
suitVotes[,X := X + `4`]
suitVotes[,`:=`(`4` = NULL)]
setkey(suitVotes, SS_NoSpace, Spp)
setkey(suit, SS_NoSpace, Spp)
suitVotes[suit, Curr := i.Feasible]
suitVotes[is.na(Curr), Curr := 5]
suitVotes[,NewSuit := NewSuitNoCurr(as.matrix(.SD),c(1,2,3,5)), .SDcols = c("1","2","3","X")]
toc()