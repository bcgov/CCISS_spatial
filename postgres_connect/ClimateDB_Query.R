library(RPostgres)
library(pool)
library(data.table)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "bgc_climate_data",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##important tables:
## - szsum_fut: future climate summarised by subzone, period, scenario
## - szsum_curr: historic climate summarised by subzone, period

##summaries by future subzone
dat <- dbGetQuery(pool,"select * from szsum_fut where bgc = 'SBSdk' and climvar = 'MAT' and stat = 'mean' and period = '2021-2040'")
