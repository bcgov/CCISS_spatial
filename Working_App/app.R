library(shiny)
library(leaflet)
library(RPostgreSQL)
library(sf)
library(shinyWidgets)
library(data.table)
library(colourvalues)
library(Rcpp)
library(shinyjs)
library(shinybusy)
source("HexSource.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "jujL6cB3Wm9y", host = "138.197.168.220", 
                 port = 5432, dbname = "cciss")

cw <- fread("./inputs/BigGrid_Crosswalk.csv")
newIDs <- unique(cw$Index)
allID <- fread("./inputs/DPG_AllID.csv")
transCol <- data.table(ID = newIDs,Col = "#FFFFFFFF")
cols <- fread("./inputs/WNA_v12_HexCols.csv")
dists <- dbGetQuery(con,"select distinct dist_code from dist_codes")[,1]
load("./inputs/Dist_MapBoundaries.Rdata")
gcms <- c("CESM1-CAM5","CanESM2","HadGEM2-ES","MIROC5","GISS-E2R","CCSM4","CSIRO-Mk3-6-0","MPI-ESM-LR")
feas <- fread("./inputs/Feasibility_v11_21.csv")
feas <- feas[!is.na(Feasible),.(BGC,SS_NoSpace,Spp,Feasible)]
feas <- feas[Spp != "X",]
allSpp <- c("Py","Fd","At","Ep","Sx","Pl","Bl","Cw","Hw","Pw","Lw")

cppFunction("
NumericVector NewSuitNoCurr(NumericMatrix x, NumericVector vals){
  int n = x.nrow();
  NumericVector res(n);
  for(int i = 0; i < n; i++){
    res[i] = vals[0]*x(i,0)+vals[1]*x(i,1)+vals[2]*x(i,2)+vals[3]*x(i,3);
  }
  return(res);
}")

LG_cols <- data.table(PropMod = c(-1,-0.9,-0.7,-0.5,-0.3,0.3,0.5,0.7,0.9),
                      Col = c("#ff1900","#ff6633","#f78952","#ffcb94","#ffffff","#92eff7","#69cfff","#3b9dff","#002aff"))
setkey(LG_cols,PropMod)
feas_cols <- data.table(Suit = c(1,2,3),Col = c("#0c8a32","#43a7e0","#db3700"))
change_cols <- data.table(Diff = c(-3,-2,-1,0,1,2,3),
                          Col = c("#ff1900","#ff6633","#f78952","#ffffff","#69cfff","#3b9dff","#002aff"))
lgLeg <- list(
  labels = c(LG_cols$PropMod,"Bifurcating"),
  colours = c(LG_cols$Col,"#c217a8"),
  title = "% Model loss/gain"
)

rawLeg <- list(
  labels = c("Good Feasibility","Poor Feasibility"),
  colours = c("#440154FF","#FDE725FF"),
  title = "Model Votes Raw"
)

feasLeg <- list(
  labels = feas_cols$Suit,
  colours = feas_cols$Col,
  title = "Tree Feasibility"
)

changeLeg <- list(
  labels = change_cols$Diff,
  colours = change_cols$Col,
  title = "Feasibility Change"
)

calcFeas <- function(spp,eda,type){
    SSPreds <- fread("./inputs/DPG_SSPreds.csv")
    suit <- unique(feas[Spp == spp,])
    SSPreds <- SSPreds[Eda == eda,]
    SSPred <- SSPreds[,.(SiteNo,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
    
    setkey(SSPred,SS.pred)
    setkey(suit,SS_NoSpace)
    suitMerge <- suit[SSPred]
    setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
    suitMerge <- suitMerge[!is.na(Spp),]
    if(type == "Loss/Gain"){
      suitMerge[suit, Curr := i.Feasible, on = "SS_NoSpace"]
      suitMerge <- suitMerge[,.(SiteNo,FuturePeriod,SS_NoSpace,SS.pred,SSprob,Feasible,Curr)]
      setkey(suitMerge,SiteNo,FuturePeriod)
      suitMerge[,Diff := Curr - Feasible]
      suitMerge[,Class := fifelse(Diff < 0,"Loss",fifelse(Diff == 0,"Same","Gain"))]
      suitSum <- suitMerge[,.(PropMod = sum(SSprob)), by = .(SiteNo,FuturePeriod,Class)]
      suitSum <- suitSum[Class != "Same",]
      suitSum[,`:=`(Min = min(PropMod),Max = max(PropMod)), by = .(SiteNo,FuturePeriod)]
      suitSum[,Bifurc := fifelse((Min != Max) & (Min > 0.25*Max),-1,NA_integer_)]
      suitSum <- suitSum[suitSum[,.I[which.max(PropMod)],by = .(SiteNo,FuturePeriod)]$V1]
      suitSum[,PropMod := fifelse(Class == "Loss",PropMod*-1,PropMod)]
      suitSum[,`:=`(Min = NULL,Max = NULL)]
      setkey(suitSum,PropMod)
      suitSum <- LG_cols[suitSum, roll = T]
      suitSum[Bifurc == -1,Col := "#c217a8"]
      suitSum <- suitSum[,.(SiteNo,FuturePeriod,Col)]
      suitSum <- na.omit(suitSum)
      return(suitSum)
    }
    temp <- data.table(SiteNo = -1, Spp = spp, FuturePeriod = 2025, SS_NoSpace = "Temp", Feasible = c(1,2,3,4,5))
    suitMerge <- rbind(suitMerge,temp, fill = T)
    suitVotes <- data.table::dcast(suitMerge, SiteNo + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                   value.var = "SSprob", fun.aggregate = sum)
    suitVotes <- suitVotes[SiteNo != -1,]
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
    suitVotes <- suitVotes[,.(SiteNo,SS_NoSpace, FuturePeriod,NewSuit,Curr)]
    if(type == "RawVotes"){
      suitVotes[,Col := colour_values(NewSuit)]
      suitVotes <- suitVotes[,.(SiteNo,FuturePeriod,Col)]
      return(suitVotes)
    }else if(type == "Feasibility"){
      suitVotes[,NewSuit := round(NewSuit)]
      suitVotes <- suitVotes[NewSuit < 3.5,]
      suitVotes[feas_cols,Col := i.Col, on = c(NewSuit = "Suit")]
      suitVotes <- suitVotes[,.(SiteNo,FuturePeriod,Col)]
      return(suitVotes)
    }else if(type == "Change"){
      suitVotes[,NewSuit := round(NewSuit)]
      suitVotes <- suitVotes[NewSuit < 4.5,]
      suitVotes[,Diff := Curr - NewSuit]
      suitVotes[change_cols, Col := i.Col, on = "Diff"]
      suitVotes <- suitVotes[,.(SiteNo,FuturePeriod,Col)]
      return(suitVotes)
    }
}

calcFeasHist <- function(spp,eda,type){
  SSPreds <- fread("./inputs/DPG_Historic_SS.csv")
  suit <- unique(feas[Spp == spp,])
  SSPreds <- SSPreds[Edatopic == eda,]
  SSPreds[suit,NewSuit := i.Feasible, on = c(SSPred = "SS_NoSpace")]
  SSPreds[suit,CurrSuit := i.Feasible, on = c("SS_NoSpace")]
  SSPreds <- SSPreds[!is.na(NewSuit),]
  SSPreds <- SSPreds[,.(SiteNo,Period,NewSuit,CurrSuit)]
  colnames(SSPreds)[2] <- "FuturePeriod"
  if(type == "Change" | type == "Loss/Gain"){
    SSPreds[,Diff := CurrSuit - NewSuit]
    SSPreds[change_cols, Col := i.Col, on = "Diff"]
    SSPreds <- SSPreds[,.(SiteNo,FuturePeriod,Col)]
    SSPreds <- na.omit(SSPreds)
    return(SSPreds)
  }else{
    SSPreds[feas_cols,Col := i.Col, on = c(NewSuit = "Suit")]
    SSPreds <- SSPreds[,.(SiteNo,FuturePeriod,Col)]
    SSPreds <- na.omit(SSPreds)
    return(SSPreds)
  }
}

origDat <- calcFeasHist("Sx","C4","Feasibility")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Single Map",
             fluidRow(
               column(2,
                      selectInput("Dist","Choose District",choices = "DPG",multiple = F,selected = "DPG"),
                      radioButtons("Type","Choose Map Type",choices = c("BGC","Feasibility"),selected = "Feasibility"),
                      radioButtons("period","Select Period",choiceNames = 
                                     c("Reference","Current","2011-2040","2041-2070","2071-2100"),
                                   choiceValues = c("Normal61","Current91","2025","2055","2085"),
                                   selected = "Current91")
                      ),
               column(6,
                      leafletjs_hex,
                      #add_busy_spinner(),
                      leafletOutput("map1",height = "80vh")
                      ),
               column(4,
                      h3("Feasibility Options"),
                      selectInput("sppPick","Select Tree Species",choices = c("Choose one" = "", allSpp)),
                      selectInput("edaPick","Select Site Position",choices = c("C4","B2","D6"),selected = "C4"),
                      radioButtons("feasType","Select map type", choices = c("Feasibility","RawVotes","Change","Loss/Gain"),
                                   selected = "Feasibility"),
                      hr(),
                      h3("BGC Options"),
                      selectInput("col1_gcm","Select GCM",choices = gcms),
                      radioButtons("col1_scn","Select Scenario", choices = c("rcp45","rcp85"))
                      )
             )
      )
  
  )

)

server <- function(input, output, session) {
    
    globalServer <- reactiveValues()
    globalFeas <- reactiveValues(data = origDat,datPer = origDat[FuturePeriod == "Current91",])
    globalChange <- reactiveVal(TRUE)
    globalLeg <- reactiveValues(leg = feasLeg)
    
    observeEvent(input$Dist,{
      globalServer$Server <- paste0("http://178.128.227.4/data/",input$Dist,"/{z}/{x}/{y}.pbf")
      globalServer$Layer <- input$Dist
    })
    
    observe({
      if(input$Type == "BGC"){
        globalLeg$leg <- NULL
      }else{
        if(input$feasType == "Feasibility"){
          globalLeg$leg <- feasLeg
        }else if(input$feasType == "RawVotes"){
          globalLeg$leg <- rawLeg
        }else if(input$feasType == "Change"){
          globalLeg$leg <- changeLeg
        }else{
          globalLeg$leg <- lgLeg
        }
      }
    })
    
    observe({
      input$sppPick
      input$edaPick
      input$Type
      input$feasType
      globalChange(TRUE)
    },priority = 200)
    
    observe({
      dat <- globalFeas$data
      if(input$period %in% dat$FuturePeriod){
        globalChange(FALSE)
      }else{
        globalChange(TRUE)
      }
    },priority = 203)
    
    observe({
      if(input$Type == "Feasibility"){
        leafletProxy("map1") %>%
          addLegend(position = "bottomright",
                    labels = globalLeg$leg$labels,
                    colors = globalLeg$leg$colours,
                    title = globalLeg$leg$title,
                    layerId = "bec_hex")
      }else{
        leafletProxy("map1") %>%
          clearControls()
      }
      
    })
    
    observeEvent({c(input$sppPick,input$edaPick,input$period,input$Type,input$feasType)},{
      show_modal_spinner()
      if(globalChange()){
        if(input$period %in% c('2025','2055','2085')){
          globalFeas$data <- calcFeas(input$sppPick,input$edaPick,input$feasType)
        }else{
          globalFeas$data <- calcFeasHist(input$sppPick,input$edaPick,input$feasType)
        }
      }
    },priority = 100)
    
    observeEvent({c(input$period,input$sppPick,input$edaPick,input$Type,input$feasType)},{
      if(input$Type == "Feasibility"){
        dat <- globalFeas$data 
        dat <- dat[FuturePeriod == input$period,]
        dat <- dat[allID, on = c(SiteNo = "ID")]
        dat[is.na(Col),Col := "#919191"]
        dat[,Type := NULL]
        dat[,FuturePeriod := NULL]
        setnames(dat,c("NewID","Col"))
        session$sendCustomMessage("newCol1",dat[,.(NewID,Col)])
        remove_modal_spinner()
      }
    },priority = 80)
  
    getDistDat1 <- reactive({
      if(input$period %in% c('2025','2055','2085')){
        dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
                                     "' and scenario = '",input$col1_scn,"' and futureperiod = '",
                                     input$period,"' and gcm = '",input$col1_gcm,"'"))
      }else{
        dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from historic_sf where dist_code = '",input$Dist,
                                     "' and period = '",
                                     input$period,"'"))
      }
        
        dat <- as.data.table(dat)
        dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
        dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
        dat2[,N := NULL]     
        dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
        dat2
    })

    output$map1 <- renderLeaflet({
        temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
        leaflet() %>%
            fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
            addHexMap() %>%
            invokeMethod(data = "xxx", method = "addHexTiles1", globalServer$Server,globalServer$Layer)
    })
    
    observeEvent({c(input$col1_scn,input$col1_gcm,input$sppPick,
                    input$edaPick,input$period,
                    input$Type)},{
      if(input$Type == "BGC"){
        show_modal_spinner()
        dat <- getDistDat1()
        session$sendCustomMessage("newCol1",dat[,.(NewID,Col)])
        remove_modal_spinner()
      }
    },priority = 20)

    # observeEvent({c(input$Dist)},{
    #     colDat <- getDistDat1()
    #     leafletProxy("map1") %>%
    #         invokeMethod(data = colDat, method = "addHexTiles1", ~NewID,~Col,globalServer$Server,globalServer$Layer)
    # })
    observeEvent(input$Dist,{
        temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
        leafletProxy("map1") %>%
            fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4])
    })
    
    onStop(function() {
      dbDisconnect(conn = con)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# getDistDat2 <- reactive({
#     dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
#                                  "' and scenario = '",input$col2_scn,"' and futureperiod = '",
#                                  input$col2_per,"' and gcm = '",input$col2_gcm,"'"))
#     dat <- as.data.table(dat)
#     dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
#     dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
#     dat2[,N := NULL]     
#     dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
#     dat2
# })
# 
# getDistDat3 <- reactive({
#     dat <- dbGetQuery(con,paste0("select siteno,bgc_pred from future_sf where dist_code = '",input$Dist,
#                                  "' and scenario = '",input$col3_scn,"' and futureperiod = '",
#                                  input$col3_per,"' and gcm = '",input$col3_gcm,"'"))
#     dat <- as.data.table(dat)
#     dat[cw,NewID := i.Index, on = c(siteno = "OldIdx")]
#     dat2 <- dat[,.N,by = .(NewID,bgc_pred)][order(-N), .SD[1], by = NewID]
#     dat2[,N := NULL]     
#     dat2[cols,Col := i.Col, on = c(bgc_pred = "BGC")]
#     dat2
# })

# output$map2 <- renderLeaflet({
#   temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
#   leaflet() %>%
#     fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
#     addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
#     addHexMap()
# })
# 
# output$map3 <- renderLeaflet({
#   temp <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$Dist][[1]])
#   leaflet() %>%
#     fitBounds(lng1 = temp[1], lat1 = temp[2],lng2 = temp[3], lat2 = temp[4]) %>%
#     addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron") %>%
#     addHexMap()
# })

# observeEvent({c(input$Dist)},{
#   colDat <- getDistDat2()
#   leafletProxy("map2") %>%
#     invokeMethod(data = colDat, method = "addHexTiles2", ~NewID,~Col,globalServer$Server,globalServer$Layer)
# })
# 
# observeEvent({c(input$Dist)},{
#   colDat <- getDistDat3()
#   leafletProxy("map3") %>%
#     invokeMethod(data = colDat, method = "addHexTiles3", ~NewID,~Col,globalServer$Server,globalServer$Layer)
# })

# observeEvent({c(input$col2_scn,input$col2_per,input$col2_gcm)},{
#   dat <- getDistDat2()
#   session$sendCustomMessage("newCol2",dat[,.(NewID,Col)])
# })
# 
# observeEvent({c(input$col3_scn,input$col3_per,input$col3_gcm)},{
#   dat <- getDistDat3()
#   session$sendCustomMessage("newCol3",dat[,.(NewID,Col)])
# })