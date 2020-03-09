
shinyServer(function(input, output, session) {

## AGGIORNAMENTI

  autoInvalidate <- reactiveTimer(3600)

  reacval<-reactiveValues(fileList=list.files("www/pcm_data/", full.names=T),
            oldList=NULL,
            dataTables=allData
            )

  observe({
    autoInvalidate()

    curFileList <- list.files("www/pcm_data/", full.names=T)
    curFileList <- curFileList[grepl(".csv$", curFileList) | grepl(".txt$", curFileList)]
    if (any(!(curFileList %in% isolate(reacval$fileList)))) {
      diff <- setdiff(curFileList, isolate(reacval$fileList))
      reacval$oldList <- isolate(reacval$fileList)
      reacval$fileList <- curFileList
      reacval$dataTables <- rbind(isolate(reacval$dataTables), get_covid19_data(diff))
    }
  })

  get_data <- reactive({
    reacval$dataTables
  })


observe({
#  reacval$fileList
#  reacval$oldList
#  reacval$dataTables <- lapply(isolate(reacval$fileList), )
})


## REGIONI

output$lineRegion <- renderPlotly({
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
  p <- ggplot(allDataReg) + my_ggtheme() +
        geom_line(aes(x=data, y=totale_casi, color=denominazione_regione)) +
        scale_color_manual(values=d3hexcols20)
  p
})


output$tabRegion <- renderDataTable({
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
  allDataReg$data <- as.Date(allDataReg$data)
  latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
  latestDataReg$`casi su 10^4 ab.` <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
  latestDataReg$pop <- NULL

  datatable(latestDataReg,
    selection = list(target = NULL),
    options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
    rownames=F)
})


output$mapRegion <- renderLeaflet({
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi),
                    by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione, codice_regione=allDataConf$codice_regione),
                    FUN=sum)
  latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
  pltRegioni <- merge(regioni, latestDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")
  pal <- colorBin("YlOrRd", domain = log10(pltRegioni$totale_casi))
  leaflet(data = pltRegioni) %>% addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
      addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
               label = ~paste(DEN_REG, " - casi:", totale_casi))
})


## PROVINCE

output$lineProvince <- renderPlotly({
  myReg <- input$regionSel
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
  allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi),
                    by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                    FUN=sum)
  p <- ggplot(allDataProv) + my_ggtheme() +
        geom_line(aes(x=data, y=totale_casi, color=denominazione_provincia)) +
        scale_color_manual(values=d3hexcols20)
  p
})


output$tabProvince <- renderDataTable({
  myReg <- input$regionSel
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
  allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                    by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                    FUN=sum)
  #allDataReg$data <- as.Date(allDataReg$data)
  latestDataProv <- allDataProv[allDataProv$data==max(allDataProv$data),]
  latestDataProv$`casi su 10^4 ab.`  <- round(latestDataProv$totale_casi / latestDataProv$pop * 10000, 3)
  latestDataProv$pop <- NULL

  datatable(latestDataProv,
    selection = list(target = NULL),
    options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
    rownames=F)
})


output$mapProvince <- renderLeaflet({
  myReg <- input$regionSel
  allData <- get_data()
  allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
  allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
  allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi),
                    by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia, codice_provincia=allDataConf$codice_provincia),
                    FUN=sum)
  #allDataReg$data <- as.Date(allDataReg$data)
  latestDataProv <- allDataProv[allDataProv$data==max(allDataProv$data),]
  pltProvince <- merge(province, latestDataProv[,c("codice_provincia", "totale_casi")], by.x="COD_PROV", by.y="codice_provincia")
  my_frame <- st_drop_geometry(regioni[regioni$COD_REG == unique(pltProvince$COD_REG), c("reg_long", "reg_lat")])
  #pltProvince <- merge(province, latestDataProv[,c("codice_regione", "reg_long", "reg_lat")], by.x="COD_REG", by.y="codice_regione")
  pal <- colorBin("YlOrRd", domain = log10(pltProvince$totale_casi))
  leaflet(data = pltProvince) %>% addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
      addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
               label = ~paste(DEN_UTS, " - casi:", totale_casi))
})


})
