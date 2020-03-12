
shinyServer(function(input, output, session) {

## AGGIORNAMENTI

  autoInvalidate <- reactiveTimer(3600000)

  reacval<-reactiveValues(
						fileList=list.files(dir_prov, full.names=T),
            oldList=NULL,
            dataTables=allData,
            dateRange=date_range,
						fileList_reg=list.files(dir_reg, full.names=T),
            oldList_reg=NULL,
            dataTables_reg=allData_reg,
            dateRange_reg=date_range_reg
					)

  observe({
    autoInvalidate()

    curFileList <- list.files(dir_prov, full.names=T)
    curFileList <- curFileList[(grepl(".csv$", curFileList) | grepl(".txt$", curFileList)) & grepl('2020',curFileList)]
    if (any(!(curFileList %in% isolate(reacval$fileList)))) {
      diff <- setdiff(curFileList, isolate(reacval$fileList))
      reacval$oldList <- isolate(reacval$fileList)
      reacval$fileList <- curFileList
      reacval$dataTables <- rbind(isolate(reacval$dataTables), get_covid19_data(diff))
      reacval$dateRange <- c(isolate(reacval$dateRange), as.Date(gsub(".*/.*-(\\d{,8}).csv$", "\\1", diff), format="%Y%m%d"))
    }

		curFileList_reg <- list.files(dir_reg, full.names=T)
		curFileList_reg <- curFileList_reg[(grepl(".csv$", curFileList_reg) | grepl(".txt$", curFileList_reg)) & grepl('2020',curFileList_reg)]
		if (any(!(curFileList_reg %in% isolate(reacval$fileList_reg)))) {
			diff <- setdiff(curFileList_reg, isolate(reacval$fileList_reg))
			reacval$oldList_reg <- isolate(reacval$fileList_reg)
			reacval$fileList_reg <- curFileList_reg
			reacval$dataTables_reg <- rbind(isolate(reacval$dataTables_reg), get_covid19_data_reg(diff))
			reacval$dateRange_reg <- c(isolate(reacval$dateRange_reg), as.Date(gsub(".*/.*-(\\d{,8}).csv$", "\\1", diff), format="%Y%m%d"))
		}


		assign("resOut", list(reg=reacval$dataTables_reg, prov=reacval$dataTables), envir=.GlobalEnv)

  })

  get_data <- reactive({
    res <- reacval$dataTables
    if (!is.null(input$drangeSel)) res <- res[res$data >= input$drangeSel[1] & res$data <= input$drangeSel[2],]
    res
  })

  get_last_date <- reactive({
    strftime(max(reacval$dateRange), format="%d-%m-%Y")
  })

## CONFIG

## Se metto il dateRange dipendente dal reacVal, all'inizio la app ha due refresh e sembra congelata all'utente medio...
## quindi congelo il range dal 24/02 alla data di oggi e tanto poi il box con la data di aggiornamento dice quale sia la data più recente!
#output$drangeUI <- renderUI({
#  drange <- reacval$dateRange
#  dateRangeInput("drangeSel", label="Periodo di interesse", start = min(drange), end = max(drange), min = min(drange),
#  max = max(drange), format = "dd-mm-yyyy", startview = "month", weekstart = 1,
#  language = "it", separator = " a ")
#})



## REGIONI

output$updateRegUI <- renderUI({
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineRegion <- renderPlotly({
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
    colnames(allDataReg)[2] <- "regione"
    colnames(allDataReg)[3] <- "casi totali"
    p <- ggplot(allDataReg) + my_ggtheme() +
          geom_line(aes(x=data, y=`casi totali`, color=regione)) +
          scale_color_manual(values=d3hexcols20)
    p
  }
})


output$tabRegion <- renderDT({
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
    allDataReg$data <- as.Date(allDataReg$data)
    latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    latestDataReg$`casi su 10^4 abit.` <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
    latestDataReg$pop <- NULL
    latestDataReg$data <- strftime(latestDataReg$data, format="%d-%m-%Y")
    colnames(latestDataReg)[2] <- "regione"
    colnames(latestDataReg)[3] <- "casi totali"

    datatable(latestDataReg,
      selection = list(target = NULL),
      options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
      rownames=F)
  }
})


output$mapRegion <- renderLeaflet({
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione, codice_regione=allDataConf$codice_regione),
                      FUN=sum)
    latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    latestDataReg$densita_casi <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
    pltRegioni <- merge(regioni, latestDataReg[,c("codice_regione", "totale_casi", "densita_casi")], by.x="COD_REG", by.y="codice_regione")
    pal <- colorBin("YlOrRd", domain = log10(pltRegioni$totale_casi))
    leaflet(data = pltRegioni) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
                 label = ~paste(DEN_REG, "- casi:", totale_casi))
  }
})


## PROVINCE

output$updatePrvUI <- renderUI({
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineProvince <- renderPlotly({
  myReg <- input$regionSel
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
    allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi),
                      by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                      FUN=sum)
    colnames(allDataProv)[2] <- "provincia"
    colnames(allDataProv)[3] <- "casi totali"
    p <- ggplot(allDataProv) + my_ggtheme() +
          geom_line(aes(x=data, y=`casi totali`, color=provincia)) +
          scale_color_manual(values=d3hexcols20)
    p
  }
})


output$tabProvince <- renderDT({
  myReg <- input$regionSel
  if (!is.null(allData)) {
    allData <- get_data()
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
    allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                      FUN=sum)
    latestDataProv <- allDataProv[allDataProv$data==max(allDataProv$data),]
    latestDataProv$`casi su 10^4 abit.` <- round(latestDataProv$totale_casi / latestDataProv$pop * 10000, 3)
    latestDataProv$pop <- NULL
    latestDataProv$data <- strftime(latestDataProv$data, format="%d-%m-%Y")
    colnames(latestDataProv)[2] <- "provincia"
    colnames(latestDataProv)[3] <- "casi totali"

    datatable(latestDataProv,
      selection = list(target = NULL),
      options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
      rownames=F)
  }
})


output$mapProvince <- renderLeaflet({
  myReg <- input$regionSel
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
    allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia, codice_provincia=allDataConf$codice_provincia),
                      FUN=sum)
    latestDataProv <- allDataProv[allDataProv$data==max(allDataProv$data),]
    latestDataProv$densita_casi <- round(latestDataProv$totale_casi / latestDataProv$pop * 10000, 3)
    pltProvince <- merge(province, latestDataProv[,c("codice_provincia", "totale_casi", "densita_casi")], by.x="COD_PROV", by.y="codice_provincia")
    my_frame <- st_drop_geometry(regioni[regioni$COD_REG == unique(pltProvince$COD_REG), c("reg_long", "reg_lat")])
    #pltProvince <- merge(province, latestDataProv[,c("codice_regione", "reg_long", "reg_lat")], by.x="COD_REG", by.y="codice_regione")
    pal <- colorBin("YlOrRd", domain = log10(pltProvince$totale_casi))
    leaflet(data = pltProvince) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
                 label = ~paste(DEN_UTS, "- casi:", totale_casi))

  }
})


})
