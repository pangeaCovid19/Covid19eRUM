
shinyServer(function(input, output, session) {






  DT_lang_opt <- list(language = list(lengthMenu="Mostra _MENU_ righe per pagina",
                info="Vista da _START_ a _END_ di _TOTAL_ elementi",
                infoEmpty="Vista da 0 a 0 di 0 elementi",
                infoFiltered="(filtrati da _MAX_ elementi totali)",
                paginate=list(previous="Precedente",`next`="Successivo")))

## AGGIORNAMENTI

  autoInvalidate <- reactiveTimer(3600000)

  reacval<-reactiveValues(
						fileList=list.files(dir_prov, full.names=T),
            oldList=NULL,
            dataTables_reg_flt=allData_reg_flt,
            dataTables_prv=allData_prv,
            dateRange_prv=date_range_prv,
						mdataProv=mtimeProv,
						fileList_reg=list.files(dir_reg, full.names=T),
            oldList_reg=NULL,
            dataTables_reg=allData_reg,
            dateRange_reg=date_range_reg,
						mdataReg=mtimeReg,
						modelliIta=modelliIta,
						modelliReg=modelliReg,
						modelliItaExp=modelliItaExp,
						modelliRegExp=modelliRegExp,
            mobile=F
					)
          observe({


          		if (!is.null(session$request$HTTP_USER_AGENT)){
          			if (grepl("mobile",tolower(session$request$HTTP_USER_AGENT)) || grepl("iphone",tolower(session$request$HTTP_USER_AGENT)) )
          			reacval$mobile<-T
          		}
          	})

  observe({
		if(verbose) cat("\n OBSERVE:leggiDati")
    autoInvalidate()
		pathProv 	<- paste0(dir_prov,provRDS)
		pathReg 	<- paste0(dir_reg,regRDS)
		if (file.info(pathProv)$mtime > isolate(reacval$mdataProv)) {
      ## Qui creiamo anche i dati regionali con solo i confermati... capire se vogliamo usare questi per le previsioni...
			prvData	<- readRDS(pathProv)
			reacval$mdataProv <- file.info(pathProv)$mtime
      reacval$dateRange_prv <- max(prvData$data)

      prvData <- prvData[!grepl("aggiornamento", prvData$denominazione_provincia),]
      prvData <- aggregate(list(totale_casi=prvData$totale_casi, pop=allDataConf$pop),
                        by=list(data=prvData$data, denominazione_regione=prvData$denominazione_regione,
                                denominazione_provincia=prvData$denominazione_provincia, codice_provincia=prvData$codice_provincia),
                        FUN=sum)
      prvData$`casi su 10mila abit` <- round(prvData$totale_casi / prvData$pop * 10000, 3)
      reacval$dataTables_prv <- prvData

      regDataFlt	<- readRDS(pathProv)
      regDataFlt <- regDataFlt[!grepl("aggiornamento", regDataFlt$denominazione_provincia),]
      regDataFlt <- aggregate(list(totale_casi=regDataFlt$totale_casi, pop=regDataFlt$pop),
                            by=list(data=regDataFlt$data,
                                    denominazione_regione=regDataFlt$denominazione_regione, codice_regione=regDataFlt$codice_regione),
                            FUN=sum)
      regDataFlt$`casi su 10mila abit` <- round(regDataFlt$totale_casi / regDataFlt$pop * 10000, 3)
      reacval$dataTables_reg_flt <- regDataFlt

      #assign("allData_prv",prvData,envir=.GlobalEnv)
      #assign("allData_reg_flt",regData,envir=.GlobalEnv)
		}
		if (file.info(pathReg)$mtime > isolate(reacval$mdataReg)) {
			regData <- readRDS(pathReg)
			reacval$dataTables_reg <- regData
			reacval$mdataReg <- file.info(pathReg)$mtime
			reacval$dateRange_reg <- max(regData$data)

			tsReg <- getTimeSeries(regData)
#			modelliIta <- list()
#			for(i in  1:length(campiPrevisioni)){
#				modelliIta<-loglinmodel2(tsReg$Italia, var="totale_casi", rangepesi=c(0,1))
#			}
#			names(modelliIta) <- campiPrevisioni
#			modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel2)

#################

			modelliIta <- list()
			modelliItaExp <- list()

			for(i in  1:length(campiPrevisioni)){
				modelliIta[[i]]<-loglinmodel3(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=TRUE)
				modelliItaExp[[i]]<-loglinmodel3(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=FALSE)

			}
			names(modelliIta) <- campiPrevisioni
			names(modelliItaExp) <- campiPrevisioni

			modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel3, quadratico=TRUE)
			modelliRegExp <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel3, quadratico=FALSE)
#################
			reacval$modelliIta 	<- modelliIta
			reacval$modelliReg 	<- modelliReg
			reacval$modelliItaExp 	<- modelliItaExp
			reacval$modelliRegExp 	<- modelliRegExp
			assign("allData_reg",regData,envir=.GlobalEnv)
			assign("modelliReg",modelliReg,envir=.GlobalEnv)
			assign("modelliIta",modelliIta,envir=.GlobalEnv)
			assign("modelliRegExp",modelliRegExp,envir=.GlobalEnv)
			assign("modelliItaExp",modelliItaExp,envir=.GlobalEnv)
		}
  })

  get_last_date <- reactive({
		if(verbose) cat("\n REACTIVE:get_last_date")
    strftime(max(reacval$dateRange_prv), format="%d-%m-%Y")
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

	if(verbose) cat("\n renderUI:updateRegUI")
  h4(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineRegioni <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegioni")
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg)) {
    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
    p <- ggplot(allDataReg) + my_ggtheme() +
          suppressWarnings(geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
            aes(x=data, y=`casi totali`, color=regione,
            text = paste('Regione:', regione, '<br>Data:', strftime(data, format="%d-%m-%Y"),
             '<br>Casi: ', `casi totali`)))) +
          scale_color_manual(values=d3hexcols20) +
          theme(axis.text.x=element_text(angle=45, hjust=1)) +
          labs(x="")
    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  }
})

output$tabRegioniNEW <- renderDT({
	if(verbose) cat("\n renderDT:tabRegioni")
  allDataReg <- reacval$dataTables_reg
  if (!is.null(allData)) {
    regrdx <- allDataReg[allDataReg$data==max(allDataReg$data), ]
		out <- regrdx[, c('denominazione_regione', 'tamponi', 'totale_casi', 'totale_ospedalizzati','terapia_intensiva','deceduti','dimessi_guariti')]
		names(out) <- c('regione', 'tamponi', 'casi', 'ospedalizzati', 'Terapia intensiva','deceduti','guariti')
 #   out$`casi su 10mila abit` <- round(out$totale_casi / out$pop * 10000, 3)

    datatable(out,
      selection = list(target = NULL),
      options= c(list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})

output$tabRegioni <- renderDT({
	if(verbose) cat("\n renderDT:tabRegioni")
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg)) {
    allDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    allDataReg$pop <- NULL
    allDataReg$data <- strftime(allDataReg$data, format="%d-%m-%Y")
    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))

    datatable(allDataReg,
      selection = list(target = NULL),
      options= c(list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})


output$selRegioni <- renderUI({
  allDataReg <- reacval$dataTables_reg_flt
  if (!is.null(allDataReg)) {
    fluidRow(sliderInput("giornoReg", "Giorno:",
              min = min(allDataReg$data) + 1, max = max(allDataReg$data),
              value = max(allDataReg$data), animate = animationOptions(interval = 1500), timeFormat="%b %d")
    )
  }
})


observe({
  myGiorno <- input$giornoReg
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg) & !is.null(myGiorno)) {
    allDataReg <- allDataReg[allDataReg$data==myGiorno,]
    allDataReg$totale_casi[allDataReg$totale_casi==0] <- NA_integer_
    pltRegioni <- merge(regioni, allDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")
    pal <- colorNumeric("YlOrRd", domain = log10(pltRegioni$totale_casi))
    leafletProxy(mapId="mapRegioni", data=pltRegioni) %>% clearShapes() %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
             label = ~paste(DEN_REG, "- casi:", totale_casi))
  }
})

output$mapRegioni <- renderLeaflet({
	if(verbose) cat("\n renderLeaflet:mapRegioni")
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg)) {
    allDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    pltRegioni <- merge(regioni, allDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")
    pal <- colorNumeric("YlOrRd", domain = log10(pltRegioni$totale_casi))

		suppressWarnings(leaflet(data = pltRegioni, options = leafletOptions(zoomControl = FALSE,minZoom = 3, maxZoom = 6)) %>%
			addTiles()%>%
			addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
      # i poligoni li mette l'observe sopra... se li mettiamo anche qui, sfarfalla all'avvio
			#addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity=.7,
		  #          label = ~paste(DEN_REG, "- casi:", totale_casi)) %>%
      addLegend(pal = pal, values = ~log10(totale_casi), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomleft',
                title = paste0("casi")))
  }
})

output$mapRegioniGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapRegioniGG")
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg)) {
    allDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    pltRegioni <- merge(regioni, allDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")

    # map_italia è definito una volta sola nel global
    map_italia +
      geom_sf(data = pltRegioni, aes(fill = totale_casi), color="black", size=.2) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Numero casi", trans = "log10") +
      geom_text(data = pltRegioni, aes(x=reg_long, y=reg_lat, label=paste(DEN_REG, "\n casi:", totale_casi)), cex=2.5, color="black", fontface = "bold")
  }
})


## PROVINCE

output$updatePrvUI <- renderUI({
	if(verbose) cat("\n renderUI:updatePrvUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineProvince <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineProvince")
  myReg <- input$regionSel
  allDataPrv <- copy(reacval$dataTables_prv)
  if (!is.null(allDataPrv) & !is.null(myReg)) {
    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
    setnames(allDataPrv, old=c('denominazione_provincia', 'totale_casi'), new=c('provincia', 'casi totali'))
    p <- ggplot(allDataPrv) + my_ggtheme() +
          suppressWarnings(geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
            aes(x=data, y=`casi totali`, color=provincia,
            text = paste('Provincia:', provincia, '<br>Data:', strftime(data, format="%d-%m-%Y"),
             '<br>Casi: ', `casi totali`)))) +
          scale_color_manual(values=d3hexcols20) +
          theme(axis.text.x=element_text(angle=45, hjust=1)) +
          labs(x="")
    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  }
})


output$tabProvince <- renderDT({
	if(verbose) cat("\n renderDT:tabProvince")
  myReg <- input$regionSel
  allDataPrv <- copy(reacval$dataTables_prv)

  if (!is.null(allDataPrv) & !is.null(myReg)) {
    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
    allDataPrv <- allDataPrv[allDataPrv$data==max(allDataPrv$data),]
    setnames(allDataPrv, old=c('denominazione_provincia', 'totale_casi'), new=c('provincia', 'casi totali'))
    allDataPrv$pop <- NULL
    allDataPrv$data <- strftime(allDataPrv$data, format="%d-%m-%Y")

    datatable(allDataPrv,
      selection = list(target = NULL),
      options= c(list(paging = F, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})


output$selProvince <- renderUI({
  allDataPrv <- reacval$dataTables_prv
  if (!is.null(allDataPrv)) {
    fluidRow(selectInput("regionSel", label="Seleziona regione", choices=regioniList, selected = "Lombardia"),
      sliderInput("giornoPrv", "Giorno:",
        min = min(allDataPrv$data) + 1, max = max(allDataPrv$data),
        value = max(allDataPrv$data), animate = animationOptions(interval = 1500), timeFormat="%b %d")
    )
  }
})


observe({
  myGiorno <- input$giornoPrv
  myReg <- isolate(input$regionSel)
  allDataPrv <- copy(reacval$dataTables_prv)

  if (!is.null(allDataPrv) & !is.null(myGiorno) & !is.null(myReg)) {
    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
    allDataPrv <- allDataPrv[allDataPrv$data == myGiorno,]
    allDataPrv$totale_casi[allDataPrv$totale_casi==0] <- NA_integer_
    pltProvince <- merge(province, allDataPrv[,c("codice_provincia", "totale_casi")], by.x="COD_PROV", by.y="codice_provincia")
    pal <- colorNumeric("YlOrRd", domain = log10(pmax(1,allDataPrv$totale_casi)))
    leafletProxy(mapId="mapProvince", data=pltProvince) %>% clearShapes() %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
             label = ~paste(DEN_UTS, "- casi:", totale_casi))
  }
})

output$mapProvince <- renderLeaflet({
	if(verbose) cat("\n renderLeaflet:mapProvince")
  myReg <- input$regionSel
  allDataPrv <- copy(reacval$dataTables_prv)

  if (!is.null(allDataPrv) & !is.null(myReg)) {
    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
    allDataPrv <- allDataPrv[allDataPrv$data == max(allDataPrv$data),]
    allDataPrv$totale_casi[allDataPrv$totale_casi==0] <- NA_integer_
    pltProvince <- merge(province, allDataPrv[,c("codice_provincia", "totale_casi")], by.x="COD_PROV", by.y="codice_provincia")
    my_frame <- st_drop_geometry(regioni[regioni$COD_REG == unique(pltProvince$COD_REG), c("reg_long", "reg_lat")])
    pal <- colorNumeric("YlOrRd", domain = log10(pmax(1,allDataPrv$totale_casi)))
    suppressWarnings(leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        # i poligoni li mette l'observe sopra... se li mettiamo anche qui, sfarfalla all'avvio
        #addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
        #         label = ~paste(DEN_UTS, "- casi:", totale_casi)) %>%
        addLegend(pal = pal, values = ~log10(pmax(1,allDataPrv$totale_casi)), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomright',
                title = paste0("casi")))

  }
})

output$mapProvinceGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapProvinceGG")
  myReg <- input$regionSel
  allDataPrv <- copy(reacval$dataTables_prv)

  if (!is.null(allDataPrv) & !is.null(myReg)) {
    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
    allDataPrv <- allDataPrv[allDataPrv$data==max(allDataPrv$data),]
    pltProvince <- merge(province, allDataPrv[,c("codice_provincia", "totale_casi")], by.x="COD_PROV", by.y="codice_provincia")

    # map_regioni è definita una volta sola nel global
    map_regioni[[unique(pltProvince$COD_REG)]] +
      geom_sf(data = pltProvince, aes(fill = totale_casi), color="black", size=.2) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Numero casi", trans = "log10") +
      geom_text(data = pltProvince, aes(x=prv_long, y=prv_lat, label=paste(DEN_UTS, "\n casi:", totale_casi)), cex=2.5, color="black", fontface = "bold")
  }
})

getTimeSeriesReact <- reactive({
	if(verbose) cat("\n reactive:getTimeSeriesReact")
	allDataReg <- copy(reacval$dataTables_reg)
	if (!is.null(allDataReg)) {
		tstot <- getTimeSeries(allDataReg)
		if(saveRDSout) saveRDS(file="tstotOut.RDS",tstot)
		tstot
  }
})

## PREVISIONI
##

output$updatePrevisioniUI <- renderUI({
	if(verbose) cat("\n renderUI:updatePrevisioniUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})


output$updateTIUI <- renderUI({
	if(verbose) cat("\n renderUI:updateTIUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})




prevRegion <- reactive({
	if(verbose) cat("\n reactive:prevRegion")
	allDataReg <- copy(reacval$dataTables_reg)
  tipoModello <- input$modelloFit
	nahead=3
	cat("\ttipoModello:", tipoModello)

	if(is.null(tipoModello)) return(NULL)
	if(tipoModello=="Esp. quadratico"){
		modelliReg=isolate(reacval$modelliReg)
	} else modelliReg <- isolate(reacval$modelliRegExp)


	if (!is.null(allDataReg)) {
		tsReg <- getTimeSeriesReact()
    tsReg["Italia"] <- NULL
		if(saveRDSout) saveRDS(file="prevRegionList.RDS",list(tsReg, modelliReg, allDataReg))

		prevDT <- get_predictions(modelliReg, tsReg, nahead=nahead, alldates=TRUE)
    setnames(prevDT, old=c("outName"), new=c("regione"))
		setDF(prevDT)
		prevDT[,c("dataind","data2")]<-NULL
		prevDT
  }
})


output$fitRegion <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegion")
  allDataReg <- copy(reacval$dataTables_reg)
	regioniSel <- input$regionSelFit
  tipoGraph <- input$regionLinLogFit

  if (!is.null(allDataReg) & length(regioniSel) > 0) {
		tsReg <- getTimeSeriesReact()[which(names(getTimeSeriesReact())%in%regioniSel)]
		if(saveRDSout) saveRDS(file="fitRegionList.RDS",list(tsReg, allDataReg))

		prevDT <- copy(prevRegion())
		setnames(prevDT, old=c('Attesi'), new=c('casi totali'))
    prevDT <- prevDT[which(prevDT$regione%in%regioniSel),]
    assign("prevDTvedi",prevDT, envir=.GlobalEnv)

    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
		setDF(allDataReg)
    allDataReg <- allDataReg[which(allDataReg$regione%in%regioniSel),]

    ## attenzione ai compromessi tra ggplot & plotly...
    ## * per avere geom_line funzionante e i tooltip basati su testo
    ##   dobbiamo aggiungere group=1 e aes text, ma questi rompono ggplot
    ## * aggiungendo errorbar (che ha bisogno di aes y, anche se ggplot no) o ribbon,
    ##   invece, rompiamo la legenda che non riesce più a mettere simboli e nomi giusti
    ## * notare che una eventuale errorbar va messa prima delle altre geometrie,
    ##   altrimenti copre i tooltip
    ## parte di questo problema sembra cosa nota https://github.com/ropensci/plotly/issues/1164
    ## e non legata a 'ggplotly'
    ## per ora metto gli intervalli di previsione nei tooltip
    datamax<-max(prevDT$data)
		p <- ggplot() + my_ggtheme() +
       #geom_errorbar(data=prevDT, aes(x=data, y=`casi totali`, ymin=LowerRange, ymax=UpperRange, color=regione), width=0.1) +
       #geom_ribbon(data=prevDT, aes(x=data, y=`casi totali`, ymin=LowerRange, ymax=UpperRange, fill=regione), alpha=.35, guides=F) +
       #scale_fill_manual(values=d3hexcols20) +
       suppressWarnings(geom_line(data=prevDT, lty=2, group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
           aes(x=data, y=`casi totali`, color=regione,
             text = paste('Data:', strftime(data, format="%d-%m-%Y"),
              '<br>Regione: ', regione,
              '<br>Casi (fit): ', round(`casi totali`),
              '<br>Intervallo previsione:', paste0('[', round(LowerRange,2), ', ', round(UpperRange,2),']')
              )))) +
       suppressWarnings(geom_point(data=prevDT[which(prevDT$data>=datamax-2),], shape=22,
           aes(x=data, y=`casi totali`, color=regione,
              text = paste('Data:', strftime(data, format="%d-%m-%Y"),
              '<br>Regione: ', regione,
              '<br>Casi (fit): ', round(`casi totali`),
              '<br>Intervallo previsione:', paste0('[', round(LowerRange,2), ', ', round(UpperRange,2),']')
            )))) +
       suppressWarnings(geom_point(data=allDataReg, aes(x=data, y=`casi totali`, color=regione,
             text = paste('Data:', strftime(data, format="%d-%m-%Y"),
              '<br>Regione: ', regione,
              '<br>Casi: ', `casi totali`
              )))) +
       scale_color_manual(values=d3hexcols20) + scale_x_date(date_breaks="2 day",date_labels="%b %d") +
			 theme(axis.text.x=element_text(angle=45,hjust=1)) +
       labs(x="")

    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  }
})


prevIta <- reactive({
	if(verbose) cat("\n reactive:prevIta")
	allDataReg <- copy(reacval$dataTables_reg)
	tipoModello <- input$modelloFit
	nahead=3
	cat("\ttipoModello:", tipoModello)

	if(is.null(tipoModello)) return(NULL)
	if(tipoModello=="Esp. quadratico"){
		modelliIta=isolate(reacval$modelliIta)
	} else modelliIta <- isolate(reacval$modelliItaExp)

	if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()["Italia"]
		if(saveRDSout) saveRDS(file="prevItaList.RDS",list(tsIta, modelliIta, allDataReg))

    prevDT <- get_predictions(modelliIta, tsIta, nahead=nahead, alldates=TRUE)
    setnames(prevDT, old=c("outName"), new=c("variabilePrevista"))
    setDF(prevDT)
    prevDT[,c("dataind","data2")]<-NULL
		if(assignout) assign("prevItaVar",list(tsIta=tsIta, modelliIta=modelliIta, nahead=nahead), envir=.GlobalEnv)
		prevDT
  }
})



output$fitIta <- renderPlotly({
if(verbose) cat("\n renderPlotly:fitIta")
  allDataReg <- copy(reacval$dataTables_reg)
  tipoGraph <- input$regionLinLogFit

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()$Italia
		if(saveRDSout) saveRDS(file="fitItaList.RDS",list(tsIta, allDataReg))
		prevItaDT <-copy(prevIta())
		setnames(prevItaDT, old=c('Attesi'), new=c('casi'))

		setDF(allDataReg)
		varPrev <- unique(prevItaDT$variabilePrevista)

		dataRDX <- allDataReg[, c('data', varPrev)]
		dataIta <- aggregate(dataRDX[,2:5], sum, by=list(data=dataRDX$data))

    tmp <- data.frame(
    		data=rep(unique(dataIta$data), times=length(varPrev)),
    		casi=unlist(lapply(2:5, function(x) dataIta[, x])),
    		variabilePrevista=rep(unique(varPrev), each=nrow(dataIta))
    )

    ## attenzione ai compromessi tra ggplot & plotly...
    ## * per avere geom_line funzionante e i tooltip basati su testo
    ##   dobbiamo aggiungere group=1 e aes text, ma questi rompono ggplot
    ## * aggiungendo errorbar (che ha bisogno di aes y, anche se ggplot no) o ribbon,
    ##   invece, rompiamo la legenda che non riesce più a mettere simboli e nomi giusti
    ## * notare che una eventuale errorbar va messa prima delle altre geometrie,
    ##   altrimenti copre i tooltip
    ## parte di questo problema sembra cosa nota https://github.com/ropensci/plotly/issues/1164
    ## e non legata a 'ggplotly'
    ## per ora metto gli intervalli di previsione nei tooltip
    datamax<-max(prevItaDT$data)
		p <- ggplot() + my_ggtheme() +
       #geom_errorbar(data=prevDT, aes(x=data, y=`casi totali`, ymin=LowerRange, ymax=UpperRange, color=regione), width=0.1) +
       #geom_ribbon(data=prevDT, aes(x=data, y=`casi totali`, ymin=LowerRange, ymax=UpperRange, fill=regione), alpha=.35, guides=F) +
       #scale_fill_manual(values=d3hexcols20) +
			 suppressWarnings(geom_line(data=prevItaDT, linetype=2, group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
              aes(x=data, y=casi, color=variabilePrevista,
                text = paste('Data:', strftime(data, format="%d-%m-%Y"),
                 '<br>Variabile: ', variabilePrevista,
                 '<br>Casi (fit): ', round(casi),
                 '<br>Intervallo previsione:', paste0('[', round(LowerRange,2), ', ', round(UpperRange,2),']')
                 )))) +
       suppressWarnings(geom_point(data=prevItaDT[which(prevItaDT$data>=datamax-2),], shape=22,
                     aes(x=data, y=`casi`, color=variabilePrevista,
                        text = paste('Data:', strftime(data, format="%d-%m-%Y"),
                        '<br>Variabile: ', variabilePrevista,
                        '<br>Casi (fit): ', round(`casi`),
                        '<br>Intervallo previsione:', paste0('[', round(LowerRange,2), ', ', round(UpperRange,2),']')
                      )))) +
			 suppressWarnings(geom_point(data=tmp,
              aes(x=data, y=casi, color=variabilePrevista,
                text = paste('Data:', strftime(data, format="%d-%m-%Y"),
                 '<br>Variabile: ', variabilePrevista,
                 '<br>Casi: ', casi
                 )))) +
			 scale_color_manual(values=d3hexcols20) + scale_x_date(date_breaks="2 day",date_labels="%b %d") +
       theme(axis.text.x = element_text(angle=45,hjust=1)) +
       labs(x="", color = "variabile prevista")

    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  }
})


prevItaLongTerm <- reactive({
	if(verbose) cat("\n reactive:prevIta")
	allDataReg <- copy(reacval$dataTables_reg)
	nahead=10

	modelliIta=isolate(reacval$modelliIta)
	if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()["Italia"]

    prevDT <- get_predictions(modelliIta, tsIta, nahead=nahead, alldates=F)
    setnames(prevDT, old=c("outName"), new=c("variabilePrevista"))
    setDF(prevDT)
		prevDT[prevDT$variabilePrevista == "totale_casi",]
  }
})


output$fitCasesIta <- renderPlotly({
  if(verbose) cat("\n renderPlotly:fitCasesIta")
  prevItaDT <- copy(prevItaLongTerm())
  tsIta <- copy(getTimeSeriesReact()[["Italia"]])

  if (!is.null(prevItaDT) & !is.null(tsIta)) {
  		setnames(prevItaDT, old=c('Attesi'), new=c('casi'))
      setnames(tsIta, old=c('totale_casi'), new=c('casi'))
      num_rows <- nrow(tsIta)
      datiIta <- rbind(tsIta[, c("data", "casi")], prevItaDT[, c("data", "casi")])
      datiIta$tipo <- c(rep("osservati", num_rows), rep("predetti", nrow(datiIta) - num_rows))
      datiIta$tipo <- factor(datiIta$tipo, levels=c("osservati", "predetti"))

			indmax <- which.max(datiIta$casi)
      vdate <- datiIta$data[indmax]
		  datiIta <-datiIta[datiIta$data <= vdate,]
      datiIta$label <- c(rep("", nrow(datiIta)-1), "picco\n previsto")

      p <- ggplot() + my_ggtheme() +
  					suppressWarnings(geom_bar(data=datiIta, aes(x=data, y=casi, fill=tipo,
              text = paste('Data:', strftime(data, format="%d-%m-%Y"),
               '<br>Casi: ', round(casi))), stat="identity", width = 0.8))+
  #          geom_text(data=datiIta, aes(x=data, y=casi, label=label), cex=2.5, color="black", fontface = "bold") +
  					scale_fill_manual(values=d3hexcols) +#scale_x_date(date_breaks="2 day",date_labels="%b %d")+
            theme(axis.text.x=element_text(angle=45,hjust=1)) +
            labs(x="") +
            theme(legend.title = element_blank())
      ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
   }
})


terapiaInt <- reactive({
	if(verbose) cat("\n reactive:terapiaInt")
	datamax <- isolate(reacval$dateRange_reg[2])
  allDataReg <- copy(reacval$dataTables_reg)
	tint <- merge(allDataReg[(allDataReg$data==datamax), c('denominazione_regione', 'terapia_intensiva')], Tintensiva, by="denominazione_regione")
	tint$percTI <- round(tint$terapia_intensiva/tint$lettiTI*100)
	tint
})

output$terapiaIntPlotPercNow<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntPlot")

	tint <- terapiaInt()
	if(is.null(tint)) return(NULL)
	p <- ggplot(data=tint, aes(x=denominazione_regione, y=percTI,
                text = paste('Regione:', denominazione_regione,
                  '<br>Percentuale: ', round(percTI)))) +
          geom_bar(stat="identity", fill="steelblue") + my_ggtheme() +
	        theme(axis.text.x=element_text(angle=45,hjust=1))+
          labs(x="", y="% letti occupati per CoVid19")
	ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  if(reacval$mobile){
    p<-p+coord_flip()
  }
  p

})

output$terapiaIntPlotNow<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntPlot")

	tint <- terapiaInt()
	if(is.null(tint)) return(NULL)

	tintLong <- data.frame(rep(tint$denominazione_regione, 2), c(tint$terapia_intensiva,tint$lettiTI) )
	names(tintLong) <- c("regione", "numero")
	tintLong$dati <- rep(c('pazienti CoVid19', 'letti disponibili'), each=nrow(tint))


	p <-ggplot(data=tintLong, aes(x=regione, y=numero, fill=dati,
                text = paste0('Regione: ', regione,
                  '<br>numero ', dati, ": ", numero))) +
        geom_bar(stat="identity", position=position_dodge())+my_ggtheme() +
	      theme(axis.text.x=element_text(angle=45,hjust=1))+
        scale_fill_manual(values=d3hexcols) +
        labs(x="", y="numero letti")
  ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  if(reacval$mobile){
    p<- p+coord_flip()
  }
  p
})

output$terapiaIntPlotPercPrev<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntPlot")

	tint <- terapiaInt()
	allDataReg <- copy(reacval$dataTables_reg)
	prevDT <-copy(prevRegion())
	if(is.null(tint)) return(NULL)
	if(is.null(allDataReg)) return(NULL)
	if(is.null(prevDT)) return(NULL)

	totitalia<-aggregate( allDataReg[,c('totale_casi', 'terapia_intensiva')],by=list(data=allDataReg$data), sum)
	totitalia$perc <-totitalia$terapia_intensiva/totitalia$totale_casi
	percTI <-totitalia[which.max(totitalia$data), 'perc']

	nahead <-3
	oggi <- isolate(reacval$dateRange_reg)[2]

  prevFin <- prevDT[between(prevDT$data,oggi+1,oggi+nahead),]
	prevFin$Attesi 		<-round(prevFin$Attesi*percTI)

	prevFin$UpperRange	<-prevFin$UpperRange*percTI
	prevFin$LowerRange	<-prevFin$LowerRange*percTI
	prevFin$data <- strftime(prevFin$data, format="%d-%m-%Y")
  prevFin[,c("dataind","data2")]<-NULL
  prevFin$ttip <- paste('Data:', prevFin$data,
          '<br>Regione:', prevFin$regione,
          '<br>Ricover attesi:', round(prevFin$Attesi),
          '<br>Intervallo previsione:', paste0('[', round(prevFin$LowerRange,2), ', ', round(prevFin$UpperRange,2),']')
        )

	Ntint <- nrow(tint)
  postiLetto <- data.frame(data=rep("posti disponibili", Ntint), Attesi= tint$lettiTI,
                UpperRange=rep(0,Ntint), LowerRange=rep(0,Ntint),
                regione=tint$denominazione_regione, stringsAsFactors=F)
  postiLetto$ttip <- paste0('Regione: ', postiLetto$regione,
          '<br>Posti disponibili: ', round(postiLetto$Attesi))

	out <- rbind(prevFin, postiLetto)
  out$data <- factor(out$data, levels=c(unique(prevFin$data[order(strptime(prevFin$data, format="%d-%m-%Y"))]), "posti disponibili"))
	p <-ggplot(data=out, aes(x=regione, y=Attesi, fill=data,
                text = ttip)) +
        geom_bar(stat="identity", position=position_dodge()) + my_ggtheme() +
	      theme(axis.text.x=element_text(angle=45,hjust=1)) +
	      geom_errorbar(aes(ymin=LowerRange, ymax=UpperRange), width=.2, position=position_dodge(.9))+
        scale_fill_manual(values=d3hexcols) +
        labs(x="", y="numero letti", fill="")
  ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  if(reacval$mobile){
    p<-p+coord_flip()
  }
  p



})

prevRegionCompare <- reactive({
	if(verbose) cat("\n reactive:prevRegion")
	allDataReg <- copy(reacval$dataTables_reg)
  tipoModello <- input$modelloFit


	if(is.null(tipoModello)) return(NULL)
	if(tipoModello=="Esp. quadratico"){
		modelliReg=isolate(reacval$modelliReg)
	} else modelliReg <- isolate(reacval$modelliRegExp)


	if (!is.null(allDataReg)) {
		tsReg <- getTimeSeriesReact()
    tsReg["Italia"] <- NULL
		if(saveRDSout) saveRDS(file="prevRegionList.RDS",list(tsReg, modelliReg, allDataReg))

		prevDT <- get_predictions(modelliReg, tsReg, nahead=nahead, alldates=TRUE)
    setnames(prevDT, old=c("outName"), new=c("regione"))
		setDF(prevDT)
		prevDT[,c("dataind","data2")]<-NULL
		prevDT
  }
})

output$dateCompare <- renderUI({
	if(verbose) cat("\n renderUI:dateCompare")
	files <- list.files("www/pastModels/")
	dateTmp <- gsub("modelliIta_|modelliItaExp_|.RDS|modelliReg_|modelliRegExp_", "", files)
	date <- as.Date(unique(dateTmp))
	date <- sort(date[-which.max(date)])
	selectizeInput("dataComparazione", label="Data Modello da comparare", choices=date, selected = max(date))

})



# compara previsioni
prevItaCompare <- reactive({
	if(verbose) cat("\n reactive:prevItaCompare")
	inpData <- input$dataComparazione
	tipoVariazione <-input$tipoCompare
		if(verbose) cat("\n inpData", inpData)


	if(is.null(inpData)) return(NULL)
	if(is.null(tipoVariazione)) return(NULL)


	dataMod <-as.Date(inpData)

	tsIta <- getTimeSeriesReact()["Italia"]

	assign("tsIta",tsIta, envir=.GlobalEnv)
	assign("dataMod",dataMod, envir=.GlobalEnv)

	modelliItaPast <- readRDS(paste0("www/pastModels/modelliIta_", dataMod, ".RDS"))
	modelliItaExpPast  <- readRDS(paste0("www/pastModels/modelliItaExp_", dataMod, ".RDS"))

	if(tipoVariazione=='Totale'){

		vero <-tsIta$Italia[tsIta$Italia$data==(dataMod+1), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')]

		tsIta$Italia <- tsIta$Italia[ tsIta$Italia$data<=dataMod, ]

	  prevDT <- get_predictions(modelliIta, tsIta, nahead=1, alldates=FALSE)
		prevDTexp <- get_predictions(modelliItaExp, tsIta, nahead=1, alldates=FALSE)
		prevDT$Modello 		<- "Esp. quadratico"
		prevDTexp$Modello <- "Esponenziale"

		prevDT$Osservato 		<- unlist(vero)
		prevDTexp$Osservato  <- unlist(vero)

		prevDT$'Confidence Level68%' 		<- paste0(format(prevDT$LowerRange, big.mark="'"), ' - ', format(prevDT$UpperRange, big.mark="'"))
		prevDTexp$'Confidence Level68%'  <- paste0(format(prevDTexp$LowerRange, big.mark="'"), ' - ', format(prevDTexp$UpperRange, big.mark="'"))

		setnames(prevDT, old=c('LowerRange', 'UpperRange', 'outName', 'Attesi'), new=c('Minimo', 'Massimo', 'Variabile', 'Previsto'))
		setnames(prevDTexp, old=c('LowerRange', 'UpperRange', 'outName', 'Attesi'), new=c('Minimo', 'Massimo', 'Variabile', 'Previsto'))

		out <-rbind(prevDT[, c('data', 'Modello', 'Variabile', 'Confidence Level68%', 'Previsto', 'Osservato')],
					prevDTexp[, c('data', 'Modello', 'Variabile', 'Confidence Level68%', 'Previsto',  'Osservato')])

		out$Variazione <- paste0 (round((out$Osservato-out$Previsto)/out$Osservato*100, 2), " %")
		out$Previsto 		<- format(out$Previsto, big.mark="'")
		out$Osservato 		<- format(out$Osservato, big.mark="'")

		out

	} else {
		veroMod <-unlist(tsIta$Italia[tsIta$Italia$data==(dataMod), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')])
		vero <-unlist(tsIta$Italia[tsIta$Italia$data==(dataMod+1), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')])

		DVero <- vero - veroMod

		tsIta$Italia <- tsIta$Italia[ tsIta$Italia$data<=dataMod, ]

	  prevDT <- get_predictions(modelliIta, tsIta, nahead=1, alldates=FALSE)
		prevDTexp <- get_predictions(modelliItaExp, tsIta, nahead=1, alldates=FALSE)
		prevDT$Modello 		<- "Esp. quadratico"
		prevDTexp$Modello <- "Esponenziale"

		prevDT$Osservato 		<- unlist(DVero)
		prevDTexp$Osservato  <- unlist(DVero)

		prevDT$VarPrev 			<- prevDT$Attesi - veroMod
		prevDTexp$VarPrev  	<- prevDTexp$Attesi - veroMod

		prevDT$VarMin 			<- prevDT$LowerRange - veroMod
		prevDTexp$VarMin  	<- prevDTexp$LowerRange - veroMod

		prevDT$VarMax    <- prevDT$UpperRange - veroMod
		prevDTexp$VarMax <- prevDTexp$UpperRange - veroMod

		prevDT$Variazione    <- prevDT$VarPrev - DVero
		prevDTexp$Variazione <- prevDTexp$VarPrev - DVero

		prevDT$'Confidence Level68%' 		<- paste0(format(prevDT$VarMin, big.mark="'"), ' - ', format(prevDT$VarMax, big.mark="'"))
		prevDTexp$'Confidence Level68%'  <- paste0(format(prevDTexp$VarMin, big.mark="'"), ' - ', format(prevDTexp$VarMax, big.mark="'"))

		setnames(prevDT, old=c('VarMin', 'VarMax', 'outName', 'VarPrev'), new=c('Minimo', 'Massimo', 'Variabile', 'Previsto'))
		setnames(prevDTexp, old=c('VarMin', 'VarMax', 'outName', 'VarPrev'), new=c('Minimo', 'Massimo', 'Variabile', 'Previsto'))

		outPerc <-rbind(prevDT[, c('data', 'Modello', 'Variabile', 'Confidence Level68%', 'Previsto', 'Osservato')],
					prevDTexp[, c('data', 'Modello', 'Variabile', 'Confidence Level68%', 'Previsto',  'Osservato')])

		outPerc$Variazione <- paste0 (round((outPerc$Osservato-outPerc$Previsto)/outPerc$Osservato*100, 2), " %")

		outPerc$Previsto 		<- format(outPerc$Previsto, big.mark="'")
		outPerc$Osservato 		<- format(outPerc$Osservato, big.mark="'")

		outPerc
	}


})


output$tabCompare <- renderDT({
	if(verbose) cat("\n renderDT:tabCompare")
  out <- prevItaCompare()


  if (!is.null(out)) {
		out <- out[order(out$Variabile),]
    datatable(out,extensions = c('Scroller'),
      selection = list(target = NULL),
      options= c(list(dom = 't',scroller=T,scrollX="300",scrollY="300",paging = T, searching = F, info=F, ordering=F, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})





output$tab_desktop<-renderUI({

  fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
  	h1("Analisi previsionale nelle province italiane"),
  	fluidRow(
  		column(12,h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione Descrizione Modelli"))

  	),

  		 br(),
  		fluidRow(style="padding:30px;background-color:#ffffff",
  			column(2,fluidRow(selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),
				radioButtons("modelloFit", label="Tipologia Modello", choices=c("Esp. quadratico","Esponenziale"), selected="Esp. quadratico"),
				checkboxGroupInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit)),
  		column(10,

  			fluidRow(column(6,align="center",h4("Andamento casi positivi per regione con previsione a 3 giorni")),column(6,align="center",h4("Andamenti globali in Italia con previsione a 3 giorni"))),
  			fluidRow(
  				column(width=6,align="left", plotlyOutput(outputId="fitRegion"), #spiegaFitPos
  				),br(),
  				column(width=6,align="left",plotlyOutput(outputId="fitIta"), #spiegaFitTot
  				),br(),fluidRow(style="padding:20px;",spiegaFitTotePos)
  			)
      )
  		),br(),br(),
  		fluidRow(style="padding:30px;background-color:#ffffff",width=12,  h2("Previsione del numero di casi totali a medio termine con modello esponenziale quadratico"), plotlyOutput(outputId="fitCasesIta")

  		),br(),
      fluidRow(
        box(width=12,
          column(width=4,
            selectizeInput("tipoCompare", label="Tipo Comparazione", choices=c("Totale", "Incremento Giornaliero"), , selected = "Lineare")
          ),
          column(width=4,
            uiOutput("dateCompare")
          ),
          DTOutput("tabCompare"),spiegaTabellaCompare

        )
      ),br(),
  	)

  })

  output$tab_mobile<-renderUI({

    fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
    	h1("Previsioni"),
    	fluidRow(
    		column(12,h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione Descrizione Modelli"))

    	),
    		 br(),
    		fluidRow(style="padding:30px;background-color:#ffffff",
        fluidRow(

          column(5,pickerInput(inputId = "regionSelFit", label = "Seleziona regioni", choices = regioniList,selected=regioni2fit, options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = TRUE)),
          column(4,selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),
          column(2,radioButtons("modelloFit", label="Tipologia Modello", choices=c("Esp. quadratico", "Esponenziale" ), selected="Esp. quadratico"))),

          fluidRow(align="center",h4("Andamento casi positivi per regione con previsione a 3 giorni")),
           plotlyOutput(outputId="fitRegion"), spiegaFitPos
          ),br(),
          fluidRow(style="padding:30px;background-color:#ffffff",
          fluidRow(align="center",h4("Andamenti globali in Italia con previsione a 3 giorni")),
           plotlyOutput(outputId="fitIta"), spiegaFitTot

    		),br(),br(),
    		fluidRow(style="padding:30px;background-color:#ffffff",width=12,  h2("Previsione del numero di casi totali a medio termine con modello esponenziale quadratico"), plotlyOutput(outputId="fitCasesIta")

    		),br(),
        fluidRow(
          box(width=12,
            column(width=4,
              selectizeInput("tipoCompare", label="Tipo Comparazione", choices=c("Totale", "Incremento Giornaliero"), , selected = "Lineare")
            ),
            column(width=4,
              uiOutput("dateCompare")
            ),
            DTOutput("tabCompare"),spiegaTabellaCompare

          )
        ),br(),
    	)

    })

output$spaces_mobile_prev<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br())
    }

  }
  })

output$spaces_mobile_intro<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br())
    }

  }
  })

output$spaces_mobile_ti<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br())
    }

  }
  })

output$spaces_mobile_reg<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br())
    }

  }
  })

output$spaces_mobile_prov<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br())
    }

  }

  })

  output$sidebar <- renderUI({
    out<-NULL
    if((length(reacval$mobile)>0)){
      if(reacval$mobile){
        out<-list(br(),br(),br())
      }
    }

       })




})
