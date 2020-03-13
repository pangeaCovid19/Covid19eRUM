
shinyServer(function(input, output, session) {

## AGGIORNAMENTI

  autoInvalidate <- reactiveTimer(3600000)

  reacval<-reactiveValues(
						fileList=list.files(dir_prov, full.names=T),
            oldList=NULL,
            dataTables=allData,
            dateRange=date_range,
						mdataProv=mtimeProv,
						fileList_reg=list.files(dir_reg, full.names=T),
            oldList_reg=NULL,
            dataTables_reg=allData_reg,
            dateRange_reg=date_range_reg,
						mdataReg=mtimeReg,
						modelliIta=modelliIta,
						modelliReg=modelliReg
					)

  observe({
		if(verbose) cat("\n OBSERVE:leggiDati")
    autoInvalidate()
		pathProv 	<- paste0(dir_prov,provRDS)
		pathReg 	<- paste0(dir_reg,regRDS)
		if( file.info(pathProv)$mtime > isolate(reacval$mdataProv)  ) {
			dataProv	<- readRDS(pathProv)
			reacval$dataTables  <- dataProv
			reacval$mdataProv 	<- file.info(pathProv)$mtime
			assign("allData",dataProv,envir=.GlobalEnv)
		}
		if( file.info(pathReg)$mtime > isolate(reacval$mdataReg)  ) {
			regData <- readRDS(pathReg)
			reacval$dataTables_reg<- regData
			reacval$mdataReg <- file.info(pathReg)$mtime

			tsReg <- getTimeSeries(regData)
			modelliIta <- list()
			for(i in  1:length(campiPrevisioni)){
				modelliIta<-loglinmodel2(tsReg$Italia, var="totale_casi", rangepesi=c(0,1))
			}
			names(modelliIta) <- campiPrevisioni
			modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel2)

			reacval$modelliIta 	<- modelliIta
			reacval$modelliReg 	<- modelliReg
			assign("allData_reg",regData,envir=.GlobalEnv)
			assign("modelliReg",modelliReg,envir=.GlobalEnv)
			assign("modelliIta",modelliIta,envir=.GlobalEnv)

		}



		#assign("resNew", list(reg=reacval$dataTables_reg, prov=reacval$dataTables), envir=.GlobalEnv)
  })

  get_data <- reactive({
		if(verbose) cat("\n REACTIVE:getDATA")
    res <- reacval$dataTables
    if (!is.null(input$drangeSel)) res <- res[res$data >= input$drangeSel[1] & res$data <= input$drangeSel[2],]
    res
  })


	  get_data_reg <- reactive({
			if(verbose) cat("\n REACTIVE:getDATA")
	    res <- reacval$dataTables_reg
	    if (!is.null(input$drangeSel)) res <- res[res$data >= input$drangeSel[1] & res$data <= input$drangeSel[2],]
	    res
	  })

  get_last_date <- reactive({
		if(verbose) cat("\n REACTIVE:get_last_date")
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
	if(verbose) cat("\n renderUI:updateRegUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineRegion <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegion")
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
	if(verbose) cat("\n renderDT:tabRegion")
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
	if(verbose) cat("\n renderLeaflet:mapRegion")

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

		leaflet(data = pltRegioni, options = leafletOptions(zoomControl = FALSE,minZoom = 5, maxZoom = 5)) %>%
			addTiles()%>%
			addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
			addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
		  label = ~paste(DEN_REG, "- casi:", totale_casi))
#    leaflet(data = pltRegioni) %>% addTiles() %>%
 #       addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
  #      addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
   #              label = ~paste(DEN_REG, "- casi:", totale_casi))
  }
})

output$mapRegionGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapRegionGG")
  allData <- get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione, codice_regione=allDataConf$codice_regione),
                      FUN=sum)
    latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    latestDataReg$densita_casi <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
    pltRegioni <- merge(regioni, latestDataReg[,c("codice_regione", "totale_casi", "densita_casi")], by.x="COD_REG", by.y="codice_regione")

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
	if(verbose) cat("\n renderDT:tabProvince")
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
	if(verbose) cat("\n renderLeaflet:mapProvince")
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
    leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 5, maxZoom = 5)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
                 label = ~paste(DEN_UTS, "- casi:", totale_casi))

  }
})

output$mapProvinceGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapProvinceGG")
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

    # map_regioni è definita una volta sola nel global
    map_regioni[[unique(pltProvince$COD_REG)]] +
      geom_sf(data = pltProvince, aes(fill = totale_casi), color="black", size=.2) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Numero casi", trans = "log10") +
      geom_text(data = pltProvince, aes(x=prv_long, y=prv_lat, label=paste(DEN_UTS, "\n casi:", totale_casi)), cex=2.5, color="black", fontface = "bold")

  }
})


## PREVISIONI

output$updatePrevisioniUI <- renderUI({
	if(verbose) cat("\n renderUI:updatePrevisioniUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

prevRegion <- reactive({
	if(verbose) cat("\n reactive:prevRegion")
	allDataReg <- copy( reacval$dataTables_reg)
	modelliReg <- isolate(reacval$modelliReg)
	nahead=3

	if (!is.null(allDataReg)) {
		tsReg <- getTimeSeries(allDataReg)
		tsReg <- tsReg[which(names(tsReg)!="Italia")]
		prev <- mapply(FUN=predictNextDays, tsReg, modelliReg, nahead=nahead, SIMPLIFY=F)

		prevDT <- rbindlist(prev)
		prevDT[, regione:=rep(names(prev), each=nahead)]
		setDF(prevDT)
  }
})


output$fitRegion <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegion")
  allDataReg <- copy( reacval$dataTables_reg)

  if (!is.null(allDataReg)) {
		tsReg <- getTimeSeries(allDataReg)
		tsReg <- tsReg[which(names(tsReg)!="Italia")]

		prevDT <-copy(prevRegion())
		setnames(prevDT, old=c('Attesi'), new=c('casi totali'))

    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
		setDF(allDataReg)

		out <- rbind(allDataReg[, c('regione', 'casi totali', "data")], prevDT[, c('regione', 'casi totali', "data")])
 #   colnames(allDataReg)[2] <- "regione"
 #   colnames(allDataReg)[3] <- "casi totali"
    p <- ggplot(out) + my_ggtheme() +
          geom_line(aes(x=data, y=`casi totali`, color=regione)) +
          scale_color_manual(values=d3hexcols20)
    p
  }
})



output$fitRegLog <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegLog")
  allDataReg <- copy( reacval$dataTables_reg)
	modelliReg <- isolate(reacval$modelliReg)

  if (!is.null(allData)) {
		tsReg <- getTimeSeries(allDataReg)
		tsReg <- tsReg[which(names(tsReg)!="Italia")]

		prevDT <-copy(prevRegion())
		setnames(prevDT, old=c('Attesi'), new=c('casi totali'))

    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
		setDF(allDataReg)

		out <- rbind(allDataReg[, c('regione', 'casi totali', "data")], prevDT[, c('regione', 'casi totali', "data")])
		out[,'casi totali' ] <- log(out[,'casi totali' ] )

		fit <- as.data.frame(do.call(rbind, lapply(modelliReg, function(x) x$coefficients)))
		names(fit[,1]) <- "int"

    p <- ggplot(out) + my_ggtheme() +
					geom_point(aes(x=data, y=`casi totali`, color=regione))+
#					geom_abline(data=fit, mapping=aes(slope=data, intercept='int')) +
					geom_abline(slope=fit['Lombardia',2], intercept=fit['Lombardia',1])+
					geom_abline(slope=fit['Veneto',2], intercept=fit['Veneto',1])+
					geom_abline(slope=fit['Emilia Romagna',2], intercept=fit['Emilia Romagna',1])+
					#geom_abline(data=d,   mapping=aes(slope=s, intercept=ic, linetype=factor(s))) +
					scale_color_manual(values=d3hexcols20)
    p
  }
})


prevIta <- reactive({
	if(verbose) cat("\n reactive:prevIta")
	allDataReg <- copy( reacval$dataTables_reg)
	modelliIta <- isolate(reacval$modelliIta)
	nahead=3

	if (!is.null(allDataReg)) {
		tsIta <- getTimeSeries(allDataReg)$Italia
		prevIta <- lapply(modelliIta, function(modello, dati, nahead){
			predictNextDays(dati=dati, modello=modello,nahead=nahead)
		},dati=tsIta, nahead=nahead)

		prevItaDT <- rbindlist(prevIta)
		prevItaDT[, variabilePrevista:=rep(names(prevIta), each=nahead)]
		assign('prevItaDT',prevItaDT,envir=.GlobalEnv)
		setDF(prevItaDT)
  }
})

output$fitIta <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegion")
  allDataReg <- copy( reacval$dataTables_reg)

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeries(allDataReg)$Italia
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

		out <- rbind(tmp, prevItaDT[, c('data', 'casi',  'variabilePrevista')])

  #  setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
	#	setDF(allDataReg)
#		out <- rbind(allDataReg[, c('regione', 'casi totali', "data")], prevDT[, c('regione', 'casi totali', "data")])

    p <- ggplot(out) + my_ggtheme() +
          geom_line(aes(x=data, y=`casi`, color=variabilePrevista)) +
          scale_color_manual(values=d3hexcols20)
    p
  }
})



output$fitItaLog <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitItaLog")
  allDataReg <- copy( reacval$dataTables_reg)
	modelliIta <- isolate(reacval$modelliIta)

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeries(allDataReg)$Italia
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
		out <- rbind(tmp, prevItaDT[, c('data', 'casi',  'variabilePrevista')])
		out$casi <- log(out$casi)

		fit <- as.data.frame(do.call(rbind, lapply(modelliIta, function(x) x$coefficients)))
		names(fit[,1]) <- "int"

    p <- ggplot(out) + my_ggtheme() +
					geom_point(aes(x=data, y=`casi`, color=variabilePrevista))+
#					geom_abline(data=fit, mapping=aes(slope=data, intercept='int')) +
					geom_abline(slope=fit[1,2], intercept=fit[1,1])+
					geom_abline(slope=fit[2,2], intercept=fit[2,1])+
					geom_abline(slope=fit[3,2], intercept=fit[3,1])+
					geom_abline(slope=fit[4,2], intercept=fit[4,1])+
					#geom_abline(data=d,   mapping=aes(slope=s, intercept=ic, linetype=factor(s))) +
					scale_color_manual(values=d3hexcols20)
    p
  }
})



})
