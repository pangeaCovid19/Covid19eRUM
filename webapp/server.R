
shinyServer(function(input, output, session) {
  reacval<-reactiveValues(mobile=F)


  observe({
  		if (!is.null(input$GetNavUserAgent)){
  			if (grepl("mobile",tolower(input$GetNavUserAgent)) || grepl("android",tolower(input$GetNavUserAgent)))
  			reacval$mobile<-T
  		}
  	})


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
            dataTables=allData,
            dateRange=date_range,
						mdataProv=mtimeProv,
						fileList_reg=list.files(dir_reg, full.names=T),
            oldList_reg=NULL,
            dataTables_reg=allData_reg,
            dateRange_reg=date_range_reg,
						mdataReg=mtimeReg,
						modelliIta=modelliIta,
						modelliReg=modelliReg,
						modelliItaExp=modelliItaExp,
						modelliRegExp=modelliRegExp
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
			reacval$dateRange_reg 	<- max(regData$data)

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

  get_data <- reactive({
		if(verbose) cat("\n REACTIVE:getDATA")
    res <- reacval$dataTables
    #if (!is.null(input$drangeSel)) res <- res[res$data >= input$drangeSel[1] & res$data <= input$drangeSel[2],]
    res
  })

#DEPRECATA
#	  get_data_reg <- reactive({
#			if(verbose) cat("\n REACTIVE:getDATA")
#	    res <- reacval$dataTables_reg
	    #if (!is.null(input$drangeSel)) res <- res[res$data >= input$drangeSel[1] & res$data <= input$drangeSel[2],]
#	    res
#	  })

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
  h4(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineRegion <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegion")
  allData <- reacval$dataTables#get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
    colnames(allDataReg)[2] <- "regione"
    colnames(allDataReg)[3] <- "casi totali"
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

output$tabRegionNEW <- renderDT({
	if(verbose) cat("\n renderDT:tabRegion")
  allDataReg <- reacval$dataTables_reg#get_data()
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

output$tabRegion <- renderDT({
	if(verbose) cat("\n renderDT:tabRegion")
  allData <- reacval$dataTables#get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop), by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione), FUN=sum)
    allDataReg$data <- as.Date(allDataReg$data)
    latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    latestDataReg$`casi su 10mila abit` <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
    latestDataReg$pop <- NULL
    latestDataReg$data <- strftime(latestDataReg$data, format="%d-%m-%Y")
    colnames(latestDataReg)[2] <- "regione"
    colnames(latestDataReg)[3] <- "casi totali"

    datatable(latestDataReg,
      selection = list(target = NULL),
      options= c(list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})


output$mapRegion <- renderLeaflet({
	if(verbose) cat("\n renderLeaflet:mapRegion")

  allData <- reacval$dataTables#get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataReg <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_regione=allDataConf$denominazione_regione, codice_regione=allDataConf$codice_regione),
                      FUN=sum)
    latestDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    latestDataReg$densita_casi <- round(latestDataReg$totale_casi / latestDataReg$pop * 10000, 3)
    pltRegioni <- merge(regioni, latestDataReg[,c("codice_regione", "totale_casi", "densita_casi")], by.x="COD_REG", by.y="codice_regione")
    pal <- colorNumeric("YlOrRd", domain = log10(pltRegioni$totale_casi))

		suppressWarnings(leaflet(data = pltRegioni, options = leafletOptions(zoomControl = FALSE,minZoom = 3, maxZoom = 6)) %>%
			addTiles()%>%
			addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
			addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity=.7,
		            label = ~paste(DEN_REG, "- casi:", totale_casi)) %>%
      addLegend(pal = pal, values = ~log10(totale_casi), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomleft',
                title = paste0("casi")))
  }
})

output$mapRegionGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapRegionGG")
  allData <- reacval$dataTables#get_data()
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
  allData <- reacval$dataTables#get_data()
  if (!is.null(allData)) {
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
    allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi),
                      by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                      FUN=sum)
    colnames(allDataProv)[2] <- "provincia"
    colnames(allDataProv)[3] <- "casi totali"
    p <- ggplot(allDataProv) + my_ggtheme() +
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

  if (!is.null(allData)) {
    allData <- reacval$dataTables#get_data()
    allDataConf <- allData[!grepl("aggiornamento", allData$denominazione_provincia),]
    allDataConf <- allDataConf[allDataConf$denominazione_regione == myReg,]
    allDataProv <- aggregate(list(totale_casi=allDataConf$totale_casi, pop=allDataConf$pop),
                      by=list(data=allDataConf$data, denominazione_provincia=allDataConf$denominazione_provincia),
                      FUN=sum)
    latestDataProv <- allDataProv[allDataProv$data==max(allDataProv$data),]
    latestDataProv$`casi su 10mila abit` <- round(latestDataProv$totale_casi / latestDataProv$pop * 10000, 3)
    latestDataProv$pop <- NULL
    latestDataProv$data <- strftime(latestDataProv$data, format="%d-%m-%Y")
    colnames(latestDataProv)[2] <- "provincia"
    colnames(latestDataProv)[3] <- "casi totali"

    datatable(latestDataProv,
      selection = list(target = NULL),
      options= c(list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})


output$mapProvince <- renderLeaflet({
	if(verbose) cat("\n renderLeaflet:mapProvince")
  myReg <- input$regionSel
  allData <- reacval$dataTables#get_data()
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
    pal <- colorNumeric("YlOrRd", domain = log10(pltProvince$totale_casi))
    suppressWarnings(leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
                 label = ~paste(DEN_UTS, "- casi:", totale_casi)) %>%
        addLegend(pal = pal, values = ~log10(totale_casi), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomright',
                title = paste0("casi")))

  }
})

output$mapProvinceGG <- renderPlot({
	if(verbose) cat("\n renderPlot:mapProvinceGG")
  myReg <- input$regionSel
  allData <- reacval$dataTables#get_data()
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
 			#map_italia+
      geom_sf(data = pltProvince, aes(fill = totale_casi), color="black", size=.2) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Numero casi", trans = "log10") +
      geom_text(data = pltProvince, aes(x=prv_long, y=prv_lat, label=paste(DEN_UTS, "\n casi:", totale_casi)), cex=2.5, color="black", fontface = "bold")


  }
})

getTimeSeriesReact <- reactive({
	if(verbose) cat("\n reactive:getTimeSeriesReact")
	allDataReg <- copy( reacval$dataTables_reg)
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


get_predictions <- function(modelli, datiTS, nahead, alldates=FALSE) {
  previsioni <- mapply(FUN=predictNextDays, datiTS, modelli, nahead=nahead, all=alldates, SIMPLIFY=F)
  previsioniDT <- rbindlist(previsioni)
  if (alldates)
    previsioniDT[, outName:=rep(names(modelli), each=nrow(datiTS[[1]])+nahead)]
  else
    previsioniDT[, outName:=rep(names(modelli), each=nahead)]
		setDF(previsioniDT)
  previsioniDT
}


prevRegion <- reactive({
	if(verbose) cat("\n reactive:prevRegion")
	allDataReg <- copy( reacval$dataTables_reg)
  tipoModello <- input$modelloFit
	nahead=3
	cat("\ttipoModello:", tipoModello)

	if(is.null(tipoModello)) return(NULL)
	if(tipoModello=="Exp. quadratico"){
		modelliReg=isolate(reacval$modelliReg)
	} else modelliReg <- isolate(reacval$modelliRegExp)


	if (!is.null(allDataReg)) {
		tsReg <- getTimeSeriesReact()
    tsReg["Italia"] <- NULL
		if(saveRDSout) saveRDS(file="prevRegionList.RDS",list(tsReg, modelliReg, allDataReg))
		if(assignout) assign("prevRegionList",list(tsReg, modelliReg, allDataReg), envir=.GlobalEnv)

		prevDT <- get_predictions(modelliReg, tsReg, nahead=nahead, alldates=TRUE)
    setnames(prevDT, old=c("outName"), new=c("regione"))
		setDF(prevDT)
		prevDT[,c("dataind","data2")]<-NULL
		if(assignout) assign("prevDT",prevDT, envir=.GlobalEnv)
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
    #mindataprev <- max(prevDT$data) - 2

    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
		setDF(allDataReg)

		p <- ggplot() + my_ggtheme() +
       suppressWarnings(geom_line(data=prevDT[which(prevDT$regione%in%regioniSel),], linetype=2, group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
           aes(x=data, y=`casi totali`, color=regione,
             text = paste('Data:', strftime(data, format="%d-%m-%Y"),
              '<br>Casi (fit): ', round(`casi totali`),
              '<br>Regione: ', regione)))) +
       suppressWarnings(geom_point(data=allDataReg[which(allDataReg$regione%in%regioniSel),], aes(x=data, y=`casi totali`, color=regione,
             text = paste('Data:', strftime(data, format="%d-%m-%Y"),
              '<br>Casi: ', `casi totali`,
              '<br>Regione: ', regione)))) +
			 scale_color_manual(values=d3hexcols20) +
			 #geom_rect(aes(xmin=mindataprev-0.5, xmax=max(prevDT$data+1), ymin=0, ymax=max(prevDT$casi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(prevDT$data),max(prevDT$data+1)))+
			 theme(axis.text.x=element_text(angle=45,hjust=1)) +
			 #geom_errorbar(data=prevDT,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
       labs(x="")


    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
    ###
    #allDataReg$UpperRange<-0
    #allDataReg$LowerRange<-0
    #out <- rbind(allDataReg[, c('regione', 'casi totali', "data","UpperRange","LowerRange")], prevDT[, c('regione', 'casi totali', "data","UpperRange","LowerRange")])
 		#out <- out[which(out$regione %in% regioniSel), ]

    #out1<-out[which(out$data<mindataprev),]
    #out2<-out[which(out$data>=mindataprev),]
    ###
    #q <- ggplot() + my_ggtheme() +
 		#		 geom_line(data=out1,aes(x=data, y=`casi totali`, color=regione)) +
 		#		 geom_point(data=out2,aes(x=data, y=`casi totali`, color=regione))+
 		#		 scale_color_manual(values=d3hexcols20)+
 		#		 geom_rect(aes(xmin=mindataprev-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+
 		#		 theme(axis.text.x=element_text(angle=45,hjust=1))+
 		#		 geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
 		#		 xlab("")
 	  #q
  }
})


prevIta <- reactive({
	if(verbose) cat("\n reactive:prevIta")
	allDataReg <- copy(reacval$dataTables_reg)
	tipoModello <- input$modelloFit
	nahead=3
	cat("\ttipoModello:", tipoModello)

	if(is.null(tipoModello)) return(NULL)
	if(tipoModello=="Exp. quadratico"){
		modelliIta=isolate(reacval$modelliIta)
	} else modelliIta <- isolate(reacval$modelliItaExp)

	if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()["Italia"]
		if(saveRDSout) saveRDS(file="prevItaList.RDS",list(tsIta, modelliIta, allDataReg))

    prevDT <- get_predictions(modelliIta, tsIta, nahead=nahead, alldates=TRUE)
    setnames(prevDT, old=c("outName"), new=c("variabilePrevista"))
    setDF(prevDT)
    prevDT[,c("dataind","data2")]<-NULL
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

		p <- ggplot() + my_ggtheme() +
			 suppressWarnings(geom_line(data=prevItaDT, linetype=2, group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
              aes(x=data, y=casi, color=variabilePrevista,
                text = paste('Data:', strftime(data, format="%d-%m-%Y"),
                 '<br>Casi (fit): ', round(casi),
                 '<br>Variabile: ', variabilePrevista)))) +
			 suppressWarnings(geom_point(data=tmp, aes(x=data, y=casi, color=variabilePrevista,
                text = paste('Data:', strftime(data, format="%d-%m-%Y"),
                 '<br>Casi: ', casi,
                 '<br>Variabile: ', variabilePrevista)))) +
			 scale_color_manual(values=d3hexcols20) +
       theme(axis.text.x = element_text(angle=45,hjust=1)) +
       labs(x="", color = "variabile prevista")

    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

    #tmp$UpperRange<-NA
    #tmp$LowerRange<-NA
    #out <- rbind(tmp, prevItaDT)
    #minprevdata <- max(prevItaDT$data)-2
    #cat("\t", as.character(minprevdata))

    #setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
    #setDF(allDataReg)
    #out1<-out[which(out$data<minprevdata),]
    #out2<-out[which(out$data>=minprevdata),]

    #p <- ggplot() + my_ggtheme() +
    #      geom_line(data=out1, aes(x=data, y=casi, color=variabilePrevista)) +
    #      geom_point(data=out2, aes(x=data, y=casi, color=variabilePrevista)) +
    #      scale_color_manual(values=d3hexcols20)+
    #      geom_rect(aes(xmin=minprevdata-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", #alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))  +
    #      geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=variabilePrevista),width=0.1)+
    #      xlab("")
    #p
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
	assign('tsIta',tsIta,envir=.GlobalEnv)
	assign('prevItaDT',prevItaDT,envir=.GlobalEnv)
	assign('gts',getTimeSeriesReact(),envir=.GlobalEnv)

  if (!is.null(prevItaDT) & !is.null(tsIta)) {
  		setnames(prevItaDT, old=c('Attesi'), new=c('casi'))
      setnames(tsIta, old=c('totale_casi'), new=c('casi'))
      num_rows <- nrow(tsIta)
      datiIta <- rbind(tsIta[, c("data", "casi")], prevItaDT[, c("data", "casi")])
      datiIta$tipo <- c(rep("veri", num_rows), rep("predetti", nrow(datiIta) - num_rows))
      datiIta$tipo <- factor(datiIta$tipo, levels=c("veri", "predetti"))

			indmax <- which.max(datiIta$casi)
      vdate <- datiIta$data[indmax]
		  datiIta <-datiIta[datiIta$data <= vdate,]
      datiIta$label <- c(rep("", nrow(datiIta)-1), "picco\n previsto")

      p <- ggplot() + my_ggtheme() +
  					suppressWarnings(geom_bar(data=datiIta, aes(x=data, y=casi, fill=tipo,
              text = paste('Data:', strftime(data, format="%d-%m-%Y"),
               '<br>Casi: ', round(casi))), stat="identity", width = 0.8))+
  #          geom_text(data=datiIta, aes(x=data, y=casi, label=label), cex=2.5, color="black", fontface = "bold") +
  					scale_fill_manual(values=d3hexcols) +
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
})

output$terapiaIntPlotPercPrev<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntPlot")

	tint <- terapiaInt()
	allDataReg <- copy( reacval$dataTables_reg)
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

})



output$tab_desktop<-renderUI({

  fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
  	h1("Analisi previsionale nelle province italiane"),
  	fluidRow(
  		column(10,h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione di approfondimento")),
  		column(2,radioButtons("modelloFit", label="Tipologia Modello", choices=c("Esponenziale", "Exp. quadratico")))
  	),

  		 br(),
  		fluidRow(style="padding:30px;background-color:#ffffff",
  			column(2,fluidRow(selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),checkboxGroupInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit)),
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

  		),br()
  	)

  })

  output$tab_mobile<-renderUI({

    fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
    	h1("Analisi previsionale nelle province italiane"),
    	fluidRow(
    		column(10,h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione di approfondimento")),
    		column(2,radioButtons("modelloFit", label="Tipologia Modello", choices=c("Esponenziale", "Exp. quadratico")))
    	),

    		 br(),
    		fluidRow(style="padding:30px;background-color:#ffffff",
    			column(2,fluidRow(selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),checkboxGroupInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit)),
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

    		),br()
    	)

    })




})
