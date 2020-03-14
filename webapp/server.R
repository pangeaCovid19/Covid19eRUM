
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
          geom_line(aes(x=data, y=`casi totali`, color=regione)) +
          scale_color_manual(values=d3hexcols20)
    p
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
      options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
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
    pal <- colorBin("YlOrRd", domain = log10(pltRegioni$totale_casi))

		leaflet(data = pltRegioni, options = leafletOptions(zoomControl = FALSE,minZoom = 5, maxZoom = 5)) %>%
			addTiles()%>%
			addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
			addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity=.7,
		  label = ~paste(DEN_REG, "- casi:", totale_casi))
#    leaflet(data = pltRegioni) %>% addTiles() %>%
 #       addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
  #      addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey",
   #              label = ~paste(DEN_REG, "- casi:", totale_casi))
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
          geom_line(aes(x=data, y=`casi totali`, color=provincia)) +
          scale_color_manual(values=d3hexcols20)
    p
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
      options=list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))),
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
    pal <- colorBin("YlOrRd", domain = log10(pltProvince$totale_casi))
    leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
                 label = ~paste(DEN_UTS, "- casi:", totale_casi))

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
output$graficiPrevisioniUI <- renderUI({
	if(verbose) cat("\n renderUI:graficiPrevisioniUI")
	tipoGraph <- input$regionLinLogFit
	if(tipoGraph=="Lineare"){
		fluidRow(
			box(width=6, title = tagList(shiny::icon("globe-europe"), "Totali Positivi per regione con previsione a 3 giorni"), status = "primary", solidHeader = F,
					collapsible = T, plotlyOutput(outputId="fitRegion"), spiegaFitPos
			),
			box(width=6, title =  "Andamenti globali in Italia con previsione a 3", status = "primary", solidHeader = F,
					collapsible = T,  plotlyOutput(outputId="fitIta"), spiegaFitTot
			)
		)
	} else {
		fluidRow(
			box(width=6, title = tagList(shiny::icon("globe-europe"), "Totali Positivi per regione con previsione a 3 in scala logaritmica"), status = "primary", solidHeader = F,
					collapsible = T, plotlyOutput(outputId="fitRegLog"), spiegaFitPosLog
			),
			box(width=6, title = tagList( "Andamenti globali in Italia con previsione a 3 in scala logaritmica"), status = "primary", solidHeader = F,
					collapsible = T,  plotlyOutput(outputId="fitItaLog"), spiegaFitTotLog
			)
		)
	}



})

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
		tsReg <- getTimeSeriesReact()[which(names(getTimeSeriesReact())!="Italia")]
		if(saveRDSout) saveRDS(file="prevRegionList.RDS",list(tsReg, modelliReg, allDataReg))
		if(assignout) assign("prevRegionList",list(tsReg, modelliReg, allDataReg), envir=.GlobalEnv)

		prev <- mapply(FUN=predictNextDays, tsReg, modelliReg, nahead=nahead, SIMPLIFY=F, all=TRUE)

		prevDT <- rbindlist(prev)
		prevDT[, regione:=rep(names(prev), each=nahead+nrow(tsReg[[1]]))]
		#prevDT[, regione:=rep(names(prev), each=nahead)]
		setDF(prevDT)

		if(assignout) assign("prevDT",prevDT, envir=.GlobalEnv)
		prevDT
  }
})



output$fitRegion <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegion")
  allDataReg <- copy( reacval$dataTables_reg)
	regioniSel <- input$regionSelFit

  if (!is.null(allDataReg)) {
		tsReg <- getTimeSeriesReact()[which(names(getTimeSeriesReact())%in%regioniSel)]
		#tsReg <- getTimeSeriesReact()[which(names(getTimeSeriesReact())!="Italia")]
		if(saveRDSout) saveRDS(file="fitRegionList.RDS",list(tsReg, allDataReg))
		#	tsReg <- getTimeSeries(allDataReg)
		#	tsReg <- tsReg[which(names(tsReg)!="Italia")]

		prevDT <-copy(prevRegion())
		setnames(prevDT, old=c('Attesi'), new=c('casi totali'))

    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
		setDF(allDataReg)
		mindataprev <- max(prevDT$data)-2


		p <- ggplot() + my_ggtheme() +
			 geom_line(data=prevDT[which(prevDT$regione%in%regioniSel),],aes(x=data, y=`casi totali`, color=regione), linetype=2) +
			 geom_point(data=allDataReg[which(allDataReg$regione%in%regioniSel),],aes(x=data, y=`casi totali`, color=regione))+
			 scale_color_manual(values=d3hexcols20)+
	#		 geom_rect(aes(xmin=mindataprev-0.5, xmax=max(prevDT$data+1), ymin=0, ymax=max(prevDT$casi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(prevDT$data),max(prevDT$data+1)))+
			 theme(axis.text.x=element_text(angle=45,hjust=1))+
		#		 geom_errorbar(data=prevDT,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
			 xlab("")

		p
    ###
   # allDataReg$UpperRange<-0
 #   allDataReg$LowerRange<-0
 #   out <- rbind(allDataReg[, c('regione', 'casi totali', "data","UpperRange","LowerRange")], prevDT[, c('regione', 'casi totali', "data","UpperRange","LowerRange")])
#		out <- out[which(out$regione %in% regioniSel), ]

#    out1<-out[which(out$data<mindataprev),]
#    out2<-out[which(out$data>=mindataprev),]
    ###
 ###q <- ggplot() + my_ggtheme() +
###				 geom_line(data=out1,aes(x=data, y=`casi totali`, color=regione)) +
###				 geom_point(data=out2,aes(x=data, y=`casi totali`, color=regione))+
###				 scale_color_manual(values=d3hexcols20)+
###				 geom_rect(aes(xmin=mindataprev-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+
###				 theme(axis.text.x=element_text(angle=45,hjust=1))+
###				 geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
###				 xlab("")
###	 q



  }
})

output$fitRegLog <- renderPlotly({
	if(verbose) cat("\n renderPlotly:fitRegLog")
  allDataReg <- copy( reacval$dataTables_reg)
	modelliReg <- isolate(reacval$modelliReg)
	regioniSel <- input$regionSelFit

  if (!is.null(allData)) {
		tsReg <- getTimeSeriesReact()[which(names(getTimeSeriesReact())%in%regioniSel)]
	if(saveRDSout) saveRDS(file="fitRegLogList.RDS",list(tsReg, modelliReg, allDataReg))
	#	tsReg <- getTimeSeries(allDataReg)
	#	tsReg <- tsReg[which(names(tsReg)!="Italia")]

	prevDT <-copy(prevRegion())
	setnames(prevDT, old=c('Attesi'), new=c('casi'))

  setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi'))
	setDF(allDataReg)

	mindataprev <- max(prevDT$data)-2


	allDataReg$logcasi <- log(allDataReg[,'casi'])
	#prevDT$logcasi <- log(prevDT[,'casi'])

	p <- ggplot() + my_ggtheme() +
	labs(title = "", x = "", y = "casi") +
 	geom_point(data= allDataReg[which(allDataReg$regione%in%regioniSel),], aes(x=data, y=casi, color=regione))+
	geom_line(data= prevDT[which(prevDT$regione%in%regioniSel),],aes(x=data, y=casi, color=regione), linetype=2) + scale_y_log10() +
 	#geom_abline(slope=df$slopes, intercept=df$intercepts, linetype=2)+
 	scale_color_manual(values=d3hexcols20)+
 #	geom_rect(aes(xmin=mindataprev-0.5, xmax=max(prevDT$data+1), ymin=0, ymax=max(prevDT$logcasi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(prevDT$data),max(prevDT$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))  +
 #	geom_errorbar(data=logcasi,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
 	xlab("")

 	 p

#out <- rbind(allDataReg[, c('regione', 'casi totali', "data")], prevDT[, c('regione', 'casi totali', "data")])

 # allDataReg$UpperRange<-10000
 # allDataReg$LowerRange<-10000
 # out <- rbind(allDataReg[, c('regione', 'casi totali', "data","UpperRange","LowerRange")], prevDT[, c('regione', 'casi totali', "data","UpperRange","LowerRange")])
#	out[,'casi totali' ] <- log(out[,'casi totali' ] )
 # out$UpperRange<-log(out$UpperRange)
 # out$LowerRange<-log(out$LowerRange)

#	out <- out[which(out$regione %in% regioniSel), ]
 # out1<-out[which(out$data<mindataprev),]
 # out2<-out[which(out$data>=mindataprev),]

#	fit <- as.data.frame(do.call(rbind, lapply(modelliReg, function(x) x$coefficients)))
#	names(fit[,1]) <- "int"

#definisco i fit per multiple regioni: da agganciare al tuo slectInput sulle regioni
 #   df<-data.frame(slopes=c(fit['Lombardia',2],fit['Veneto',2],fit['Emilia Romagna',2]),intercepts=c(fit['Lombardia',1],fit['Veneto',1],fit['Emilia Romagna',1]))

 #   df<-data.frame(
#			slopes=as.array(fit[regioniSel,2]),
#			intercepts=as.array(fit[regioniSel,1])
#		)


#    delta<-10 #definisce dopo quanti gg datta data minima parte la retta di fit
#    xstart_fit<-min(out$data)
#    xend_fit<-max(out$data)
#    p <- ggplot(out) + my_ggtheme() +
#		geom_point(aes(x=data, y=`casi totali`, color=regione))+
#	geom_abline(data=fit, mapping=aes(slope=data, intercept='int')) +
# geom_abline(slope=fit['Lombardia',2], intercept=fit['Lombardia',1], linetype=2)+
# geom_abline(slope=fit['Veneto',2], intercept=fit['Veneto',1], linetype=2)+
# geom_abline(slope=fit['Emilia Romagna',2], intercept=fit['Emilia Romagna',1], linetype=2)+
          #questo geom_segment non funge e non so perché
          #geom_segment(aes(x=xstart_fit+delta, xend=xend_fit, y=df$intercepts+df$slopes*as.numeric(xstart_fit+delta), yend=df$intercepts+df$slopes*as.numeric(xend_fit)), linetype=2)+
#    geom_abline(slope=df$slopes, intercept=df$intercepts, linetype=2)+
#		scale_color_manual(values=d3hexcols20)+
#    geom_rect(aes(xmin=mindataprev-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out[['casi totali']])*1.05),fill="grey", alpha=0.3)+xlim(c(min(out$data),max(out$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))  +
#    geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
#  xlab("")
#   p

  }
})


prevIta <- reactive({
	if(verbose) cat("\n reactive:prevIta")
		allDataReg <- copy( reacval$dataTables_reg)
		modelliIta <- isolate(reacval$modelliIta)
		nahead=3

		if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()$Italia
		if(saveRDSout) saveRDS(file="prevItaList.RDS",list(tsIta, modelliIta, allDataReg))
			#	tsIta <- getTimeSeries(allDataReg)$Italia
			prevIta <- lapply(modelliIta, function(modello, dati, nahead){
			predictNextDays(dati=dati, modello=modello,nahead=nahead, all=TRUE)
		},dati=tsIta, nahead=nahead)

		prevItaDT <- rbindlist(prevIta)
		prevItaDT[, variabilePrevista:=rep(names(prevIta), each=nrow(tsIta)+nahead)]
		setDF(prevItaDT)
		prevItaDT
  }
})


output$fitIta <- renderPlotly({
if(verbose) cat("\n renderPlotly:fitRegion")
  allDataReg <- copy( reacval$dataTables_reg)

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()$Italia
		if(saveRDSout) saveRDS(file="fitItaList.RDS",list(tsIta, allDataReg))
		#	tsIta <- getTimeSeries(allDataReg)$Italia
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

#out <- rbind(tmp, prevItaDT[, c('data', 'casi',  'variabilePrevista')])
    tmp$UpperRange<-NA
    tmp$LowerRange<-NA
    out <- rbind(tmp, prevItaDT)
		minprevdata <- max(prevItaDT$data)-2

		cat("\t", as.character(minprevdata))

		p <- ggplot() + my_ggtheme() +
			 geom_line(data=prevItaDT,aes(x=data, y=casi, color=variabilePrevista), linetype=2) +
			 geom_point(data=tmp,aes(x=data, y=casi, color=variabilePrevista))+
			 scale_color_manual(values=d3hexcols20)+
#			 geom_rect(aes(xmin=mindataprev-0.5, xmax=max(prevItaDT$data+1), ymin=0, ymax=max(prevItaDT$casi)*1.05),fill="grey", alpha=0.3)+ xlim(c(min(prevItaDT$data),max(prevItaDT$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))+
		#		 geom_errorbar(data=prevDT,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
			 xlab("")
			 #geom_rect(aes(xmin=minprevdata-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))

		p

  #  setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
#	setDF(allDataReg)
 #   out1<-out[which(out$data<minprevdata),]
#    out2<-out[which(out$data>=minprevdata),]

 #   p <- ggplot() + my_ggtheme() +
  #        geom_line(data=out1, aes(x=data, y=casi, color=variabilePrevista)) +
   #       geom_point(data=out2, aes(x=data, y=casi, color=variabilePrevista)) +
 #         scale_color_manual(values=d3hexcols20)+
 #         geom_rect(aes(xmin=minprevdata-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", #alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))  +
 #         geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=variabilePrevista),width=0.1)+
 #         xlab("")
  #  p
  }
})


output$fitItaLog <- renderPlotly({
if(verbose) cat("\n renderPlotly:fitItaLog")
  allDataReg <- copy( reacval$dataTables_reg)
modelliIta <- isolate(reacval$modelliIta)

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()$Italia
		if(saveRDSout) saveRDS(file="fitItaLogList.RDS",list(tsIta, modelliIta, allDataReg))
		#	tsIta <- getTimeSeries(allDataReg)$Italia
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
		minprevdata <- max(prevItaDT$data)-2
#out <- rbind(tmp, prevItaDT[, c('data', 'casi',  'variabilePrevista')])
#
if(assignout) assign("tmp",tmp, envir=.GlobalEnv)
if(assignout) assign("prevItaDT",prevItaDT, envir=.GlobalEnv)
	tmp$logcasi <- log(tmp[,'casi'])
	#prevItaDT$logcasi <- log(prevItaDT[,'casi'])

	p <- ggplot() + my_ggtheme() +
		labs(title = "", x = "", y = "casi (log)") +
	 	geom_point(data= tmp, aes(x=data, y=casi, color=variabilePrevista))+
		geom_line(data= prevItaDT,aes(x=data, y=casi, color=variabilePrevista), linetype=2) + scale_y_log10() +
	 	#geom_abline(slope=df$slopes, intercept=df$intercepts, linetype=2)+
	 	scale_color_manual(values=d3hexcols20)
	 #	geom_errorbar(data=logcasi,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=regione),width=0.1)+
	# 	xlab("")#+
	# 	geom_rect(aes(xmin=mindataprev-0.5, xmax=max(prevItaDT$data+1), ymin=0, ymax=max(prevItaDT$logcasi)*1.05),fill="grey", alpha=0.3)+xlim(c(min(prevItaDT$data),max(prevItaDT$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))

 	 p

  #  tmp$UpperRange<-NA
 #   tmp$LowerRange<-NA
 #   out <- rbind(tmp, prevItaDT[, c('data', 'casi',  'variabilePrevista', 'UpperRange', 'LowerRange')])
 #   out$casi <- log(out$casi)
 #   out$UpperRange<-log(out$UpperRange)
 #   out$LowerRange<-log(out$LowerRange)
  #  out1<-out[which(out$data<minprevdata),]
 #   out2<-out[which(out$data>=minprevdata),]

#		fit <- as.data.frame(do.call(rbind, lapply(modelliIta, function(x) x$coefficients)))
#		names(fit[,1]) <- "int"

 #   delta<-10 #definisce dopo quanti gg datta data minima parte la retta di fit
 #   xstart_fit<-min(out$data)
 #   xend_fit<-max(out$data)
 #   p<-ggplot() + my_ggtheme() +
#            geom_point(data= out, aes(x=data, y=casi, color=variabilePrevista))+
#
#            geom_segment(aes(x=xstart_fit+delta, xend=xend_fit, y=fit[1:4,1]+fit[1:4,2]*as.numeric(xstart_fit+delta), yend=fit[1:4,1]+fit[1:4,2]*as.numeric(xend_fit)), linetype=2)+

 #           scale_color_manual(values=d3hexcols20)+
 #           geom_rect(aes(xmin=minprevdata-0.5, xmax=max(out$data+1), ymin=0, ymax=max(out$casi)*1.05),fill="grey", #alpha=0.3)+xlim(c(min(out$data),max(out2$data+1)))+theme(axis.text.x=element_text(angle=45,hjust=1))  +
 #           geom_errorbar(data=out2,aes(x=data,ymin=LowerRange, ymax=UpperRange, color=variabilePrevista),width=0.1)+
 #           xlab("")

  }
})





})
