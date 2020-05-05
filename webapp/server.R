
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
						modelliItaGomp=modelliItaGomp,
						modelliRegGomp=modelliRegGomp,
            mobile=F
					)



observe({

		if (!is.null(input$GetNavUserAgent)){

			if (grepl("mobile",tolower(input$GetNavUserAgent)) || grepl("android",tolower(input$GetNavUserAgent)))
			reacval$mobile<-T
		}
	})


  observe({
		if(verbose) cat("\n OBSERVE:leggiDati")
    autoInvalidate()
		pathProv 	<- paste0(dir_prov,provRDS)
		pathReg 	<- paste0(dir_reg,regRDS)
		if (file.info(pathProv)$mtime > isolate(reacval$mdataProv)) {
			if(verbose) cat("\t entro   file.info(pathProv)$mtime > isolate(reacval$mdataProv)  ")
      ## Qui creiamo anche i dati regionali con solo i confermati... capire se vogliamo usare questi per le previsioni...
			prvData	<- readRDS(pathProv)
			reacval$mdataProv <- file.info(pathProv)$mtime
      reacval$dateRange_prv <- max(prvData$data)

      prvData <- prvData[!grepl("aggiornamento", prvData$denominazione_provincia),]
      prvData <- aggregate(list(totale_casi=prvData$totale_casi, pop=allDataConf$pop),
                        by=list(data=prvData$data, denominazione_regione=prvData$denominazione_regione,
                                denominazione_provincia=prvData$denominazione_provincia, codice_provincia=prvData$codice_provincia),
                        FUN=sum)
      prvData$`casi su 10mila abit` <- round(prvData$totale_casi / prvData$pop * 10000, 2)
      reacval$dataTables_prv <- prvData

      regDataFlt	<- readRDS(pathProv)
      regDataFlt <- regDataFlt[!grepl("aggiornamento", regDataFlt$denominazione_provincia),]
      regDataFlt <- aggregate(list(totale_casi=regDataFlt$totale_casi, pop=regDataFlt$pop),
                            by=list(data=regDataFlt$data,
                                    denominazione_regione=regDataFlt$denominazione_regione, codice_regione=regDataFlt$codice_regione),
                            FUN=sum)
      regDataFlt$`casi su 10mila abit` <- round(regDataFlt$totale_casi / regDataFlt$pop * 10000, 2)
      reacval$dataTables_reg_flt <- regDataFlt

      if(assignout) assign("outallData_prv",prvData,envir=.GlobalEnv)
      if(assignout) assign("outallData_reg_flt",regData,envir=.GlobalEnv)
		}
		if (file.info(pathReg)$mtime > isolate(reacval$mdataReg)) {
			if(verbose) cat("\t entro   file.info(pathReg)$mtime > isolate(reacval$mdataReg)  ")
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

output$data_agg<-renderText({
  str<-get_last_date()
  str<-paste("Dati aggiornati al:",str)
  })


## REGIONI
output$updateRegUI <- renderUI({

	if(verbose) cat("\n renderUI:updateRegUI")
  h4(paste("Dati aggiornati al giorno:", get_last_date()))
})


output$lineRegioni <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegioni")
  allDataReg <- copy(reacval$dataTables_reg)
#	allDataReg <- copy(res)
	var2plot <- input$variabileLineRegioni
	if(is.null(var2plot)) return(NULL)
	var2plotNew <- gsub("_", " ", var2plot)


  if (!is.null(allDataReg)) {
#		allDataReg <- copy(res)
    setnames(allDataReg, old=c('denominazione_regione',var2plot), new=c('regione', 'VAR2PLOT'))
    p <- ggplot(allDataReg) + my_ggtheme() +
          suppressWarnings(geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
            aes(x=data, y=VAR2PLOT, color=regione,
            text = paste('Regione:', regione, '<br>Data:', strftime(data, format="%d-%m-%Y"),
             '<br>Casi: ', VAR2PLOT)))) +
          scale_color_manual(values=color_regioni) +
          theme(axis.text.x=element_text(angle=45, hjust=1)) +
					guides(fill=guide_legend(title="regione")) +
          xlab("")+ylab(var2plotNew)
    if(reacval$mobile){
      p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
    else{
      p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")+theme(legend.title=element_blank())
    }
    plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

    if(reacval$mobile){

      plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
            layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
    }
    plot

  }
})


output$confrontoGiornoUI <- renderUI({
	allDataReg <- copy(reacval$dataTables_reg)
	if (is.null(allDataReg))retunr(NULL)

	sliderInput("confrontoGiorno", "Giorno:", min = min(allDataReg$data) + 1, max = max(allDataReg$data), value = max(allDataReg$data), animate = animationOptions(interval = 1500), timeFormat="%b %d",width='90%')

})



output$puntiRegioni <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegioni")
  allDataReg <- copy(reacval$dataTables_reg)
	xVar <- input$confrontox #input$variabileLineRegioni
	yVar <- input$confrontoy #input$variabileLineRegioni
	assiGraph <- input$confrontoTipoGratico
	giorno <- input$confrontoGiorno

	#	allDataReg <- copy(allData_reg)

	if(is.null(xVar)) return(NULL)
	if(is.null(yVar)) return(NULL)
	if (is.null(allDataReg)) return(NULL)
	if (is.null(assiGraph)) assiGraph <- "Lineari"
	if (is.null(giorno)) return(NULL)

#	validate((need("Selezionari assi co, nomi diversi")))

	xVarNew <- gsub("_", " ", xVar)
	yVarNew <- gsub("_", " ", yVar)


	if(xVar == yVar) {
		setnames(allDataReg, old=c('denominazione_regione',xVar), new=c('regione','XVAR'))
		allDataReg$YVAR <- allDataReg$XVAR
	} else setnames(allDataReg, old=c('denominazione_regione',xVar, yVar), new=c('regione','XVAR', 'YVAR'))

	#setnames(allDataReg, old=c('denominazione_regione',xVar, yVar), new=c('regione','XVAR', 'YVAR'))

	tmp <- allDataReg[which(allDataReg$data==giorno),]

	dfplot <- data.frame( XVAR= pmax(log10(tmp$XVAR), 0), YVAR=pmax(0, log10(tmp$YVAR)), regione=tmp[,'regione' ])
	dfplot <- data.frame( XVAR= pmax((tmp$XVAR), 0), YVAR=pmax(0, (tmp$YVAR)), regione=tmp[,'regione' ])
	#dfplot[as.data.frame(lapply(dfplot, is.infinite))] <- 0

	p <- ggplot(dfplot) + my_ggtheme() +
		suppressWarnings(
			geom_point( data=dfplot,
				aes(
					x=XVAR, y=YVAR, color=regione, text = 	paste( '<br>', xVarNew, ':', XVAR, '<br>',yVarNew,': ', YVAR, '<br>Regione: ',regione)
		 		)
		 )
	 ) +
	  scale_color_manual(values=color_regioni) +
	  theme(axis.text.x=element_text(angle=45, hjust=1)) +
		guides(fill=guide_legend(title="regione")) +
	  xlab(xVarNew)+ylab(yVarNew)

		if(assiGraph=="Logaritmici") p <- p + scale_y_log10()+ scale_x_log10()
    if(!reacval$mobile){
      p <- p +theme(legend.title=element_blank())
    }

  plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  if(reacval$mobile){

    plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
          layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
  }
  plot

})


output$tabRegioni <- renderDT({
	if(verbose) cat("\n renderDT:tabRegioni")
  allDataReg <- reacval$dataTables_reg
  if (!is.null(allDataReg)) {
    regrdx <- allDataReg[allDataReg$data==max(allDataReg$data), ]
		regrdx$casi10k <- round(regrdx$totale_casi / regrdx$pop * 10000, 2)
		regrdx$casiTampone <- round(regrdx$totale_casi / regrdx$tamponi , 2)

		out <- regrdx[, c('denominazione_regione', 'tamponi', 'totale_casi','casiTampone', 'totale_ospedalizzati','terapia_intensiva','deceduti','dimessi_guariti','casi10k')]
		names(out) <- c('regione', 'tamponi', 'casi', 'casi x tampone', 'ospedalizzati', 'Terapia intensiva','deceduti','guariti', 'casi su 10mila abit.')
 #   out$`casi su 10mila abit` <- round(out$totale_casi / out$pop * 10000, 3)

    datatable(out,extensions = c('Scroller'),
      selection = list(target = NULL),
      options= c(list(dom = 't',scroller=T,scrollX="300",scrollY="300",paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})

#output$tabRegioniOLD <- renderDT({
#	if(verbose) cat("\n renderDT:tabRegioni")
# allDataReg <- copy(reacval$dataTables_reg_flt)
#  if (!is.null(allDataReg)) {
#    allDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
#    allDataReg$pop <- NULL
#    allDataReg$data <- strftime(allDataReg$data, format="%d-%m-%Y")
#    setnames(allDataReg, old=c('denominazione_regione', 'totale_casi'), new=c('regione', 'casi totali'))
#    datatable(allDataReg,
#      selection = list(target = NULL),
#      options= c(list(paging = T, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
#      rownames=F)
#  }
#})


output$selRegioni <- renderUI({
	if(verbose) cat("\n renderUI:selRegioni")
  allDataReg <- reacval$dataTables_reg_flt
	if (is.null(allDataReg)) {
		if(verbose) cat("\t allDataReg è NULL")
		return(NULL)
	}
  if (animazione) {
		if(verbose) cat("\t animazione: ", animazione)
    fluidRow(sliderInput("giornoReg", "Giorno:",
              min = min(allDataReg$data) + 1, max = max(allDataReg$data),
              value = max(allDataReg$data), animate = animationOptions(interval = 1500), timeFormat="%b %d")
    )
  }
})

observe({
	if(verbose) cat("\n observe:tabRegioni")
	allDataReg <- copy(reacval$dataTables_reg_flt)
	if (is.null(allDataReg)) return(NULL)
	if(assignout) assign('outallData_observe', allDataReg, envir=.GlobalEnv)

	if(animazione){
		if(verbose) cat("\t animazione: ", animazione)
		myGiorno <- input$giornoReg
	} else myGiorno <- max(allDataReg$data, na.rm=TRUE)

  if (!is.null(myGiorno)) {
    allDataReg <- allDataReg[allDataReg$data==myGiorno,]
    allDataReg$totale_casi[allDataReg$totale_casi==0] <- NA_integer_
    pltRegioni <- merge(regioni, allDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")
    #pal <- colorNumeric("YlOrRd", domain = log10(pltRegioni$totale_casi))
		pal <- palRegioni()
    leafletProxy(mapId="mapRegioni", data=pltRegioni) %>% clearShapes() %>%
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
             label = ~paste(DEN_REG, "- casi:", totale_casi))
  }
})

palRegioni <- reactive({
	if(verbose) cat("\n reactive:pal")
	indDataMax <- which(reacval$dataTables_reg_flt$totale_casi > 0)
	colorNumeric("YlOrRd", domain = log10(c(1, (reacval$dataTables_reg_flt$totale_casi[indDataMax] ))))
})

output$mapRegioni <- renderLeaflet({

	if(verbose) cat("\n renderLeaflet:mapRegioni --- ")
  allDataReg <- copy(reacval$dataTables_reg_flt)

  if (!is.null(allDataReg)) {
    allDataReg <- allDataReg[allDataReg$data==max(allDataReg$data),]
    pltRegioni <- merge(regioni, allDataReg[,c("codice_regione", "totale_casi")], by.x="COD_REG", by.y="codice_regione")
		#pltRegioni$prova <- 10^log10(pltRegioni$totale_casi)
    #pal <- colorNumeric("YlOrRd", domain = log10(pltRegioni$totale_casi))
		pal <- palRegioni()

		if(animazione){

			if(verbose) cat("\n animazione --- TRUE")
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
		} else  {
			if(verbose) cat("\n animazione --- FALSE")
			suppressWarnings(leaflet(data = pltRegioni, options = leafletOptions(zoomControl = FALSE,minZoom = 3, maxZoom = 6)) %>%
				addTiles()%>%
				addProviderTiles("CartoDB.Positron") %>% setView(lng=12.5, lat=41.3, zoom=5)  %>%
				# i poligoni li mette l'observe sopra... se li mettiamo anche qui, sfarfalla all'avvio
				addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity=.7,          label = ~paste(DEN_REG, "- casi:", totale_casi)) %>%
				addLegend(pal = pal, values = ~log10(10^(1:4)), opacity = 0.7,
									labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
									position = 'bottomleft',
									title = paste0("casi")))
		}
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


# output$selLagRegione1 <- renderUI ({
# 	if (verbose) cat("\nrenderUI-selLagRegione1")
# 	myDate <- isolate(reacval$dateRange_reg)
# 	regione1 	<- input$serieStoricheRegion1
# 	if(is.null(regione1))return(NULL)
# 	if(is.null(myDate))return(NULL)
# 	dg <- ceiling(as.numeric(myDate[2]-myDate[1])/2)
# 	sliderInput("lagRegione1", paste0("Lag ",regione1), min = -dg , max = dg, value = 0)
# })
#
#
# output$selLagRegione2 <- renderUI ({
# 	if (verbose) cat("\nrenderUI-selLagRegione2")
# 	regione2 	<- input$serieStoricheRegion2
# 	myDate <- isolate(reacval$dateRange_reg)
# 	if(is.null(regione2))return(NULL)
# 	if(is.null(myDate))return(NULL)
# 	dg <- ceiling(as.numeric(myDate[2]-myDate[1])/2)
# 	sliderInput("lagRegione2", paste0("Lag ",regione2), min = -dg , max = dg, value = 0)
# 	noUiSliderInput( "lagRegione2", paste0("Lag ",regione2), min = -dg , max = dg, value = 0, step=1, orientation="vertical", height=100)
#
# })

#######################################################################

output$selLagProvince <- renderUI ({
	if (verbose) cat("\nrenderUI-selLagProvince")
	province <- input$provSelSerieStoriche
	myDate <- isolate(reacval$dateRange_prv)
	if(is.null(province))return(NULL)
	if(is.null(myDate))return(NULL)
	dg <- ceiling(as.numeric(myDate[2]-myDate[1])/2)

	 #sliderInput("lagRegione2", paste0("Lag ",regione2), min = -dg , max = dg, value = 0)
	 res <- lapply(province, function(x) {
 	 initval<-0

	 #perché non prendo direttamente l'input?
 	 if (paste0("selLag_",x) %in% names(input)){
		 if(verbose) cat("\n\t IN if",paste0("selLag_",x), "->",initval)
 		 #initval<-reacLiveOp$listProvince[[paste0("selLag_",x)]]
 		 initval<-isolate(input[[paste0("selLag_",x)]])
 	 } else if(verbose)
   { cat("\n\t OUT if",paste0("selLag_",x), "->", initval)}
 	 #sliderInput(paste0("iOraUtilizzoBuffer_",x), label = paste("Orario Utilizzo Buffer",x), min = minOra, max = maxOra,	value = valori2)
   #sliderInput(paste0("selLag_",x), paste0("Lag ",x), min = -dg , max = dg, value = initval, step=1)
   if(reacval$mobile){
     noUiSliderInput( paste0("selLag_",x), x, min = -dg , max = dg, value = initval, step=1, orientation="horizontal",inline=T,width='140px', color=color_province[x],
     format=wNumbFormat(decimals = 0,prefix = 'giorno '))
   }else{
     noUiSliderInput( paste0("selLag_",x), x, min = -dg , max = dg, value = initval, step=1, orientation="vertical",height='120px',inline=T,width='140px', color=color_province[x],
     format=wNumbFormat(decimals = 0,prefix = 'giorno '))
   }

	})

})


#
#

output$lineProvinceConfronto <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineProvinceConfronto")
  allDataPrv <- copy(reacval$dataTables_prv)

	selprov <- input$provSelSerieStoriche
	if (is.null(selprov)) return(NULL)
	if(verbose) cat("\n \t province:", selprov)

	lagProvince <- unlist(
		lapply(selprov, function(x) {
			initval<-input[[paste0("selLag_",x)]]
			if(verbose) cat("\t initval:", initval)
			if (is.null(initval)) return(0)
			initval
 		})
	)
	if (is.null(lagProvince)) return(NULL)

		if(length(selprov)!=length(lagProvince)) return (NULL)

	names(lagProvince) <- selprov

	var2plot <- "totale_casi"
	if(is.null(var2plot)) var2plot <- "totale_casi"#return(NULL)
	if (is.null(allDataPrv)) return(NULL)

	var2plotNew <- gsub("_", " ", var2plot)

  setnames(allDataPrv, old=c('denominazione_provincia',var2plot), new=c('provincia', 'VAR2PLOT'))

	dataPrv <- allDataPrv[allDataPrv$provincia %in% selprov, ]
	setDT(dataPrv)
	for(i in 1:length(selprov)){
		prv <- selprov[i]
		lag <- lagProvince[which(names(lagProvince)==prv)]
		dataPrv[provincia==prv, datanew:=data+lag]
	}

  p <- ggplot(dataPrv) + my_ggtheme() +
        suppressWarnings(geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
          aes(x=datanew, y=VAR2PLOT, color=provincia,
          text = paste('Provincia:', provincia, '<br>Data:', strftime(data, format="%d-%m-%Y"),
           '<br>Casi: ', VAR2PLOT)))) +
				geom_point( aes(x=datanew, y=VAR2PLOT, color=provincia)) +
        scale_color_manual(values=color_province) +
        theme(legend.title=element_blank())+theme(axis.text.x=element_text(angle=45, hjust=1)) +
				guides(fill=guide_legend(title="provincia")) +
        xlab("")+ylab(var2plotNew)
  if(reacval$mobile){
    p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
  else{
    p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")
  }
  plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

  if(reacval$mobile){

    plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
          layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
  }
  # else{
  #   plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))
  # }
  plot

})
#######################################################################

#reacCompare$listaRegioniCompare
#reacCompare$listRegioni
output$selLagRegioni <- renderUI ({
	if (verbose) cat("\nrenderUI-selLagRegioni")
	regioni <- input$regionSelSerieStoriche
	myDate <- isolate(reacval$dateRange_reg)
	if(is.null(regioni))return(NULL)
	if(is.null(myDate))return(NULL)
	dg <- ceiling(as.numeric(myDate[2]-myDate[1])/2)


	 #sliderInput("lagRegione2", paste0("Lag ",regione2), min = -dg , max = dg, value = 0)
	 res <- lapply(regioni, function(x) {
 	 initval<-0

	 #perché non prendo direttamente l'input?
 	 if (paste0("selLag_",x) %in% names(input)){
		 if(verbose) cat("\n\t IN if",paste0("selLag_",x), "->",initval)
 		 #initval<-reacLiveOp$listRegioni[[paste0("selLag_",x)]]
 		 initval<-isolate(input[[paste0("selLag_",x)]])
 	 } else if(verbose)
   { cat("\n\t OUT if",paste0("selLag_",x), "->", initval)}
 	 #sliderInput(paste0("iOraUtilizzoBuffer_",x), label = paste("Orario Utilizzo Buffer",x), min = minOra, max = maxOra,	value = valori2)
   #sliderInput(paste0("selLag_",x), paste0("Lag ",x), min = -dg , max = dg, value = initval, step=1)
   if(reacval$mobile){
     noUiSliderInput( paste0("selLag_",x), x, min = -dg , max = dg, value = initval, step=1, orientation="horizontal",color=color_regioni[x],inline=T,width='140px',
     format=wNumbFormat(decimals = 0,prefix = 'giorno '))
   }else{
     noUiSliderInput( paste0("selLag_",x), x, min = -dg , max = dg, value = initval, step=1, orientation="vertical",color=color_regioni[x],height='120px',inline=T,width='140px',
     format=wNumbFormat(decimals = 0,prefix = 'giorno '))
   }

	})

})


#
#

output$lineRegioniConfronto <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegioniConfronto")
  allDataReg <- copy(reacval$dataTables_reg)



	regioni <- input$regionSelSerieStoriche

	if(verbose) cat("\t -> regioni:", regioni)
	if (is.null(regioni)) return(NULL)
	if(verbose) cat("\n \t regioni:", regioni)

	lagRegioni <- unlist(
		lapply(regioni, function(x) {
			initval<-input[[paste0("selLag_",x)]]
			if(verbose) cat("\t initval:", initval)
			if (is.null(initval)) return(NULL)
			initval
 		})
	)
	if (is.null(lagRegioni)) return(NULL)

	if(length(regioni)!=length(lagRegioni)) return (NULL)

	names(lagRegioni) <- regioni

	var2plot <- input$variabileCompare
	if(is.null(var2plot)) var2plot <- "totale_casi"#return(NULL)
	if (is.null(allDataReg)) return(NULL)

	var2plotNew <- gsub("_", " ", var2plot)

  setnames(allDataReg, old=c('denominazione_regione',var2plot), new=c('regione', 'VAR2PLOT'))

#		assign("regioni", regioni, envir=.GlobalEnv)
#			assign("lagRegioni", lagRegioni, envir=.GlobalEnv)
#				assign("allDataReg1", allDataReg, envir=.GlobalEnv)

	dataReg <- allDataReg[allDataReg$regione %in% regioni]
	setDT(dataReg)
	for(i in 1:length(regioni)){
		reg <- regioni[i]
		lag <- lagRegioni[which(names(lagRegioni)==reg)]
		dataReg[regione==reg, datanew:=data+lag]
	}

  p <- ggplot(dataReg) + my_ggtheme() +
        suppressWarnings(geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
          aes(x=datanew, y=VAR2PLOT, color=regione,
          text = paste('Regione:', regione, '<br>Data:', strftime(data, format="%d-%m-%Y"),
           '<br>Casi: ', VAR2PLOT)))) +
				geom_point( aes(x=datanew, y=VAR2PLOT, color=regione)) +
        scale_color_manual(values=color_regioni) +
        theme(legend.title=element_blank())+theme(axis.text.x=element_text(angle=45, hjust=1)) +
				guides(fill=guide_legend(title="regione")) +
        xlab("")+ylab(var2plotNew)
  if(reacval$mobile){
    p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
  else{
    p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")
  }
  plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

  if(reacval$mobile){

    plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
          layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
  }
  # else{
  #   plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))
  # }
  plot

})

#-----------------------------------------------------
output$inputRegioniCasiVsNuovicasi <- renderUI ({
	if(verbose) cat("\n renderUI:inputRegioniCasiVsNuovicasi")
	dataRange <- reacval$dateRange_reg
		ndays <- 7
	if(is.null(dataRange)) return(NULL)
	fluidRow(style="padding:20px;background-color:#ffffff",
		column(width=4,
			pickerInput(inputId = "selezionaRegioniCasiVsNuoviCasi", label = "Seleziona regioni", choices = regioniList, selected=regioni2fit, options = pickerOptions(size=10,actionsBox = TRUE,maxOptionsText=HTML('') ,selectedTextFormat = "count >20", deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
		),
		column(width=3,
			selectizeInput("variabileRegioniCasiVsNuoviCasi", label="Variabile da analizzare"	, choices=c("totale_casi", "deceduti"), selected = "totale_casi", multiple=FALSE)
		),
		column(width=4,
			sliderInput("dataRegioniCasiVsNuoviCasi", "Giorno:",
			min = min(dataRange) +ndays+ 1, max = max(dataRange),
			value = max(dataRange), animate = animationOptions(interval = 500), timeFormat="%b %d")
		),
	)
})

output$lineRegioniCasiVsNuovicasi <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineRegioniCasiVsNuovicasi")
  allDataReg 		<- copy(reacval$dataTables_reg)
#	allDataReg <- copy(res)
	regioni2plot 	<- input$selezionaRegioniCasiVsNuoviCasi
	dataMax 			<- input$dataRegioniCasiVsNuoviCasi
	var2plot 			<- input$variabileRegioniCasiVsNuoviCasi

	cat("0")
	if(is.null(var2plot)) return(NULL)
	var2plotNew <- gsub("_", " ", var2plot)
	ndays <- 7

	cat("1")
	if (is.null(allDataReg)) return(NULL)
	if (is.null(var2plot)) return(NULL)
	if (is.null(dataMax)) dataMax <- max(allDataReg)
	if (is.null(regioni2plot)) return(NULL)

		cat("2")
	setDT(allDataReg)
	allDataReg <- allDataReg[ denominazione_regione %in% regioni2plot, ]
	setorder(allDataReg, denominazione_regione, data)

	setnames(allDataReg, old=c(var2plot), new=c( 'VAR2PLOT'))
	allDataReg[, casi_nuovi:= c(0,diff(VAR2PLOT)),denominazione_regione]
	allDataReg[, casi_roll:=c(rep(NA,ndays-1), rollsum(casi_nuovi, k=ndays)),denominazione_regione]
	setorder(allDataReg, -data)

	allDataReg <- allDataReg[ !is.na(casi_roll) & casi_roll>0 , .(data, denominazione_regione, casi_roll, VAR2PLOT)]
	xra <- range(allDataReg$VAR2PLOT)
	yra <- range(allDataReg$casi_roll)
	allDataReg <- allDataReg[ data <= dataMax, ]

	validate(need(nrow(allDataReg)>0,paste0("Nessun dato corrispondente alla selezione")))

	p <- ggplot(allDataReg[ !is.na(casi_roll)]) + my_ggtheme() +
					geom_point( aes(x=VAR2PLOT, y=casi_roll, color=denominazione_regione)) +
					geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
					aes(x=VAR2PLOT, y=casi_roll,color=denominazione_regione,
					text = paste('Regione:', denominazione_regione, '<br>Totali:', VAR2PLOT,
					paste0("<br>Casi ultimi ",ndays," giorni"), casi_roll)))+
				scale_color_manual(values=color_regioni) +
				theme(legend.title=element_blank())+#theme(axis.text.x=element_text(angle=45, hjust=1)) +
				guides(fill=guide_legend(title="regione")) +
				xlab("Totali")+ylab(paste0("ultimi ",ndays," giorni"))+
				coord_cartesian(xlim =xra, ylim = yra)+
				scale_y_log10() + scale_x_log10()
        plot<-ggplotly(p)
            if(reacval$mobile){

              plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
                    layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
            }
				plot

})
#-----------------------------------------------------


## PROVINCE
## #-----------------------------------------------------
output$inputProvinceCasiVsNuovicasi <- renderUI ({
	if(verbose) cat("\n renderUI:inputProvinceCasiVsNuovicasi")
	if(!exists('provinceList')) provinceList <- sort(unique(allData_prv$denominazione_provincia))
	if(!exists('province2fit')) province2fit <- c('Bergamo', 'Brescia', 'Milano')

	dataRange <- reacval$dateRange_prv
	ndays <- 7
	if(is.null(dataRange)) return(NULL)
	fluidRow(style="padding:20px;background-color:#ffffff",
		column(width=4,
			pickerInput(inputId = "selezionaProvinceCasiVsNuoviCasi", label = "Seleziona province", choices = provinceList, selected=province2fit, options = pickerOptions(size=10,actionsBox = TRUE,maxOptionsText=HTML('') ,selectedTextFormat = "count >20", deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
		),
#		column(width=3, non ci sono i deceduti
#			selectizeInput("variabileProvinceCasiVsNuoviCasi", label="Variabile da analizzare"	, choices=c("totale_casi", "deceduti"), selected = "totale_casi", multiple=FALSE)
#		),
		column(width=4,
			sliderInput("dataProvinceCasiVsNuoviCasi", "Giorno:",
			min = min(dataRange) +ndays+ 1, max = max(dataRange),
			value = max(dataRange), animate = animationOptions(interval = 500), timeFormat="%b %d")
		),
	)
})

output$lineProvinceCasiVsNuovicasi <- renderPlotly({
	if(verbose) cat("\n renderPlotly:lineProvinceCasiVsNuovicasi")
  allDataPrv 		<- copy(reacval$dataTables_prv)
	prov2plot 	<- input$selezionaProvinceCasiVsNuoviCasi
	dataMaxxi 			<- input$dataProvinceCasiVsNuoviCasi
	var2plot 			<- "totale_casi"

	if(is.null(var2plot)) return(NULL)
	if(is.null(prov2plot)) return(NULL)
	var2plotNew <- gsub("_", " ", var2plot)
	ndays <- 7

	if(verbose) cat("\t dataMaxxi:", dataMaxxi)
	if(verbose) print(str(dataMaxxi))
	if (is.null(allDataPrv)) return(NULL)
	if (is.null(var2plot)) return(NULL)
	if (is.null(dataMaxxi)) {
		dataMaxxi <- max(allDataPrv$data)
	}

	setDT(allDataPrv)
	allDataPrv <- allDataPrv[ denominazione_provincia %in% prov2plot, ]


	setorder(allDataPrv, denominazione_provincia, data)

	setnames(allDataPrv, old=c(var2plot), new=c( 'VAR2PLOT'))
	allDataPrv[, casi_nuovi:= c(0,diff(VAR2PLOT)),denominazione_provincia]
	allDataPrv[, casi_roll:=c(rep(NA,ndays-1), rollsum(casi_nuovi, k=ndays)),denominazione_provincia]
	setorder(allDataPrv, -data)

	allDataPrv <- allDataPrv[ !is.na(casi_roll)  & casi_roll>0, .(data, denominazione_provincia, casi_roll, VAR2PLOT)]


	xra <- range(allDataPrv$VAR2PLOT)
	yra <- range(allDataPrv$casi_roll)
	allDataPrv <- allDataPrv[ data <= dataMaxxi, ]

	validate(need(nrow(allDataPrv)>0,paste0("Nessun dato corrispondente alla selezione")))

	p <- ggplot(allDataPrv[ !is.na(casi_roll)]) + my_ggtheme() +
					geom_point( aes(x=VAR2PLOT, y=casi_roll, color=denominazione_provincia)) +
					geom_line(group=1, # group=1 serve per aggirare un bug di ggplotly con tooltip = c("text")
					aes(x=VAR2PLOT, y=casi_roll,color=denominazione_provincia,
					text = paste('Provincia:', denominazione_provincia, '<br>Totali:', VAR2PLOT,
					paste0("<br>Casi ultimi ",ndays," giorni"), casi_roll)))+
				scale_color_manual(values=color_province) +
				theme(legend.title=element_blank())+#theme(axis.text.x=element_text(angle=45, hjust=1)) +
				guides(fill=guide_legend(title="province")) +
				coord_cartesian(xlim =xra, ylim = yra)+
				xlab("Totali")+ylab(paste0("ultimi ",ndays," giorni"))+
				scale_y_log10() + scale_x_log10()

        plot<-ggplotly(p)
            if(reacval$mobile){

              plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
                    layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
            }
        plot
})
#-----------------------------------------------------

output$updatePrvUI <- renderUI({
	if(verbose) cat("\n renderUI:updatePrvUI")
  h3(paste("Dati aggiornati al giorno:", get_last_date()))
})

output$lineProvince <- renderPlotly({
#  withProgress({


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
          scale_color_manual(values=color_province) +
          theme(axis.text.x=element_text(angle=45, hjust=1)) +
          labs(x="")
          if(reacval$mobile){
            p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
          else{
            p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")+theme(legend.title=element_blank())
          }
    plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

    if(reacval$mobile){

      plot<-plot%>%layout(legend=list(orientation='h',x=-0,y=-0.3))%>%
            layout(legend=list(font=list(size=12)),dragmode=F,autosize = T,heigth=3000,width = 600)
    }

    plot
  }
  #},value=0,message="Caricamento ",detail="")

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
		allDataPrv$`casi su 10mila abit` <- round(allDataPrv$`casi su 10mila abit`,2)

    datatable(allDataPrv,extensions = c('Scroller'),
      selection = list(target = NULL),
      options= c(list(dom = 't',scroller=T,scrollX="300",scrollY="300",paging = T,paging = F, searching = F, info=F, ordering=T, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})



output$selProvince <- renderUI({
  allDataPrv <- reacval$dataTables_prv
  if (!is.null(allDataPrv)) {
		if(animazione){
			fluidRow(
				selectInput("regionSel", label="Seleziona regione", choices=regioniList, selected = "Lombardia"),
	      sliderInput("giornoPrv", "Giorno:",
        min = min(allDataPrv$data) + 1, max = max(allDataPrv$data),
        value = max(allDataPrv$data), animate = animationOptions(interval = 1500), timeFormat="%b %d")
    	)
		} else {
			fluidRow(
				selectInput("regionSel", label="Seleziona regione", choices=regioniList, selected = "Lombardia")
    	)
		}
  }
})

palProvince <- reactive({
	if(verbose) cat("\n reactive:palProvince")
	indDataMax <- which(reacval$dataTables_prv$totale_casi > 0)
	colorNumeric("YlOrRd", domain = log10(c(1, (reacval$dataTables_prv$totale_casi[indDataMax] ))))
})


observe({
	if(verbose) cat("\n observe:animaProvince")
	if (animazione){
	  myGiorno <- input$giornoPrv
	  myReg <- isolate(input$regionSel)
	  allDataPrv <- copy(reacval$dataTables_prv)

	  if (!is.null(allDataPrv) & !is.null(myGiorno) & !is.null(myReg)) {
	    allDataPrv <- allDataPrv[allDataPrv$denominazione_regione == myReg,]
	    allDataPrv <- allDataPrv[allDataPrv$data == myGiorno,]
	    allDataPrv$totale_casi[allDataPrv$totale_casi==0] <- NA_integer_
	    pltProvince <- merge(province, allDataPrv[,c("codice_provincia", "totale_casi")], by.x="COD_PROV", by.y="codice_provincia")
			pal <- palProvince()
			#pal <- colorNumeric("YlOrRd", domain = log10(pmax(1,allDataPrv$totale_casi)))
	    leafletProxy(mapId="mapProvince", data=pltProvince) %>% clearShapes() %>%
	        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
	             label = ~paste(DEN_UTS, "- casi:", totale_casi))
  		}
		} else if(verbose) cat("\t non eseguito")
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
		pal <- palProvince()
		if(scalaSingolaProvincia) pal <- colorNumeric("YlOrRd", domain = log10(pmax(1,allDataPrv$totale_casi)))

		if (animazione){
    suppressWarnings(
			out <- leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        # i poligoni li mette l'observe sopra... se li mettiamo anche qui, sfarfalla all'avvio
        #addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
        #         label = ~paste(DEN_UTS, "- casi:", totale_casi)) %>%
        addLegend(pal = pal, values = ~log10(pmax(1,allDataPrv$totale_casi)), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomright',
                title = paste0("casi"))
		)} else {suppressWarnings(
			out <-leaflet(data = pltProvince, options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% setView(lng=my_frame$reg_long, lat=my_frame$reg_lat, zoom=7)  %>%
        # i poligoni li mette l'observe sopra... se li mettiamo anche qui, sfarfalla all'avvio
        addPolygons(fillColor = ~pal(log10(totale_casi)), weight = 1, stroke = TRUE, color="lightgrey", fillOpacity = .7,
                 label = ~paste(DEN_UTS, "- casi:", totale_casi)) %>%
        addLegend(pal = pal, values = ~log10(pmax(1,allDataPrv$totale_casi)), opacity = 0.7,
                labFormat = labelFormat(transform = function(x) round(10^x), big.mark = "."),
                position = 'bottomright',
                title = paste0("casi"))
		)}
		(out)

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
	} else if(tipoModello=="Gompertz") {
		modelliReg=isolate(reacval$modelliRegGomp)
	} else modelliReg <- isolate(reacval$modelliRegExp)

	if(assignout) assign("modelliRegOut", modelliReg, envir=.GlobalEnv)
	if(assignout) assign("nahead", nahead, envir=.GlobalEnv)

	if (!is.null(allDataReg)) {
		tsReg <- getTimeSeriesReact()
    tsReg["Italia"] <- NULL

		if(assignout) assign("tsReg", tsReg, envir=.GlobalEnv)

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
 #   assign("prevDTvedi",prevDT, envir=.GlobalEnv)

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
       scale_color_manual(values=color_regioni) +# scale_x_date(date_breaks="3 day",date_labels="%b %d") +
			 theme(axis.text.x=element_text(angle=45,hjust=1)) +
       labs(x="")
    if(reacval$mobile){
      p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
    else{
      p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d") +theme(legend.title=element_blank())
    }

    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
    if(reacval$mobile){
        plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0,y=-0.4))
    }
  #   else{
  #     plot<-plot%>%
  # add_annotations( text="<b>Regioni </b>", xref="paper", yref="paper",
  #                 x=1.02, xanchor="left",
  #                 y=0.8, yanchor="bottom",    # Same y as legend below
  #                 legendtitle=TRUE, showarrow=FALSE )%>%layout( legend=list(y=0.8, yanchor="top" ) )
  #   }
    plot
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
	} else if(tipoModello=="Gompertz") {
		modelliIta=isolate(reacval$modelliItaGomp)
	} else modelliIta <- isolate(reacval$modelliItaExp)

	if(assignout) assign("modelItaOut", modelliIta, envir=.GlobalEnv)
	if(assignout) assign("nahead", nahead, envir=.GlobalEnv)

	if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()["Italia"]
		if(saveRDSout) saveRDS(file="prevItaList.RDS",list(tsIta, modelliIta, allDataReg))

		if(assignout) assign("tsIta", tsIta, envir=.GlobalEnv)

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
  if(assignout) assign("allDataRegprova",allDataReg, envir=.GlobalEnv)
	#FIXME per plottare terapie intensiva ed ospedalizzati aggiungere "terapia_intensiva" e "totale_ospedalizzati"
	varPrev <- c("totale_casi", "deceduti" )

  if (!is.null(allDataReg)) {
		tsIta <- getTimeSeriesReact()$Italia
		if(saveRDSout) saveRDS(file="fitItaList.RDS",list(tsIta, allDataReg))

		prevItaDT <-copy(prevIta())
		setnames(prevItaDT, old=c('Attesi'), new=c('casi'))
    if (assignout) assign("prevItaDT1",prevItaDT, envir=.GlobalEnv)
		prevItaDT <- prevItaDT[ prevItaDT$variabilePrevista %in% varPrev, ]

		setDF(allDataReg)

		dataRDX <- allDataReg[, c('data', varPrev)]
		dataIta <- aggregate(dataRDX[,-1], sum, by=list(data=dataRDX$data))

    tmp <- data.frame(
    		data=rep(unique(dataIta$data), times=length(varPrev)),
    		casi=unlist(lapply(-1, function(x) dataIta[, x])),
    		variabilePrevista=rep(unique(varPrev), each=nrow(dataIta))
    )
    if (assignout) assign("tmp1",tmp, envir=.GlobalEnv)

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
			 scale_color_manual(values=d3hexcols20) +# scale_x_date(date_breaks="3 day",date_labels="%b %d") +
       theme(axis.text.x = element_text(angle=45,hjust=1)) +
       labs(x="", color = "variabile prevista")
       if(reacval$mobile){
         p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
       else{
         p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")+theme(legend.title=element_blank())
       }
    if (tipoGraph == "Logaritmico") p <- p + scale_y_log10()

    plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
    if(reacval$mobile){
      plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0.01,y=-0.4))
      }
      else{
        plot<-plot%>%
    add_annotations( text="<b>Variabile prevista </b>", xref="paper", yref="paper",
                    x=1.02, xanchor="left",
                    y=0.8, yanchor="bottom",    # Same y as legend below
                    legendtitle=TRUE, showarrow=FALSE )%>%layout( legend=list(y=0.8, yanchor="top" ) )
      }
    plot
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
		#prevDT[prevDT$variabilePrevista == "totale_casi",]
  }
})


output$fitCasesIta <- renderPlotly({
  if(verbose) cat("\n renderPlotly:fitCasesIta")
  prevItaDT <- copy(prevItaLongTerm())
  tsIta <- copy(getTimeSeriesReact()[["Italia"]])
  varInput<-input$varSel2
  testoLegenda<-varInput
  if (varInput=="totale contagiati") varInput<-"totale_casi"

  if (!is.null(prevItaDT) & !is.null(tsIta)) {
  		setnames(prevItaDT, old=c('Attesi'), new=c('casi'))
      prevItaDT<-prevItaDT[prevItaDT$variabilePrevista == varInput,]
      #setnames(tsIta, old='casi', new=c('totale_casi'))
      setnames(tsIta, old=varInput, new=c('casi'))
      num_rows <- nrow(tsIta)
      datiIta <- rbind(tsIta[, c("data", "casi")], prevItaDT[, c("data", "casi")])
      datiIta$tipo <- c(rep("osservazioni", num_rows), rep("previsioni", nrow(datiIta) - num_rows))
      datiIta$tipo <- factor(datiIta$tipo, levels=c("osservazioni", "previsioni"))

			indmax <- which.max(datiIta$casi)
      vdate <- datiIta$data[indmax]
		  datiIta <-datiIta[datiIta$data <= vdate,]
      datiIta$label <- c(rep("", nrow(datiIta)-1), "picco\n previsto")

      p <- ggplot() + my_ggtheme() +
  					suppressWarnings(geom_bar(data=datiIta, aes(x=data, y=casi, fill=tipo,
              text = paste('Data:', strftime(data, format="%d-%m-%Y"),
               '<br>Casi: ', round(casi))), stat="identity", width = 0.8))+
  #          geom_text(data=datiIta, aes(x=data, y=casi, label=label), cex=2.5, color="black", fontface = "bold") +
  					scale_fill_manual(values=d3hexcols)+
          #  scale_x_date(date_breaks="2 day",date_labels="%b %d")+
            theme(axis.text.x=element_text(angle=45,hjust=1)) +
            labs(x="", y="Numero casi totali") +
            theme(#legend.title = element_blank()
          )+
          guides(fill=guide_legend(title=testoLegenda))


            if(reacval$mobile){
              p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
            else{
              p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")+theme(legend.title=element_blank())
            }
      plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
      if(reacval$mobile){
        plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0,y=-0.2))
      }
      else{
        plot<-plot%>%
 add_annotations( text=paste0('<b>',testoLegenda,"</b>"), xref="paper", yref="paper",
                 x=1, xanchor="left",
                 y=0.8, yanchor="bottom",    # Same y as legend below
                 legendtitle=TRUE, showarrow=FALSE ) %>%
 layout( legend=list(y=0.8,x=1, yanchor="top" ) )
      }
      plot
   }
})

#funzione che mette insieme i dati osservati e le previsioni, comprensive di incertezze, creando un solo df
creaDFossEprev<-reactive({
  if(verbose) cat("\n creaDF")
  allDataReg <- copy(reacval$dataTables_reg)
  tsIta <- getTimeSeriesReact()$Italia
  prevItaDT <-copy(prevIta())
  setnames(prevItaDT, old=c('Attesi'), new=c('totale_casi'))
  prevItaDT$tipo<-"previsioni"
  #assign("prevItaDT1",prevItaDT, envir=.GlobalEnv)
  setDF(allDataReg)
  varPrev <- unique(prevItaDT$variabilePrevista)

  dataRDX <- allDataReg[, c('data', varPrev)]
  dataIta <- aggregate(dataRDX[,2:5], sum, by=list(data=dataRDX$data))

  tmp <- data.frame(
      data=rep(unique(dataIta$data), times=length(varPrev)),
      totale_casi=unlist(lapply(2:5, function(x) dataIta[, x])),
      variabilePrevista=rep(unique(varPrev), each=nrow(dataIta)),      stringsAsFactors=FALSE
  )
  tmp$UpperRange<-tmp$totale_casi
  tmp$LowerRange<-tmp$totale_casi
  tmp$tipo<-"osservazioni"

  datamax<-max(tmp$data)
  df<-rbind(tmp,prevItaDT[which( prevItaDT$data>datamax),c("data", "totale_casi", "variabilePrevista", "UpperRange", "LowerRange", "tipo")])
  #assign("df1",df, envir=.GlobalEnv)
  df

})

output$percDeltaTot <- renderPlotly({
  if(verbose) cat("\n renderPlotly:percDeltaTot")

  df<-creaDFossEprev()

  varInput<-input$varSel
  testoLegenda<-varInput
  if (varInput=="totale contagiati") varInput<-"totale_casi"
  subdf<-df[which(df$variabilePrevista==varInput),]

  subdf$deltaPerc<-c(NA,diff(subdf$totale_casi))/c(NA,subdf$totale_casi[1:(nrow(subdf)-1)])*100

  # subdf$UpperRangePerc<-(c(NA,subdf$UpperRange[2:nrow(subdf)])-subdf$totale_casi)/c(NA,subdf$totale_casi[1:(nrow(subdf)-1)])*100
  #
  # subdf$LowerRangePerc<- -(c(NA,subdf$LowerRange[2:nrow(subdf)])-subdf$totale_casi)/c(NA,subdf$totale_casi[1:(nrow(subdf)-1)])*100

  subdf$UpperRangePerc<-c(NA,diff(subdf$UpperRange))/c(NA,subdf$totale_casi[1:(nrow(subdf)-1)])*100

  subdf$LowerRangePerc<-c(NA,diff(subdf$LowerRange))/c(NA,subdf$totale_casi[1:(nrow(subdf)-1)])*100

  p <- ggplot() + my_ggtheme() +
          suppressWarnings(geom_bar(data=subdf, aes(x=data, y=deltaPerc,
          fill=tipo,
          text = paste('Data:', strftime(data, format="%d-%m-%Y"),
          '<br>Variazione: ', paste(round(deltaPerc,1),'%'))), stat="identity", width = 0.8))+
#   			geom_crossbar(data=subdf[which(subdf$tipo=="previsioni"),],
#            aes(x=data,
#              y=deltaPerc,
#              ymin=LowerRangePerc, ymax=UpperRangePerc,
#            text = paste('Data:', strftime(data, format="%d-%m-%Y"),
#          '<br>Variabile: ', varInput,
#          '<br>Variazione prevista: ', paste0(round(deltaPerc,1),'%'),
#          '<br>Intervallo previsione:', paste0('[', round(LowerRangePerc,1), '%, ', round(UpperRangePerc,1),'%]')
#        )),width=0.7,
#            colour="red",
#            alpha=0.4,
#            size=0.3,
#            position=position_dodge(.9)
 #         )+
          scale_fill_manual(values=d3hexcols)+
          #scale_x_date(date_breaks="2 day",date_labels="%b %d")+
          theme(axis.text.x=element_text(angle=45,hjust=1)
          #,legend.title = element_blank()
        )+
          labs(x="", y="Variazione %")#+
          # guides(fill=guide_legend(title=testoLegenda))
          if(reacval$mobile){
            p<-p+scale_x_date(date_breaks="3 day",date_labels="%b %d")}
          else{
            p<-p+scale_x_date(date_breaks="2 day",date_labels="%b %d")+theme(legend.title=element_blank())
          }
  plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
      if(reacval$mobile){
        plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0,y=-0.2))
      }
      else{
        plot<-plot%>%
 add_annotations( text=paste0('<b>',testoLegenda,"</b>"), xref="paper", yref="paper",
                 x=1, xanchor="left",
                 y=0.8, yanchor="bottom",    # Same y as legend below
                 legendtitle=TRUE, showarrow=FALSE ) %>%
 layout( legend=list(y=0.8,x=1, yanchor="top" ) )
      }
       plot
  # }
})

#FIXME stoqui
getAsintotoGomp <- reactive({
	if(verbose) cat("\n reactive:getAsintotoGomp")

	dirPastMod 	<- "www/pastModels/"
	mindata			<- as.Date('2020-04-23')

	oldMod <- list.files(dirPastMod)
	oldItaGomp	<- oldMod[grep("ItaGomp", oldMod, ignore.case=T)]
	date <- as.Date(substr(oldItaGomp, 16, 25))

	ind2read <- which(date >= mindata)
	pathItaGomp <- paste0(dirPastMod,oldItaGomp )[ind2read]
	modItaGomp <- lapply(pathItaGomp, readRDS)
	names(modItaGomp) <- date[ind2read]

	outModel <- lapply(modItaGomp, function(x){
		if(class(x$deceduti) == 'lm') {
			dec <- NA
		} else dec <- coefficients(x$deceduti)['a']

		if(class(x$deceduti) == 'lm') {
			tot <- NA
		} else tot <- coefficients(x$totale_casi)['a']
		data.frame(totale_casi=tot, deceduti=dec)
	})

	asintoto <- rbindlist(outModel)
	asintoto$data <- as.Date(names(outModel))
	asintoto
})


output$tabAsintoto <- renderDT({
	if(verbose) cat("\n renderDT:tabAsintoto")
  asintoto <- getAsintotoGomp()
	if(is.null(asintoto)) return(NULL)


	asintoto$'Variazione Deceduti' <- paste0(round(c(NA,diff(asintoto$deceduti))/asintoto$deceduti*100, 2), "%")
	asintoto$'Variazione Totale Casi' <- paste0(round(c(NA,diff(asintoto$totale_casi))/asintoto$totale_casi*100, 2), "%")

	asintoto$deceduti <- format(round(asintoto$deceduti), big.mark="'")
	asintoto$totale_casi <- format(round(asintoto$totale_casi), big.mark="'")

	setnames(asintoto, old=c( 'totale_casi', 'deceduti'), new=c("Totale Casi", "Deceduti"))

	setorder(asintoto, -data)


 	datatable(asintoto[, c('data', "Totale Casi", 'Variazione Totale Casi', "Deceduti", 'Variazione Deceduti' )],extensions = c('Scroller'),
      selection = list(target = NULL),
      options= c(list(dom = 't',scroller=T,scrollX="300",scrollY="300",paging = T, searching = F),
      rownames=F))

		#asintoto[, c('data', "Totale Casi", 'Variazione Totale Casi', "Deceduti", 'Variazione Totale Casi' )]

})

output$plotAsintoto <- renderPlotly({
	if(verbose) cat("\n renderPlotly:plotAsintoto")
  asintoto <- getAsintotoGomp()

	tab1 <- data.frame(data=asintoto$data, casi=asintoto$deceduti, var="deceduti", stringsAsFactors=F)
	tab2 <- data.frame(data=asintoto$data, casi=asintoto$totale_casi, var="totale_casi", stringsAsFactors=F)

	asi2plot <- rbind(tab2,tab1)

	p <- ggplot() + my_ggtheme() +
				suppressWarnings(
					geom_line(data=asi2plot, linetype=1, group=1,
						aes(x=data, y=casi, color=var,
							text = paste('Data:', strftime(data, format="%d-%m-%Y"),
							 '<br>Variabile: ', var,
							 '<br>Casi (fit): ', round(casi)
							 )
						)
					)
				)+
				suppressWarnings(
					geom_point(data=asi2plot, shape=22,
						aes(x=data, y=casi, color=var,
							text = paste('Data:', strftime(data, format="%d-%m-%Y"),
							 '<br>Variabile: ', var,
							 '<br>Casi (fit): ', round(casi)
							 )
						)
					)
				)

	p

})


# output$percDeltaTot <- renderPlotly({
#   if(verbose) cat("\n renderPlotly:percDeltaTot")
#   prevItaDT <- copy(prevItaLongTerm())
#   tsIta <- copy(getTimeSeriesReact()[["Italia"]])
#   df<-creaDFossEprev()
#     tsIta$deltaPerc<-c(NA,diff(tsIta$totale_casi))/c(NA,tsIta$totale_casi[1:(nrow(tsIta)-1)])*100
#     assign("tsItaprova",tsIta, envir=.GlobalEnv)
#
#     prevItaDT <-copy(prevIta())
#     setnames(prevItaDT, old=c('Attesi'), new=c('totale_casi'))
#     prevItaDT$deltaPerc<-c(NA,diff(prevItaDT$totale_casi))/c(NA,prevItaDT$totale_casi[1:(nrow(prevItaDT)-1)])*100
#
#     prevItaDT$UpperRangePerc<-c(NA,diff(prevItaDT$UpperRange))/c(NA,prevItaDT$totale_casi[1:(nrow(prevItaDT)-1)])*100
#
#     prevItaDT$LowerRangePerc<-c(NA,diff(prevItaDT$LowerRange))/c(NA,prevItaDT$totale_casi[1:(nrow(prevItaDT)-1)])*100
#
#     assign("prevItaDTprova",prevItaDT, envir=.GlobalEnv)
#
#   datamax<-max(tsIta$data)
#   tmp<-rbind(tsIta[,c("data","totale_casi","deltaPerc")],prevItaDT[which(prevItaDT$variabilePrevista=="totale_casi" & prevItaDT$data>datamax),c("data","totale_casi","deltaPerc")])
#
#   tmp$tipo<-NA
#   tmp$tipo[which(tmp$data<=datamax)]<-'osservata'
#   tmp$tipo[which(tmp$data>datamax)]<-'predetta'
#   assign("tmpprova",tmp, envir=.GlobalEnv)
#
#   p <- ggplot() + my_ggtheme() +
#           suppressWarnings(geom_bar(data=tmp, aes(x=data, y=deltaPerc,
#           fill=tipo,
#           text = paste('Data:', strftime(data, format="%d-%m-%Y"),
#           '<br>Variazione: ', paste(round(deltaPerc,1),'%'))), stat="identity", width = 0.8))+
#    				# suppressWarnings(geom_bar(data=tsIta, aes(x=data, y=deltaPerc, #fill=tipo,
#           # text = paste('Data:', strftime(data, format="%d-%m-%Y"),
#           # '<br>Variazione: ', paste(round(deltaPerc,2),'%'))), stat="identity", width = 0.8, fill="steelblue"))+
#           # suppressWarnings(geom_bar(data=prevItaDT[which(prevItaDT$data>datamax & prevItaDT$variabilePrevista=="totale_casi"),], aes(x=data, y=deltaPerc, #fill=tipo,
#           # text = paste('Data:', strftime(data, format="%d-%m-%Y"),
#           # '<br>Variazione prevista: ', paste0(round(deltaPerc,2),'%'))), stat="identity", width = 0.8, fill="orange"))+
#           geom_crossbar(data=prevItaDT[which(prevItaDT$data>datamax & prevItaDT$variabilePrevista=="totale_casi"),],
#             aes(x=data,
#               y=deltaPerc,
#               ymin=LowerRangePerc, ymax=UpperRangePerc,
#             text = paste('Data:', strftime(data, format="%d-%m-%Y"),
#           #'<br>Variabile: ', variabilePrevista,
#           '<br>Variazione prevista: ', paste0(round(deltaPerc,1),'%'),
#           '<br>Intervallo previsione:', paste0('[', round(LowerRangePerc,1), '%, ', round(UpperRangePerc,1),'%]')
#         )),width=0.7,
#             colour="orange",
#             #alpha=0.6, size=1,
#             position=position_dodge(.9)
#           )+
#           scale_fill_manual(values=d3hexcols)+
#           scale_x_date(date_breaks="2 day",date_labels="%b %d")+
#           theme(axis.text.x=element_text(angle=45,hjust=1),legend.title = element_blank())+
#           labs(x="", y="Variazione %")
#
#   plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
#   #     if(reacval$mobile){
#   #       plot<-plot%>%layout(legend=list(orientation='h',x=0,y=-0.2))
#   #     }
#        plot
#   # }
# })

output$terapiaIntStoricoTot<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntStoricoTot")
	selregione <- input$regionSelSerieStoricheTI
	tipoplot <- input$varSelTI
	if(is.null(selregione)) return(NULL)
	if(is.null(tipoplot)) return(NULL)

	dati <- copy(allData_reg)
	if(is.null(dati)) return(NULL)
	setDT(dati)
	setnames(dati, old='denominazione_regione', new="regione")
	dati <-dati[regione %in% selregione]
	setorder(dati, regione, data)
#dati[, casi_nuovi:= c(0,diff(totale_casi)),regione]
	if(tipoplot=="terapia intensiva"){
		dati[, numero_pazienti:= terapia_intensiva,regione]
	} else {
		dati[, numero_pazienti:= totale_ospedalizzati,regione]
	}

	p <- 	ggplot(dati) + my_ggtheme() +
				geom_bar(aes(y = numero_pazienti, x = data, fill=regione), stat="identity") +
		 	scale_fill_manual(values=color_regioni) +
			guides(fill=guide_legend(title="regione")) +
			theme(axis.text.x=element_text(angle=45, hjust=1)) +
			ylab("Casi Totali")
	p

})


output$nuoviPositiviStoricoReg<- renderPlotly({
	if(verbose) cat("\n renderPlotly:nuoviPositiviStoricoReg")
	selregione <- input$regionSelSerieStorichexReg
	varplot 	<- input$varSelSerieStoricheReg
	tipoplot 	<- input$tipoPlotSerieStoricheReg
	if(is.null(selregione)) return(NULL)
	if(is.null(varplot)) return(NULL)
	if(is.null(tipoplot)) return(NULL)

	dati <- copy(allData_reg)

	if(is.null(dati)) return(NULL)
	setDT(dati)
	setnames(dati, old=c('denominazione_regione'), new=c("regione"))
	dati <-dati[regione %in% selregione]


	setorder(dati, regione, data)
	if(varplot=="nuovi casi") {
		dati[, casi_nuovi:= c(0,diff(totale_casi)),regione]
	} else dati[, casi_nuovi:= c(0,diff(deceduti)),regione]

	if(tipoplot=="totale") {
		dati <- dati[, .(casi_nuovi=sum(casi_nuovi, na.rm=T)),data]
		dati <- dati[, regione:="Totale regioni selezionate"]
	}

	p <- 	ggplot(dati) + my_ggtheme() +
			geom_bar(aes(y = casi_nuovi, x = data, fill=regione), stat="identity") +
			guides(fill=guide_legend(title="regione")) +
			theme(axis.text.x=element_text(angle=45, hjust=1)) +
			ylab("")


			if(tipoplot!="totale") p <- p + scale_fill_manual(values=color_regioni)
	p

})

output$uiProvSelSerieStoricheProv <- renderUI({
	if(verbose) cat("\n renderPlotly:uiProvSelSerieStoricheProv")
	selregione <- input$regionSelSerieStorichexProv
	if(is.null(selregione)) return(NULL)

	prov <- unique(allData_prv[allData_prv$denominazione_regione %in% selregione, "denominazione_provincia"])
	prov <- sort(prov)

	pickerInput(inputId = "provSelSerieStoricheProv", label = "Seleziona province", choices = prov,selected=prov, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)

})


output$nuoviPositiviStoricoProv<- renderPlotly({
	if(verbose) cat("\n renderPlotly:nuoviPositiviStoricoProv")
#	selregione <- input$regionSelSerieStorichexProv
	selprov <- input$provSelSerieStoricheProv
	tipoplot <- input$tipoPlotSerieStorichePrev
#	if(is.null(selregione)) return(NULL)
	if(is.null(tipoplot)) return(NULL)
	if(is.null(selprov)) return(NULL)

	dati <- copy(allData_prv)
	if(is.null(dati)) return(NULL)
	setDT(dati)
	setnames(dati, old=c('denominazione_regione', 'denominazione_provincia'), new=c("regione", "provincia"))

#	dati <-dati[regione %in% selregione]
	dati <-dati[provincia %in% selprov]

	setorder(dati, provincia, data)
	dati[, casi_nuovi:= c(0,diff(totale_casi)),provincia]

	if(tipoplot=="totale") {
		dati <- dati[, .(casi_nuovi=sum(casi_nuovi, na.rm=T)),data]
		dati <- dati[, provincia:="Totale province selezionate"]
	}

	p <- 	ggplot(dati) + my_ggtheme() +
				geom_bar(aes(y = casi_nuovi, x = data, fill=provincia), stat="identity") +
			guides(fill=guide_legend(title="provincia")) +
			theme(axis.text.x=element_text(angle=45, hjust=1)) +
			ylab("Casi Totali")

	if(tipoplot!="totale")	p <- p + scale_fill_manual(values=color_province)
	p

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
					geom_hline(yintercept=100, linetype="dashed", color = "lightgrey")+
          labs(x="", y="% letti occupati per CoVid19")
	p<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

  if(reacval$mobile){
    p<-p%>%layout(dragmode=F)
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
  plot<- ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')
  if(reacval$mobile){
    plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0.6,y=-0.4))

  }
  plot
  # if(reacval$mobile){
  #   p<- p+coord_flip()
  # }

})

output$terapiaIntPlotPercPrev<- renderPlotly({
	if(verbose) cat("\n renderPlotly:terapiaIntPlotPercPrev")

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
  plot<-ggplotly(p, tooltip = c("text")) %>% config(locale = 'it')

  if(reacval$mobile){
    plot<-plot%>%layout(dragmode=F,legend=list(orientation='h',x=0.6,y=-0.4))

  }
  plot

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
	date <- sort(date[-which.max(date)], decreasing=T)
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

	if(assignout) assign("tsIta",tsIta, envir=.GlobalEnv)
	if(assignout) assign("dataMod",dataMod, envir=.GlobalEnv)

	pathModIta    <-paste0("www/pastModels/modelliIta_", dataMod, ".RDS")
	pathModItaExp <-paste0("www/pastModels/modelliItaExp_", dataMod, ".RDS")
	pathModItaGomp <-paste0("www/pastModels/modelliItaGomp_", dataMod, ".RDS")


	err <-0
	modelliItaPast <- readRDS(pathModIta)
	modelliItaExpPast  <- readRDS(pathModItaExp)
	modelliItaGompPast  <- try(readRDS(pathModItaGomp))
	if(class(modelliItaGompPast)=="try-error"){
		modelliItaGompPast <- modelliItaPast
		err <- 1
	}

	if(tipoVariazione=='Totale'){

		vero <-tsIta$Italia[tsIta$Italia$data==(dataMod+1), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')]

		tsIta$Italia <- tsIta$Italia[ tsIta$Italia$data<=dataMod, ]

	  prevDT <- get_predictions(modelliItaPast, tsIta, nahead=1, alldates=FALSE)
		prevDTexp <- get_predictions(modelliItaExpPast, tsIta, nahead=1, alldates=FALSE)
		prevDTgomp <- try(get_predictions(modelliItaGompPast, tsIta, nahead=1, alldates=FALSE))
		if(class(prevDTgomp)=="try-error"){
			prevDTgomp <- prevDT
			err <- 2
		}
		prevDT$Modello 		<- "Esp. quadratico"
		prevDTexp$Modello <- "Esponenziale"
		prevDTgomp$Modello <- "Gompertz"

		prevDT$Osservato 		<- unlist(vero)
		prevDTexp$Osservato  <- unlist(vero)
		prevDTgomp$Osservato  <- unlist(vero)

		if(err>0){
			prevDTgomp$LowerRange	<- NA
			prevDTgomp$UpperRange	<- NA
			prevDTgomp$Attesi	<- NA
		}

		prevDT$'Valore atteso +- incertezza standard' 		<- paste0(format(prevDT$LowerRange, big.mark="'"), ' - ', format(prevDT$UpperRange, big.mark="'"))
		prevDTexp$'Valore atteso +- incertezza standard'  <- paste0(format(prevDTexp$LowerRange, big.mark="'"), ' - ', format(prevDTexp$UpperRange, big.mark="'"))
		prevDTgomp$'Valore atteso +- incertezza standard'  <- paste0(
			format(round(prevDTgomp$LowerRange), big.mark="'"), ' - ',
			format(round(prevDTgomp$UpperRange), big.mark="'")
		)

		setnames(prevDT, old=c('LowerRange', 'UpperRange', 'outName', 'Attesi'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))
		setnames(prevDTexp, old=c('LowerRange', 'UpperRange', 'outName', 'Attesi'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))
		setnames(prevDTgomp, old=c('LowerRange', 'UpperRange', 'outName', 'Attesi'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))

		out <-rbind(
			prevDT[, c('data', 'Modello', 'Variabile', 'Osservato', 'Valore atteso +- incertezza standard', 'Valore atteso')],
			prevDTexp[, c('data', 'Modello', 'Variabile', 'Osservato', 'Valore atteso +- incertezza standard', 'Valore atteso')],
			prevDTgomp[, c('data', 'Modello', 'Variabile', 'Osservato', 'Valore atteso +- incertezza standard', 'Valore atteso')])

		out$Variazione <- paste0 (round((out$Osservato-out$'Valore atteso')/out$Osservato*100, 2), " %")
		out$'Valore atteso' 		<- format(round(out$'Valore atteso'), big.mark="'")
		out$Osservato 		<- format(out$Osservato, big.mark="'")

		out

	} else {
		veroMod <-unlist(tsIta$Italia[tsIta$Italia$data==(dataMod), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')])
		vero <-unlist(tsIta$Italia[tsIta$Italia$data==(dataMod+1), c('totale_casi', 'deceduti', 'totale_ospedalizzati', 'terapia_intensiva')])

		DVero <- vero - veroMod

		tsIta$Italia <- tsIta$Italia[ tsIta$Italia$data<=dataMod, ]

	  prevDT <- get_predictions(modelliItaPast, tsIta, nahead=1, alldates=FALSE)
		prevDTexp <- get_predictions(modelliItaExpPast, tsIta, nahead=1, alldates=FALSE)
		prevDTgomp <- get_predictions(modelliItaGompPast, tsIta, nahead=1, alldates=FALSE)
		if(class(prevDTgomp)=="try-error"){
			prevDTgomp <- prevDT
			err <- 2
		}

		prevDT$Modello 		<- "Esp. quadratico"
		prevDTexp$Modello <- "Esponenziale"
		prevDTgomp$Modello <- "Gompertz"

		prevDT$Osservato 		<- unlist(DVero)
		prevDTexp$Osservato  <- unlist(DVero)
		prevDTgomp$Osservato  <- unlist(DVero)

		prevDT$VarPrev 			<- prevDT$Attesi - veroMod
		prevDTexp$VarPrev  	<- prevDTexp$Attesi - veroMod
		prevDTgomp$VarPrev  	<- prevDTgomp$Attesi - veroMod

		prevDT$VarMin 			<- prevDT$LowerRange - veroMod
		prevDTexp$VarMin  	<- prevDTexp$LowerRange - veroMod
		prevDTgomp$VarMin  	<- prevDTgomp$LowerRange - veroMod

		prevDT$VarMax    <- prevDT$UpperRange - veroMod
		prevDTexp$VarMax <- prevDTexp$UpperRange - veroMod
		prevDTgomp$VarMax <- prevDTgomp$UpperRange - veroMod

		prevDT$Variazione    <- prevDT$VarPrev - DVero
		prevDTexp$Variazione <- prevDTexp$VarPrev - DVero
		prevDTgomp$Variazione <- prevDTgomp$VarPrev - DVero

		if(err<0){
			prevDTgomp$VarMin	<- NA
			prevDTgomp$VarMax	<- NA
			prevDTgomp$VarPrev	<- NA
		}

		prevDT$'Valore atteso +- incertezza standard' 		<- paste0(format(prevDT$VarMin, big.mark="'"), ' - ', format(prevDT$VarMax, big.mark="'"))
		prevDTexp$'Valore atteso +- incertezza standard'  <- paste0(format(prevDTexp$VarMin, big.mark="'"), ' - ', format(prevDTexp$VarMax, big.mark="'"))
		prevDTgomp$'Valore atteso +- incertezza standard'  <- paste0(
			format(round(prevDTgomp$VarMin), big.mark="'"), ' - ',
			format(round(prevDTgomp$VarMax), big.mark="'")
		)

		setnames(prevDT, old=c('VarMin', 'VarMax', 'outName', 'VarPrev'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))
		setnames(prevDTexp, old=c('VarMin', 'VarMax', 'outName', 'VarPrev'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))
		setnames(prevDTgomp, old=c('VarMin', 'VarMax', 'outName', 'VarPrev'), new=c('Minimo', 'Massimo', 'Variabile', 'Valore atteso'))

		outPerc <-rbind(
			prevDT[, c('data', 'Modello', 'Variabile', 'Osservato', 'Valore atteso', 'Valore atteso +- incertezza standard')],
			prevDTexp[, c('data', 'Modello', 'Variabile',  'Osservato', 'Valore atteso', 'Valore atteso +- incertezza standard')],
			prevDTgomp[, c('data', 'Modello', 'Variabile',  'Osservato', 'Valore atteso', 'Valore atteso +- incertezza standard')]
		)

		outPerc$Variazione <- paste0 (round((outPerc$Osservato-outPerc$'Valore atteso')/outPerc$Osservato*100, 2), " %")

		outPerc$'Valore atteso' 		<- format(round(outPerc$'Valore atteso'), big.mark="'")
		outPerc$Osservato 		<- format(round(outPerc$Osservato), big.mark="'")

		outPerc
	}


})


output$tabCompare <- renderDT({
	if(verbose) cat("\n renderDT:tabCompare")
  out <- prevItaCompare()

	var2Keep <- c("deceduti", "totale_casi")
	out <- out[(out$Variabile%in%var2Keep),]

  if (!is.null(out)) {
		out <- out[order(out$Variabile),]
    datatable(out,extensions = c('Scroller'),
      selection = list(target = NULL),
      options= c(list(dom = 't',scroller=T,scrollX="300",scrollY="250",paging = T, searching = F, info=F, ordering=F, order=list(list(2, 'desc'))), DT_lang_opt),
      rownames=F)
  }
})


output$tab_desktop<-renderUI({

  fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
    h1("Previsioni"),
    fluidRow(
      column(12,h4("In questa pagina proponiamo il confronto tra i dati osservati e due modelli di crescita: il modello esponenziale descrive una diffusione in cui il tasso di crescita è costante, questo accade quando l'epidemia si diffonde senza controllo; il modello esponenziale quadratico tiene in conto di una diminuzione del tasso di crescita con l'avanzare del tempo. Questa diminuzione può essere dovuta a misure contenitive o all'esaurimento della popolazione contagiabile."))

    ),
       br(),
      fluidRow(style="padding:30px;background-color:#ffffff",
	      fluidRow(
	        column(4,
	          prettyRadioButtons('regionLinLogFit',"Tipo Grafico",choices = c("Lineare", "Logaritmico"), selected = "Lineare",status = "success",shape = 'round',inline = T,animation = 'jelly',icon = icon('check'))
	          ),
	        column(3,
	          prettyRadioButtons('modelloFit',"Tipologia Modello",choices = c("Gompertz", "Esp. quadratico", "Esponenziale" ), selected="Gompertz",status = "success",shape = 'round',inline = T,animation = 'jelly',icon = icon('check'))
	          )
				),
 			 fluidRow(style="padding:30px;background-color:#ffffff",
           fluidRow(align="center",h3("Andamenti globali in Italia con previsione a 3 giorni")),
           addSpinner(plotlyOutput(outputId="fitIta"), spin = "fading-circle", color = "#009933"),
 					spiegaFitTot
 			 ),

        fluidRow(align="center",h3("Andamento casi positivi per regione con previsione a 3 giorni")),
        fluidRow(style='padding-left:30px',
					pickerInput(inputId = "regionSelFit", label = "Seleziona regioni", choices = regioniList,selected=regioni2fit, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
				),
       	addSpinner(plotlyOutput(outputId="fitRegion"), spin = "fading-circle", color = "#009933"),
			 	spiegaFitPos
			 ),

      br(),br(),

      fluidRow(style="background-color:#ffffff",

          column(10,offset=1,align="center",h3("Variazione percentuale giorno per giorno")),
          fluidRow(style="padding-left:50px;",
            pickerInput(inputId = "varSel", label = "Seleziona variabile", choices = c("deceduti","totale contagiati"),selected="totale contagiati",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE))),
        fluidRow(style="padding:10px;background-color:#ffffff",addSpinner(plotlyOutput(outputId="percDeltaTot"), spin = "fading-circle", color = "#009933"),spiegaVariazionePercentuale),
      br(),br(),
  #    br(),br(),
  #    fluidRow(style="background-color:#ffffff",
  #      column(10,offset=1,align="center", h3("Previsione del numero di casi a medio termine con modello esponenziale quadratico")),
  #      fluidRow(style="padding-left:50px;",
  #        pickerInput(inputId = "varSel2", label = "Seleziona variabile", choices = c("deceduti","totale contagiati"),selected="totale contagiati",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE))),
   #     fluidRow(style="padding:10px;background-color:#ffffff",addSpinner(plotlyOutput(outputId="fitCasesIta"), spin = "fading-circle", color = "#009933"), spiegaFitMedioTermine)
   #   ,br(),
      fluidRow(style="padding:30px;background-color:#ffffff",
          column(width=4,
            selectizeInput("tipoCompare", label="Tipo Comparazione", choices=c("Totale", "Incremento Giornaliero"), selected = "Lineare")
          ),
          column(width=4,
            uiOutput("dateCompare")
          ),
          addSpinner(DTOutput("tabCompare"), spin = "fading-circle", color = "#009933"),spiegaTabellaCompare


      ),br(),
			fluidRow(style="padding:30px;background-color:#ffffff",
				h3("Andamento dei valori asintotici (massimi raggiunti) ipotizzando un evoluzione di tipo Gompertz"),
          addSpinner(DTOutput("tabAsintoto"), spin = "fading-circle", color = "#009933")
				#	addSpinner(DTOutput("plotAsintoto"), spin = "fading-circle", color = "#009933"),

      ),br()
    )

  })

output$tab_mobile<-renderUI({

  fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
  	h1("Previsioni"),
  	fluidRow(
  		column(12,h4("In questa pagina proponiamo il confronto tra i dati osservati e due modelli di crescita: il modello esponenziale descrive una diffusione in cui il tasso di crescita è costante, questo accade quando l'epidemia si diffonde senza controllo; il modello esponenziale quadratico tiene in conto di una diminuzione del tasso di crescita con l'avanzare del tempo. Questa diminuzione può essere dovuta a misure contenitive o all'esaurimento della popolazione contagiabile."))

  	),
  		 br(),
  		fluidRow(style="padding:30px;background-color:#ffffff",
      fluidRow(


        fluidRow(style="padding-left:30px;",
        column(4,
          prettyRadioButtons('regionLinLogFit',"Tipo Grafico",choices = c("Lineare", "Logaritmico"), selected = "Lineare",status = "success",shape = 'round',inline = T,animation = 'jelly',icon = icon('check'))
          ),
        column(4,
          prettyRadioButtons('modelloFit',"Tipologia Modello",choices = c("Gompertz", "Esp. quadratico", "Esponenziale" ), selected="Gompertz",status = "success",shape = 'round',inline = T,animation = 'jelly',icon = icon('check'))
          ))),

        fluidRow(style="background-color:#ffffff",column(10,offset=1,align="center",

				fluidRow(style="background-color:#ffffff",column(10,offset=1,align="center",h4("Andamenti globali in Italia con previsione a 3 giorni"))),
				 fluidRow(style="padding:10px;background-color:#ffffff",addSpinner(plotlyOutput(outputId="fitIta"), spin = "fading-circle", color = "#009933")),
				 fluidRow( style="padding:20px;background-color:#ffffff;",spiegaFitTotePos),

				h4("Andamento casi positivi per regione con previsione a 3 giorni"))),

        fluidRow(style="padding-left:30px;",
				pickerInput(inputId = "regionSelFit", label = "Seleziona regioni", choices = regioniList,selected=regioni2fit, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto',mobile=T), multiple = TRUE)),


         fluidRow(style="padding:10px;background-color:#ffffff",
				 addSpinner(plotlyOutput(outputId="fitRegion"), spin = "fading-circle", color = "#009933")
			 ), #spiegaFitPos
        ),

         br(),br(),
         fluidRow(style="background-color:#ffffff",

             column(10,offset=1,align="center",h3("Variazione percentuale giorno per giorno")),
             fluidRow(style="padding-left:50px;",
              pickerInput(inputId = "varSel", label = "Seleziona variabile", choices = c("deceduti","totale contagiati"),selected="totale contagiati",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE))),
           fluidRow(style="padding:10px;background-color:#ffffff",addSpinner(plotlyOutput(outputId="percDeltaTot"), spin = "fading-circle", color = "#009933"), spiegaVariazionePercentuale),
         br(),br(),


         fluidRow(style="background-color:#ffffff",

           column(10,offset=1,align="center", h3("Previsione del numero di casi a medio termine con modello esponenziale quadratico")),
           fluidRow(style="padding-left:50px;",
            pickerInput(inputId = "varSel2", label = "Seleziona variabile", choices = c("deceduti","totale contagiati"),selected="totale contagiati",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE))),
           fluidRow(style="padding:10px;background-color:#ffffff",addSpinner(plotlyOutput(outputId="fitCasesIta"), spin = "fading-circle", color = "#009933"), spiegaFitMedioTermine),

  		# br(),br(),
  		# fluidRow(style="padding:30px;background-color:#ffffff", h4("Previsione del numero di casi totali a medio termine con modello esponenziale quadratico")),
      #  fluidRow(style="padding:10px;background-color:#ffffff",plotlyOutput(outputId="fitCasesIta",width="100%")
      #
  		# ),
      br(),
      fluidRow(style="padding:30px;background-color:#ffffff",

          column(width=4,
            selectizeInput("tipoCompare", label="Tipo Comparazione", choices=c("Totale", "Incremento Giornaliero"),  selected = "Lineare")
          ),
          column(width=4,
            uiOutput("dateCompare")
          ),
          addSpinner(DTOutput("tabCompare"), spin = "fading-circle", color = "#009933"),spiegaTabellaCompare


      ),br(),
			fluidRow(style="padding:30px;background-color:#ffffff",
					h3("Andamento dei valori asintotici (massimi raggiunti) ipotizzando un evoluzione di tipo Gompertz"),
          addSpinner(DTOutput("tabAsintoto"), spin = "fading-circle", color = "#009933")
				#	addSpinner(DTOutput("plotAsintoto"), spin = "fading-circle", color = "#009933"),

      ),br()
  	)

  })

output$tab_previsioni<-renderUI({
  out<-NULL
   if((length(reacval$mobile)>0)){
     if(reacval$mobile){
      out<-uiOutput("tab_mobile")
     }
     else{
       out<-uiOutput("tab_desktop")
     }
   }
  })

output$spaces_mobile_prev<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br(),br())
    }

  }
  })

output$spaces_mobile_intro<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br(),br())
    }

  }
  })

output$spaces_mobile_ti<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br(),br())
    }

  }
  })

output$spaces_mobile_reg<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br(),br())
    }

  }
  })

output$spaces_mobile_prov<-renderUI({
  out<-NULL
  if((length(reacval$mobile)>0)){
    if(reacval$mobile){
      out<-fluidRow(br(),br(),br(),br())
    }

  }

  })

  output$spaces_mobile_chisiamo<-renderUI({
    out<-NULL
    if((length(reacval$mobile)>0)){
      if(reacval$mobile){
        out<-fluidRow(br(),br(),br(),br())
      }

    }

    })


    output$spaces_mobile_diario<-renderUI({
      out<-NULL
      if((length(reacval$mobile)>0)){
        if(reacval$mobile){
          out<-fluidRow(br(),br(),br(),br())
        }

      }

      })




  output$sidebar <- renderUI({
    out<-NULL
    if((length(reacval$mobile)>0)){
      if(reacval$mobile){
        out<-list(br(),br(),br(),br())
      }
    }

       })





output$tasti_social<-renderUI({
  url_tweet <- "https://twitter.com/intent/tweet?text=CoVid19&url=https://www.pangeadds.eu/demos/CoVid19/"
  url_link <- "https://www.linkedin.com/shareArticle?mini=true&url=https://www.pangeadds.eu/demos/CoVid19/"
  url_fb<-"https://www.facebook.com/sharer/sharer.php?u=#url=https://www.pangeadds.eu/demos/CoVid19/"
fluidRow(style="padding:30px;",align='center',br(),br(),br(),br(),
#column(10,
  column(3,
  actionButton("twitter_share",label = "",style="align:center;width:40px;color: #fff; background-color: #38A1F3; border-color: #38A1F3",icon = icon("twitter"),
         onclick = sprintf("window.open('%s')", url_tweet))),
  column(3,actionButton("linkedin_share",label = "",style="width:40px;color: #fff; background-color: #0077B5; border-color: #0077B5",icon = icon("linkedin-in"),
               onclick = sprintf("window.open('%s')", url_link))),
  column(3,actionButton("fb_share",label = "",style="width:40px;color: #fff; background-color: #4267b2; border-color: #4267b2",icon = icon("fab fa-facebook-f"),
               onclick = sprintf("window.open('%s')", url_fb))))

  })

stampa_report<-function(x){
  data<-substring(x, nchar(x)-14,nchar(x)-5)
  testo<-paste0("Report del ",data)
  path<-paste0('pastDiary/tabReport_',data,'.html')
  linea<- list(a(testo,href=path,target="_blank",rel="noopener noreferrer"),br())

}

output$storico_report<-renderUI({
    files <- list.files("www/pastDiary")
    files<-sort(files,decreasing=T)
    a<-lapply(files,function(x) stampa_report(x))
    #a<-a('link prova',href="pastDiary/tabReport_2020-03-19.html",target="_blank",rel="noopener noreferrer")

    })

output$video<-renderUI({
  if(reacval$mobile){
    HTML('<iframe width="100%" height="100%" src="https://www.youtube.com/embed/EAaU1aLlcsU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  }else{
    HTML('<iframe  width="560" height="315"  src="https://www.youtube.com/embed/EAaU1aLlcsU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  }

  })

  #



})
