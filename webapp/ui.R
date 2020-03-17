library(shinydashboard)
library(shinyWidgets)
source("dashboardPangea.R")
#HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size))
# dashboardBody(
# 		tags$style(".fa-atlas color: #ff00ff ; }"
# 	)
introTab<-tabItem(tabName="intro",fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#ffe066;background-color:#ffffff;",
										HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size))))

fitTab <- tabItem(tabName = "fitPlots",#style="background-color:#ffc2b3",

							uiOutput("tab_mobile"),
							# fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#009933;",#" border-color :#009933;",
							# 	#box(width=12,
							# 	h1("Analisi previsionale nelle province italiane"),
							# 	fluidRow(
							# 		column(10,h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione di approfondimento")),
							# 		column(2,radioButtons("modelloFit", label="Tipologia Modello", choices=c("Esponenziale", "Exp. quadratico")))
							# 	),
							#
							# 		#h4("Sia gli andamenti totali che quelli delle regioni maggiormente colpite si stanno staccando dall'andamento esponenziale, questo sembra indicare che le misure preventive iniziano ad avere effetto. Infatti, il ritmo di crescita attuale sembra meglio descritto da un andamento esponenziale con rate che diminuisce linearmente nel tempo (modello esponenziale quadratico)."),
							# 		 br(),
							# 		# fluidRow(
							# 		# 	#column(width=4, selectizeInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit, multiple=TRUE, width='400px')),
							# 		# 	column(width=2, selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),
							# 		# 	#column(width=3, selectizeInput("modelloFit", label="Tipologia Modello", choices=c("Esponenziale", "Exp. quadratico"), selected = "Exp. quadratico"))
							# 		# ),
							# 		fluidRow(style="padding:30px;background-color:#ffffff",
							# 			column(2,fluidRow(selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),checkboxGroupInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit)),
							# 		column(10,
							#
							# 			fluidRow(column(6,align="center",h4("Andamento casi positivi per regione con previsione a 3 giorni")),column(6,align="center",h4("Andamenti globali in Italia con previsione a 3 giorni"))),
							# 			fluidRow(
							# 				column(width=6,align="left", plotlyOutput(outputId="fitRegion"), #spiegaFitPos
							# 				),br(),
							# 				column(width=6,align="left",plotlyOutput(outputId="fitIta"), #spiegaFitTot
							# 				),br(),fluidRow(style="padding:20px;",spiegaFitTotePos)
							#
							# 			)
							#
							# 		),
							#
							#
							# 		),br(),br(),
							# 		fluidRow(style="padding:30px;background-color:#ffffff",width=12,  h2("Previsione del numero di casi totali a medio termine con modello esponenziale quadratico"), plotlyOutput(outputId="fitCasesIta")
							#
							# 		),
							# 		br(),
							# 	#)
							# ),
							fluidRow(
                box(width=12,
									column(width=4,
										selectizeInput("tipoCompare", label="Tipo Comparazione", choices=c("Totale", "Incremento Giornaliero"), , selected = "Lineare")
									),
									column(width=4,
										uiOutput("dateCompare")
									),
									DTOutput("tabCompare")

                )
              ),

              fluidRow(
                box(width=12, uiOutput("updatePrevisioniUI"), fontiDati
                )
              )
            )


tiTab <- tabItem(tabName = "tiPlots",
							fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#cc0000;",#style="background-color :#cc0000;",
							h1("Terapia Intensiva"),

									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario sta portando avanti in questi giorni. Non ha scopo allarmistico ma solo di mostrare quali siano le criticità che il nostro paese sta affrontando a causa del CoVid19 "),
									 br(),

										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Previsione del numero di letti occupati da pazienti conCovid19 e disponibilità per regione (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercPrev")
										),br(),
	                	fluidRow(style="padding:20px;background-color:#ffffff",
											h3( "Percentuale in terapia intensiva occupati da pazienti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercNow")
										),br(),
										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Diponibilità di letti in terapia intensiva e numero di occupanti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotNow")
										),


							br()),br(),
              fluidRow(
                box(width=12, uiOutput("updateTIUI"), fontiDati
                )
              )
            )


regTab <- tabItem(tabName = "regPlots",
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#0086b3;",
					h1("Diffusione nelle regioni italiane"),
					fluidRow(style="padding:20px;background-color:#ffffff",#style="background-color :#0086b3;",
						 h3("Mappa dei casi confermati"),
								#plotOutput(outputId="mapRegionGG", height = 800),
								leafletOutput(outputId="mapRegion"),
								spiegaMappa
						),br(),
						fluidRow(style="padding:20px;background-color:#ffffff",
							h3("Andamento dei casi confermati"),
								plotlyOutput(outputId="lineRegion"),
								spiegaLinePlot

						)
						,br(),
						# box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,collapsible = T,
						# 	DTOutput(outputId="tabRegion"),
						# 	spiegaTabella
						# )
						fluidRow(style="padding:20px;background-color:#ffffff",
						h3("Tabella con casi confermati"),DTOutput(outputId="tabRegion"),spiegaTabella

						)
					,br()),br(),fluidRow(
                box(width=12, uiOutput("updateRegUI"), fontiDati
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots",
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#0086b3;",
							h1("Diffusione nelle province italiane"), br(),
							#
							fluidRow(style="padding:20px;background-color:#ffffff",selectInput("regionSel", label="Seleziona regione", choices=regioniList, selected = "Lombardia"),
								h3("Mappa dei casi confermati"),
										#plotOutput(outputId="mapProvinceGG", height = 800),
										leafletOutput(outputId="mapProvince"),
										spiegaMappa
								),br(),
								fluidRow(style="padding:20px;background-color:#ffffff",
								h3("Andamento dei casi confermati"),plotlyOutput(outputId="lineProvince"), spiegaLinePlot
								)

							,br(),
							fluidRow(style="padding:20px;background-color:#ffffff",
								column(width=12, h3("Tabella con casi confermati"),DTOutput(outputId="tabProvince"), spiegaTabella
								)


							),br()),br(),
              fluidRow(
                box(width=12, uiOutput("updatePrvUI"), fontiDati
                )
              )
            )


######################################################
#
#   Main app
#
######################################################

dashboardPage(
	skin = "black",
	dashboardHeader3(title="CoVid-19 in Italia", pagename="CoVid-19 in Italia", logo_img = "logo_pangea_esteso.png", width = 200),

## Sidebar content
	dashboardSidebar(
						sidebarMenu(id='fitCFG',
							menuItem2("Previsioni", tabName = "fitPlots", icon = icon("fas fa-chart-line"))
						),
			      sidebarMenu(id='tiCFG',
			        menuItem2("Terapia Intensiva", tabName = "tiPlots", icon = icon("fas fa-heartbeat"))
			      ),
			      sidebarMenu(id='regCFG',
			        menuItem2('Per Regione', tabName = "regPlots", icon = icon("far fa-chart-bar"))
			      ),#HTML('<font color="#0086b3">Per regione</font>')
			      sidebarMenu(id='prvCFG',
			        menuItem2("Per Provincia", tabName = "prvPlots", icon = icon("far fa-chart-bar"))#("vials"))
			      ),
			sidebarMenu(id='spiegazione',
				menuItem2("Descrizione modelli", tabName = "intro", icon = icon("fas fa-atlas"))
			)#,
		 #selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia")
	),

	dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css")),

    tabItems(
				fitTab,
				tiTab,
        regTab,
        prvTab,
		    introTab
	))
)
