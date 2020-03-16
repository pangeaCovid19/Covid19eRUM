library(shinydashboard)
source("dashboardPangea.R")
#HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size))
introTab<-tabItem(tabName="intro",fluidRow(box(width=12,HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size)))))

fitTab <- tabItem(tabName = "fitPlots",
#							h1("Analisi previsionale nelle province italiane"), br(), br(),
							fluidRow(
								box(width=12,
									h4("In questa pagina proponiamo il confronto tra i dati registrati, sia regionali che nazionali e due modelli di crescita. Il primo modello (esponenziale) descrive una diffusione incontrollata, mentre il secondo (esponenziale quadratico) tenta di tenere conto dell'effetto di misure contenitive. Per maggiori dettagli, controlla la sezione di approfondimento"),
									#h4("Sia gli andamenti totali che quelli delle regioni maggiormente colpite si stanno staccando dall'andamento esponenziale, questo sembra indicare che le misure preventive iniziano ad avere effetto. Infatti, il ritmo di crescita attuale sembra meglio descritto da un andamento esponenziale con rate che diminuisce linearmente nel tempo (modello esponenziale quadratico)."),
									 br(),
									fluidRow(
										column(width=4, selectizeInput("regionSelFit", label="Seleziona regioni", choices=regioniList, selected = regioni2fit, multiple=TRUE, width='400px')),
										column(width=2, selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare")),
										column(width=3, selectizeInput("modelloFit", label="Tipologia Modello", choices=c("Esponenziale", "Exp. quadratico"), selected = "Exp. quadratico"))
									),
									fluidRow(
										column(width=6, title = tagList(shiny::icon("globe-europe"), "Andamento casi positivi per regione con previsione a 3 giorni"), status = "primary", solidHeader = F,
												collapsible = T, plotlyOutput(outputId="fitRegion"), spiegaFitPos
										),
										column(width=6, title =  "Andamenti globali in Italia con previsione a 3 giorni", status = "primary", solidHeader = F,
												collapsible = T,  plotlyOutput(outputId="fitIta"), spiegaFitTot
										)
									),
									fluidRow(
										box(width=12, title = tagList(shiny::icon("globe-europe"), "Previsione del numero di casi totali a medio termine con modello esponenziale qudratico"), status = "primary", solidHeader = F,
												collapsible = T, plotlyOutput(outputId="fitCasesIta")
										)
									),
								)
							),
              fluidRow(
                box(width=12, uiOutput("updatePrevisioniUI"), fontiDati
                )
              )
            )


tiTab <- tabItem(tabName = "tiPlots",
							fluidRow(
								box(width=12,
									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario sta portando avanti in questi giorni. Non ha scopo allarmistico ma solo di mostrare quali siano le criticità che il nostro paese sta affrontando a causa del CoVid19 "),
									 br(),
	                fluidRow(
										box(width=12,
											title = tagList(shiny::icon("globe-europe"), "Previsione del numero di letti occupati da pazienti conCovid19 e disponibilità per regione (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercPrev")
										),
	                	box(width=12,
											title = tagList(shiny::icon("globe-europe"), "Percentuale in terapia intensiva occupati da pazienti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercNow")
										),
										box(width=12,
											title = tagList(shiny::icon("globe-europe"), "Diponibilità di letti in terapia intensiva e numero di occupanti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotNow")
										)
	                )
								)
							),
              fluidRow(
                box(width=12, uiOutput("updateTIUI"), fontiDati
                )
              )
            )


regTab <- tabItem(tabName = "regPlots",
#							h1("Diffusione nelle regioni italiane"), br(), br(),
              fluidRow(
								box(width=6, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati"), status = "primary", solidHeader = F,
										collapsible = T,
										#plotOutput(outputId="mapRegionGG", height = 800),
										leafletOutput(outputId="mapRegion"),
										spiegaMappa
								),
								box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,collapsible = T,
									DTOutput(outputId="tabRegion"),
									spiegaTabella
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,collapsible = T,
									plotlyOutput(outputId="lineRegion"),
									spiegaLinePlot
								)
              ),fluidRow(
                box(width=12, uiOutput("updateRegUI"), fontiDati
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots",
#							h1("Diffusione nelle province italiane"), br(), br()
#
              fluidRow(
                box(width=6, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T,
										selectInput("regionSel", label="Seleziona regione", choices=regioniList, selected = "Lombardia"),
										#plotOutput(outputId="mapProvinceGG", height = 800),
                    leafletOutput(outputId="mapProvince"),
										spiegaMappa
                ),
								box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T,
											DTOutput(outputId="tabProvince"), spiegaTabella
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T,
										plotlyOutput(outputId="lineProvince"), spiegaLinePlot
                )
              ),
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
				menuItem2("Previsioni", tabName = "fitPlots", icon = icon("chart-line"))
			),
      sidebarMenu(id='tiCFG',
        menuItem2("Terapia Intensiva", tabName = "tiPlots", icon = icon("stethoscope"))
      ),
      sidebarMenu(id='regCFG',
        menuItem2("Regioni", tabName = "regPlots", icon = icon("atlas"))
      ),
      sidebarMenu(id='prvCFG',
        menuItem2("Province", tabName = "prvPlots", icon = icon("atlas"))
      ),
			sidebarMenu(id='spiegazione',
				menuItem2("Descrizione modelli", tabName = "intro", icon = icon("bandcamp"))
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
