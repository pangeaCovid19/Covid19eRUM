library(shinydashboard)
library(shinyWidgets)
source("dashboardPangea.R")
includeCSS("style.css")
#HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size))
# dashboardBody(
# 		tags$style(".fa-users color: #ff00ff ; }"
# 	)
chisiamotab<-tabItem(tabName="chisiamo",

				uiOutput("spaces_mobile_chisiamo"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(37,117,173);",
										#	h1("Quanto veloce si diffonde il Coronavirus in Italia "),

											fluidRow(style="padding:30px;background-color:#ffffff;",
											HTML(readChar("../docs/chisiamo.html",file.info("../docs/chisiamo.html")$size)))))

diariotab<-tabItem(tabName="diario",

				uiOutput("spaces_mobile_diario"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(243,118,37);",


											fluidRow(style="padding:30px;background-color:#ffffff;",
											h1("Diario della diffusione"),br(),
											h4("Clicca sui link per i report aggiornati:"),
											a("Report del 19 Marzo 2020",href="tabReport.html",target="_blank",rel="noopener noreferrer")	)))


introTab<-tabItem(tabName="intro",

				uiOutput("spaces_mobile_intro"),

							fluidRow(style="padding:30px;border-style: solid;border-color:#85ede1;",
										#	h1("Quanto veloce si diffonde il Coronavirus in Italia "),

											fluidRow(style="padding:30px;background-color:#ffffff;",
											HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size)))))

fitTab <- tabItem(tabName = "fitPlots",#style="background-color:#ffc2b3",
							uiOutput("spaces_mobile_prev"),

							uiOutput("tab_previsioni"),



              fluidRow(
                box(width=12, uiOutput("updatePrevisioniUI"), fontiDati
                )
              )
            )


tiTab <- tabItem(tabName = "tiPlots",
					uiOutput("spaces_mobile_ti"),
							fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#cc0000;",#style="background-color :#cc0000;",
							h1("Terapia Intensiva"),

									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario sta portando avanti in questi giorni. Non ha scopo allarmistico ma solo di mostrare quali siano le criticità che il nostro paese sta affrontando a causa del CoVid19 "),
									 br(),

										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Previsione del numero di letti occupati da pazienti con Covid19 e disponibilità per regione (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercPrev"),
											spiegaTerIntPrevisione
										),br(),
	                	fluidRow(style="padding:20px;background-color:#ffffff",
											h3( "Percentuale in terapia intensiva occupati da pazienti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercNow"),
											spiegaTerIntPercentuale
										),br(),
										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Diponibilità di letti in terapia intensiva e numero di occupanti con CoVid19 (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotNow"),
											spiegaTerIntAttuale
										),


							br()),br(),
              fluidRow(
                box(width=12, uiOutput("updateTIUI"), fontiDati
                )
              )
            )


regTab <- tabItem(tabName = "regPlots",
					uiOutput("spaces_mobile_reg"),
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#add437;",
					h1("Diffusione nelle regioni italiane"),br(),
					fluidRow(style="padding:20px;background-color:#ffffff",
					#style="background-color :#add437;",
						 uiOutput("selRegioni"),
						 h3("Mappa dei casi confermati"),
								#plotOutput(outputId="mapRegioniGG", height = 800),
								leafletOutput(outputId="mapRegioni"),
								spiegaMappa
						),br(),
						fluidRow(style="padding:20px;background-color:#ffffff",
							h3("Andamento dei casi confermati"),
							selectizeInput("variabileLineRegioni", label="Variabile da mostrare", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE),
							plotlyOutput(outputId="lineRegioni"),
							spiegaLinePlot,
							#plotlyOutput(outputId="puntiRegioni"),
							##################################################
							##################################################
							fluidRow(tyle="padding:20px;background-color:#ffffff",
								h3("Confronto tra variabili"),
								column(width=3,
									selectizeInput("confrontox", label="Variabile su asse X", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE)
								),
								column(width=3,
									selectizeInput("confrontoy", label="Variabile su asse Y", 	choices=campiTotali , selected = "tamponi", multiple=FALSE)
								),
								column(width=3,
									selectizeInput("confrontoTipoGratico", label="Assi del grafico", 	choices=c('Lineari', 'Logaritmici') , selected = "Lineari", multiple=FALSE)
								),
								column(width=3,
									uiOutput("confrontoGiornoUI")
								)
							),
							plotlyOutput(outputId="puntiRegioni"),
							##################################################
							##################################################
						)
						,br(),
						# box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,collapsible = T,
						# 	DTOutput(outputId="tabRegioni"),
						# 	spiegaTabella
						# )
						fluidRow(style="padding:20px;background-color:#ffffff",
						h3("Tabella con casi confermati"),DTOutput(outputId="tabRegioni"),spiegaTabella

						)
					,br()),br(),fluidRow(
                box(width=12, uiOutput("updateRegUI"), fontiDati
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots",
					uiOutput("spaces_mobile_prov"),
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#add437;",
							h1("Diffusione nelle province e nelle città metropolitane italiane"), br(),
							#
							fluidRow(style="padding:20px;background-color:#ffffff",
								uiOutput("selProvince"),
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

dashboardPage(title="CoVid-19 in Italia",
	skin = "black",
	dashboardHeader3( pagename="CoVid-19 in Italia", logo_img = "logo_pangea_esteso.png", width = 200),

## bbar content
	dashboardSidebar(uiOutput('sidebar'),
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
				menuItem2("Matematica della diffusione", tabName = "intro", icon = icon("fas fa-square-root-alt"))
			),
			sidebarMenu(id='diariodibordo',
				menuItem2("Diario della diffusione", tabName = "diario", icon = icon("fas fa-book-open"))
			),
			sidebarMenu(id='presentazione',
				menuItem2("Chi Siamo", tabName = "chisiamo", icon = icon("fas fa-users"))
			)
		 #selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia")
	),

	dashboardBody(
		HTML('<script>
				  $( document ).on("shiny:sessioninitialized", function(event) {
					 var jsAgt = navigator.userAgent;
					 Shiny.onInputChange("GetNavUserAgent",jsAgt);
				 });</script>'),
	    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css")),


    tabItems(
				fitTab,
				tiTab,
        regTab,
        prvTab,
		    introTab
		    ,diariotab
		    ,chisiamotab
	))

)
