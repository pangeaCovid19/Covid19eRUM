library(shinydashboard)
library(shinyWidgets)
source("dashboardPangea.R")
includeCSS("style.css")
url_tweet <- "https://twitter.com/intent/tweet?text=CoVid19&url=https://www.pangeadds.eu/demos/CoVid19/"
url_link <- "https://www.linkedin.com/shareArticle?mini=true&url=https://www.pangeadds.eu/demos/CoVid19/"
url_fb<-"https://www.facebook.com/sharer/sharer.php?u=#url=https://www.pangeadds.eu/demos/CoVid19/"
chisiamotab<-tabItem(tabName="chisiamo",

				uiOutput("spaces_mobile_chisiamo"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(37,117,173);",
											fluidRow(style="padding:30px;background-color:#ffffff;",
											HTML(readChar("../docs/chisiamo_en.html",file.info("../docs/chisiamo_en.html")$size)),
											fluidRow(style='padding:30',align='center',br(),br(),
											 #uiOutput('video')
											 ))))

diariotab<-tabItem(tabName="diario",

				uiOutput("spaces_mobile_diario"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(243,118,37);",

											h1("Diario"),br(),

											fluidRow(style="padding:30px;background-color:#ffffff;",
											h2("Editoriale"),br(),
											a("Editoriale del 04 Maggio 2020",href="Report04maggio2020.html",target="_blank",rel="noopener noreferrer"),br(),
											a("Editoriale del 22 Marzo 2020",href="Report21marzo2020.html",target="_blank",rel="noopener noreferrer")),br(),
											fluidRow(style="padding:30px;background-color:#ffffff;",
											h2("Report"),br(),
											h4("Clicca sui link per i report aggiornati:"),
											uiOutput('storico_report')
										  )
							)
						)

introTab<-tabItem(tabName="intro",

				uiOutput("spaces_mobile_intro"),

							fluidRow(style="padding:30px;border-style: solid;border-color:#85ede1;",
											fluidRow(style="padding:30px;background-color:#ffffff;",
											HTML(readChar("../docs/intro_en.html",file.info("../docs/intro_en.html")$size)))))

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

#									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario ha sostenuto dall'inizio dell'emergenza."),
									 br(),
									 fluidRow(style="padding:20px;background-color:#ffffff",
										 fluidRow(style="padding:20px;background-color:#ffffff",
											 column( 6, h3("Hospitalized patients")),
											 column(1),
											 column(2,
												 pickerInput(inputId = "varSelTI", label = "Select variable", choices = c("intensive care","Hospitalized patients"),selected="terapia intensiva",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
											 ),
											 column(3,
												 pickerInput(inputId = "regionSelSerieStoricheTI", label = "Select regions", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='uncheck all ',selectAllText='check all'), multiple = TRUE)
											 )
										 ),
										 fluidRow(style='padding:30px;overflow-x:scroll',
										 	addSpinner(plotlyOutput("terapiaIntStoricoTot"), spin = "fading-circle", color = "#cc0000"),
									 		spiegaTerIntNuoviCasi)
									 ),br(),
									 fluidRow(style="padding:10px;background-color:#ffffff",
										 column( 6, h3( "Fraction of intensive care beds occupied by patients with CoVid19 (intensive care beds updated to 2018)")),
										 column(1),
										 column(5, uiOutput("UIgiornoTI"))
									 ),fluidRow(style="padding:10px;background-color:#ffffff",
											addSpinner(plotlyOutput("terapiaIntPlotPercNow"), spin = "fading-circle", color = "#cc0000"),
											spiegaTerIntPercentuale
										),br(),
										fluidRow(style="padding:10px;background-color:#ffffff",
											column( 6, h3("Intensive care beds occupied by patients with CoVid19 (intensive care beds updated to 2018)")),
 										 	column(1),
 										 	column(5, uiOutput("UIgiornoTI2"))
										),
										fluidRow(style="padding:10px;background-color:#ffffff",
											addSpinner(plotlyOutput("terapiaIntPlotNow"), spin = "fading-circle", color = "#cc0000"),
											spiegaTerIntAttuale
										),br(),

							br()),br(),
              fluidRow(
                box(width=12, uiOutput("updateTIUI"), fontiDati
                )
              )
            )


regTab <- tabItem(tabName = "regPlots",
					uiOutput("spaces_mobile_reg"),
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#add437;",
					h1("Focus on italian Regions"),br(),
					##################################################
					#  NUOVI CASI
					fluidRow(style="padding:20px;background-color:#ffffff",
						fluidRow(style="padding:20px;background-color:#ffffff",
							column(4, h3("Daily cases")),
							column(1),
							column(2,
								pickerInput(inputId = "tipoPlotSerieStoricheReg", label = "Graph type", choices = c("Regional","Overall"),selected="Regional",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
							),
							column(2,
								pickerInput(inputId = "varSelSerieStoricheReg", label = "Select Variable", choices = c("new cases","deaths"),selected="new cases",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
							),
							column(2,
								pickerInput(inputId = "regionSelSerieStorichexReg", label = "Select region", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='uncheck all',selectAllText='check all'), multiple = TRUE)
							)
						),
						fluidRow(style='padding:10px;overflow-x:scroll;',addSpinner(plotlyOutput('nuoviPositiviStoricoReg'), spin = "fading-circle", color = "#add437")),
						spiegaGraficoCasiGiornalieriRegioni
					),br(),
					##################################################
					fluidRow(style="padding:20px;background-color:#ffffff",
						h3("Confirmed cases, monitornig table"), DTOutput(outputId="tabRegioniMonitor"), spiegaTabellaMonitor
					),br(),
						##################################################
						# Tasso crescila logaritmico Regionale
						fluidRow(style="padding:20px;background-color:#ffffff",
							fluidRow(style="padding:20px;background-color:#ffffff",
								column(4, h3("Daily growth rate")),
								column(1),
								column(2,
									pickerInput(inputId = "tipoPlotSerieStoricheRegPer", label = "Graph type", choices = c("Regional","Overall"),selected="Overall",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
								),
								column(3,
									pickerInput(inputId = "regionSelSerieStorichexRegPer", label = "Select regions", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='uncheck all',selectAllText='check all'), multiple = TRUE)
								)
							),
							fluidRow(style='padding:10px;overflow-x:scroll',addSpinner(plotlyOutput('nuoviPositiviStoricoRegPercentuale'), spin = "fading-circle", color = "#add437")),
							spiegaGraficoNuoviPositiviStoricoRegPercentuale
						),

						##################################################
						br(),
						##################################################
						# CASI VS NUOVI CASI
						fluidRow(style="padding:20px;background-color:#ffffff",
							h3("New cases versus total cases"),
								uiOutput('inputRegioniCasiVsNuovicasi'),
								fluidRow(style="overflow-x:scroll;padding:20px;",
								addSpinner(plotlyOutput('lineRegioniCasiVsNuovicasi'), spin = "fading-circle", color = "#add437")),
								spiegaGraficoCasiVsCasiNuovi
						)



					,br(),
						fluidRow(style="padding:10px;background-color:#ffffff",
							h3("Timeseries comparison"),
							fluidRow(style="padding:10px;",
								column(width=3,
									pickerInput(inputId = "regionSelSerieStoriche", label = "Select segions", choices = regioniList,selected=regioni2fit, options = pickerOptions(size=10,actionsBox = F,maxOptionsText=HTML('<font color=#ad0000>You can compare 6 regions!</font>') ,selectedTextFormat = "count >20",maxOptions = 6), multiple = TRUE),
									selectizeInput("variabileCompare", label="Comparing variable", 	, choices=c("Infected", "Deaths"), selected = "Lazio", multiple=FALSE)
								),
								column(width=9,
									uiOutput('selLagRegioni')
								)
							),
							fluidRow(style='padding:30px;overflow-x:scroll;',
							addSpinner(plotlyOutput(outputId="lineRegioniConfronto"), spin = "fading-circle", color = "#add437")),
							spiegaConfrontoSerieRegioni
						),br(),
						fluidRow(style="padding:20px;background-color:#ffffff",
						#style="background-color :#add437;",
							 uiOutput("selRegioni"),
							 h3("Diffusion map"),
									addSpinner(leafletOutput(outputId="mapRegioni"), spin = "fading-circle", color = "#add437"),
									spiegaMappa
							),br(),
#						fluidRow(style="padding:30px;background-color:#ffffff",
#							h3("Andamento dei casi confermati"),
#							selectizeInput("variabileLineRegioni", label="Variabile da mostrare", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE),
#							fluidRow(style="overflow-x:scroll;padding:20px;",addSpinner(plotlyOutput(outputId="lineRegioni"), spin = "fading-circle", color = "#add437")),
#							spiegaLinePlot),br(),br(),
							fluidRow(style="padding:20px;background-color:#ffffff",
								h3("Resume table"),DTOutput(outputId="tabRegioni"), spiegaTabella
							),

							br()),br(),fluidRow(
                box(width=12, uiOutput("updateRegUI"), fontiDati
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots",
					uiOutput("spaces_mobile_prov"),
					fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#add437;",
							h1("Focus on Italian Districts"), br(),
							#
							##################################################
							#  NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(3, h3("Daily cases")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStorichePrev", label = "Graph type", choices = c("Districts","Overall"),selected="Districts",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(2,
										pickerInput(inputId = "regionSelSerieStorichexProv", label = "Select segions", choices = regioniList,selected="Lombardia", options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='uncheck all',selectAllText='check all'), multiple = TRUE)
									),
									column(3,
										uiOutput("uiProvSelSerieStoricheProv")
									)
								),
								fluidRow(style='padding:30px;overflow-x:scroll',addSpinner(plotlyOutput('nuoviPositiviStoricoProv'), spin = "fading-circle", color = "#add437")),
								spiegaGraficoCasiGiornalieriProvincia
							),
							##################################################
							br(),
							fluidRow(style="padding:20px;background-color:#ffffff",
								column(width=12, h3("Confirmed cases, monitornig table"),DTOutput(outputId="tabProvinceMonitor"), spiegaTabellaMonitor
								)
							),br(),
							# Tasso crescila logaritmico provinciale
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(3, h3("Daily growth rate")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStorichePrvPer", label = "Graph type", choices = c("Districts","Overall"),selected="Overall",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(3,
										pickerInput(inputId = "regionSelSerieStorichexPrvPer", label = "Select region", choices = regioniList, selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='uncheck all',selectAllText='check all'), multiple = TRUE)
									),
									column(3,
										uiOutput('inpProvincePositiviStoricoPrvPercentuale')
									)
								),
								fluidRow(style='padding:30px;overflow-x:scroll',addSpinner(plotlyOutput('nuoviPositiviStoricoPrvPercentuale'), spin = "fading-circle", color = "#add437")),
								spiegaGraficoNuoviPositiviStoricoPrvPercentuale
							),
							##################################################
							br(),
							# CASI VS NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								h3("New cases versus total cases"),
									uiOutput('inputProvinceCasiVsNuovicasi'),
										fluidRow(style="overflow-x:scroll;",align='center',addSpinner(plotlyOutput('lineProvinceCasiVsNuovicasi'), spin = "fading-circle", color = "#add437")),
									spiegaGraficoCasiVsCasiNuovi
							),
							##################################################
							br(),
							# confronto serie storiche
							fluidRow(style="padding:30px;background-color:#ffffff",
								h3("Timeseries comparison"),
								fluidRow(style="padding:20px;",
									column(width=3,
										pickerInput(inputId = "provSelSerieStoriche", label = "Select districts", choices = provinceList,selected=province2fit, options = pickerOptions(size=10,actionsBox = F,maxOptionsText=HTML('<font color=#ad0000>You can compare 6 districts!</font>') ,selectedTextFormat = "count >20",maxOptions = 6), multiple = TRUE)
									),
									column(width=9,
										uiOutput('selLagProvince')
									)
								),
								fluidRow(style='padding:30px;overflow-x:scroll;',
									addSpinner(plotlyOutput(outputId="lineProvinceConfronto"), spin = "fading-circle", color = "#add437")
								),
								spiegaConfrontoSerieProvince
							),br(),
							fluidRow(style="padding:20px;background-color:#ffffff",
							fluidRow(style='padding:30px;',
								uiOutput("selProvince")),
								h3("Diffusion map"),
										addSpinner(leafletOutput(outputId="mapProvince"), spin = "fading-circle", color = "#add437"),
										spiegaMappa
								),br(),
#								fluidRow(style="padding:40px;background-color:#ffffff",
#									h3("Andamento dei casi confermati"),
#									fluidRow(style="overflow-x:scroll;",align='center',addSpinner(plotlyOutput(outputId="lineProvince"), spin = "fading-circle", color = "#add437")), spiegaLinePlot
#							),br(),
							fluidRow(style="padding:20px;background-color:#ffffff",
								column(width=12, h3("Resume table"),DTOutput(outputId="tabProvince"), spiegaTabella
								)
							),
							br()),br(),
              fluidRow(
                box(width=12, uiOutput("updatePrvUI"), fontiDati
                )
              )
            )


worldtab <- tabItem(tabName = "world",
							fluidRow(style="padding-left:30px;padding-right:30px;border-style: solid;border-color:#33ccff;",#style="background-color :#cc0000;",
							h1("Nel mondo"),
									h4("Spiegazione blablabla"),
									 br(),
									 fluidRow(style="padding:20px;background-color:#ffffff",
										 fluidRow(style="padding:20px;background-color:#ffffff",
											 column( 6, h3("Spazio per plot")),
											 column(1),
										 )
										# fluidRow(style='padding:30px;overflow-x:scroll',addSpinner(plotlyOutput("terapiaIntStoricoTot"), spin = "fading-circle", color = "#cc0000"))
									 ),br(),
	                	fluidRow(style="padding:10px;background-color:#ffffff",
											h3( "Titolo plot")
											#addSpinner(plotlyOutput("terapiaIntPlotPercNow"), spin = "fading-circle", color = "#cc0000"),
											#spiegaTerIntPercentuale
										),br(),
										fluidRow(style="padding:10px;background-color:#ffffff",
											h3("Titolo plot")

										),br(),

							br()),br(),
              fluidRow(
                box(width=12,uiOutput("updateworld"),  fontiDati
                )
              )
            )

######################################################
#
#   Main app
#
######################################################

dashboardPage(title="CoVid-19 in Italy",
	skin = "black",
	dashboardHeader3( pagename="CoVid-19 in Italy", logo_img = "logo_pangea_esteso.png", width = 200,

		tags$li(uiOutput('data_agg') ,class='dropdown', style="color:#0b4975;font-weight: bold;margin-top:40px;margin-right: 40px;margin-left: 60px;")


												 ),


## bbar content
	dashboardSidebar(uiOutput('sidebar'),
						sidebarMenu(id='fitCFG',
							menuItem2("Forecast", tabName = "fitPlots", icon = icon("fas fa-chart-line"))
						),
			      sidebarMenu(id='tiCFG',
			        menuItem2("Intensive Care", tabName = "tiPlots", icon = icon("fas fa-heartbeat"))
			      ),
			      sidebarMenu(id='regCFG',
			        menuItem2('Regions', tabName = "regPlots", icon = icon("far fa-chart-bar"))
			      ),#HTML('<font color="#0086b3">Per regione</font>')
			      sidebarMenu(id='prvCFG',
			        menuItem2("Districts", tabName = "prvPlots", icon = icon("far fa-chart-bar"))#("vials"))
			      ),
			sidebarMenu(id='spiegazione',
				menuItem2("Spred math", tabName = "intro", icon = icon("fas fa-square-root-alt"))
			),
#			sidebarMenu(id='diariodibordo',
#				menuItem2("Diario ", tabName = "diario", icon = icon("fas fa-book-open"))
#			),
			sidebarMenu(id='presentazione',
				menuItem2("Pangea", tabName = "chisiamo", icon = icon("fas fa-users"))
			),
			uiOutput('tasti_social')
	),

	dashboardBody(
		HTML('<script>
				  $( document ).on("shiny:sessioninitialized", function(event) {
					 var jsAgt = navigator.userAgent;
					 Shiny.onInputChange("GetNavUserAgent",jsAgt);
				 });</script>'),
	    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css"),
			tags$meta(property="og:title", content="CoVid-19 in Italy"),
			tags$meta(property="og:image",content="plot.png"),
			tags$meta(name="twitter:card",content="summary_large_image"),
			tags$link(rel = "icon",href ="logo_piccolo.png",type = "image/x-icon")

),


    tabItems(
				fitTab,
				tiTab,
        regTab,
        prvTab,
			#	worldtab,
		    introTab
	#	    ,diariotab
		    ,chisiamotab
	))

)
