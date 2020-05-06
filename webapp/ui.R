library(shinydashboard)
library(shinyWidgets)
source("dashboardPangea.R")
includeCSS("style.css")
url_tweet <- "https://twitter.com/intent/tweet?text=CoVid19&url=https://www.pangeadds.eu/demos/CoVid19/"
url_link <- "https://www.linkedin.com/shareArticle?mini=true&url=https://www.pangeadds.eu/demos/CoVid19/"
url_fb<-"https://www.facebook.com/sharer/sharer.php?u=#url=https://www.pangeadds.eu/demos/CoVid19/"
#HTML(readChar("../docs/intro.html",file.info("../docs/intro.html")$size))
# dashboardBody(
# 		tags$style(".fa-users color: #ff00ff ; }"
# 	)
chisiamotab<-tabItem(tabName="chisiamo",

				uiOutput("spaces_mobile_chisiamo"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(37,117,173);",
										#	h1("Quanto veloce si diffonde il Coronavirus in Italia "),

											fluidRow(style="padding:30px;background-color:#ffffff;",
											HTML(readChar("../docs/chisiamo.html",file.info("../docs/chisiamo.html")$size)),
											fluidRow(style='padding:30',align='center',br(),br(),
											 uiOutput('video')
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
											#a(uiOutput("data_report"),href="tabReport.html",target="_blank",rel="noopener noreferrer"),
											uiOutput('storico_report')
										  )
											# ,br(),
											# fluidRow(style="padding:20px;background-color:#ffffff",
											# 	column(width=12, h3("Tabella letalità per fascia di età"),DTOutput(outputId="letality"), spiegaTabella
											# )
							)
						)


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

									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario ha sostenuto dall'inizio dell'emergenza."),
									 br(),
									 fluidRow(style="padding:20px;background-color:#ffffff",
										 fluidRow(style="padding:20px;background-color:#ffffff",
											 column( 6, h3("Numero dei pazienti ospedalizzati per regione")),
											 column(1),
											 column(2,
												 pickerInput(inputId = "varSelTI", label = "Seleziona variabile", choices = c("terapia intensiva","pazienti ospedalizzati"),selected="terapia intensiva",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
											 ),
											 column(3,
												 pickerInput(inputId = "regionSelSerieStoricheTI", label = "Seleziona regioni", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
											 )
										 ),
										 addSpinner(plotlyOutput("terapiaIntStoricoTot"), spin = "fading-circle", color = "#cc0000")
									 ),br(),

	                	fluidRow(style="padding:20px;background-color:#ffffff",
											h3( "Percentuale in terapia intensiva occupati da pazienti con CoVid19 (posti letto aggiornati al 2018)"),
											addSpinner(plotlyOutput("terapiaIntPlotPercNow"), spin = "fading-circle", color = "#cc0000"),
											spiegaTerIntPercentuale
										),br(),
										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Diponibilità di letti in terapia intensiva e numero di occupanti con CoVid19 (posti letto aggiornati al 2018)"),
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
					h1("Diffusione nelle regioni italiane"),br(),
					fluidRow(style="padding:20px;background-color:#ffffff",
					#style="background-color :#add437;",
						 uiOutput("selRegioni"),
						 h3("Mappa dei casi confermati"),
								#plotOutput(outputId="mapRegioniGG", height = 800),
								addSpinner(leafletOutput(outputId="mapRegioni"), spin = "fading-circle", color = "#add437"),
								spiegaMappa
						),br(),
						fluidRow(style="padding:30px;background-color:#ffffff",
							h3("Andamento dei casi confermati"),
							selectizeInput("variabileLineRegioni", label="Variabile da mostrare", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE),
							fluidRow(style="overflow-x:scroll;padding:20px;",addSpinner(plotlyOutput(outputId="lineRegioni"), spin = "fading-circle", color = "#add437")),
							#uiOutput('legenda_regioni'),
							spiegaLinePlot,
							#plotlyOutput(outputId="puntiRegioni"),
							##################################################
							##################################################
							#FIXME per togliere i grafici nuovi commentare fino a plotlyOutput(outputId="puntiRegioni")
#							fluidRow(style="padding:20px;background-color:#ffffff",
#								h3("Confronto tra variabili"),
#								fluidRow(style="padding:10px;",
#								column(width=3,
#									selectizeInput("confrontox", label="Variabile su asse X", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE)
#									,selectizeInput("confrontoy", label="Variabile su asse Y", 	choices=campiTotali , selected = "tamponi", multiple=FALSE)
#								),
#								column(width=2,offset=1,fluidRow(h1(' ')),
#									prettyRadioButtons('confrontoTipoGratico',"Assi del grafico",choices = c('Lineari', 'Logaritmici') , selected = "Logaritmici", status = "primary",shape = 'round',outline = FALSE,animation = 'jelly',icon = icon('check'))
#								),
#								column(width=6,fluidRow(h1(' ')),
#									uiOutput("confrontoGiornoUI")
#								))
#							),
#							fluidRow(style='padding:30px;',fluidRow(style='padding:20px;overflow-x:scroll;',addSpinner(plotlyOutput(outputId="puntiRegioni"), spin = "fading-circle", color = "#add437"))),
							# confronto serie storiche
							fluidRow(style="padding:20px;background-color:#ffffff",
								h3("Confronto tra serie storiche"),
								fluidRow(style="padding:10px;",
									column(width=3,
#										selectizeInput("serieStoricheRegion1", label="Regione 1", 	, choices=regioniList, selected = "Lombardia", multiple=FALSE),
#										selectizeInput("serieStoricheRegion2", label="Regione 2", 	, choices=regioniList, selected = "Lazio", multiple=FALSE),
										pickerInput(inputId = "regionSelSerieStoriche", label = "Seleziona regioni", choices = regioniList,selected=regioni2fit, options = pickerOptions(size=10,actionsBox = F,maxOptionsText=HTML('<font color=#ad0000>È possibile confrontare al più 6 regioni!</font>') ,selectedTextFormat = "count >20",maxOptions = 6), multiple = TRUE),
										selectizeInput("variabileCompare", label="Variabile da confrontare", 	, choices=c("totale_casi", "deceduti"), selected = "Lazio", multiple=FALSE)
									),
									column(width=9,
#										uiOutput('selLagRegione1'),
#										uiOutput('selLagRegione2'),
										uiOutput('selLagRegioni')
									)
								),
								fluidRow(style='padding:30px;overflow-x:scroll;',
								addSpinner(plotlyOutput(outputId="lineRegioniConfronto"), spin = "fading-circle", color = "#add437")),
								spiegaConfrontoSerieRegioni
							),
						####### fine confronto serie storiche
							#uiOutput('legenda_regioni_bullet'),
							##################################################
							##################################################
							# CASI VS NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								h3("Nuovi casi in funzione del numero totale di casi"),
									uiOutput('inputRegioniCasiVsNuovicasi'),
									fluidRow(style="overflow-x:scroll;padding:20px;",addSpinner(plotlyOutput('lineRegioniCasiVsNuovicasi'), spin = "fading-circle", color = "#add437")),
									spiegaGraficoCasiVsCasiNuovi
							),
							##################################################
							# Tasso crescila logaritmico Regionale
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(4, h3("Tasso di crescita giornaliero")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStoricheRegPer", label = "Tipo Grafico", choices = c("regionale","globale"),selected="globale",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(3,
										pickerInput(inputId = "regionSelSerieStorichexRegPer", label = "Seleziona regioni", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
									)
								),
								addSpinner(plotlyOutput('nuoviPositiviStoricoRegPercentuale'), spin = "fading-circle", color = "#add437"),
								spiegaGraficoNuoviPositiviStoricoRegPercentuale
							),
							##################################################
							##################################################
							#  NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(4, h3("Numero casi giornalieri")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStoricheReg", label = "Tipo Grafico", choices = c("regionale","totale"),selected="regionale",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(2,
										pickerInput(inputId = "varSelSerieStoricheReg", label = "Seleziona variabile", choices = c("nuovi casi","decessi"),selected="terapia intensiva",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(3,
										pickerInput(inputId = "regionSelSerieStorichexReg", label = "Seleziona regioni", choices = regioniList,selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
									)
								),
								addSpinner(plotlyOutput('nuoviPositiviStoricoReg'), spin = "fading-circle", color = "#add437"),
								spiegaGraficoCasiGiornalieriRegioni
							),
							##################################################
						)
						,br(),
						# box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,collapsible = T,
						# 	DTOutput(outputId="tabRegioni"),
						# 	spiegaTabella
						# )
						fluidRow(style="padding:20px;background-color:#ffffff",
						h3("Tabella con casi confermati"),DTOutput(outputId="tabRegioni"), spiegaTabella

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
							fluidRow(style='padding:30px;',
								uiOutput("selProvince")),
								h3("Mappa dei casi confermati"),
										#plotOutput(outputId="mapProvinceGG", height = 800),
										addSpinner(leafletOutput(outputId="mapProvince"), spin = "fading-circle", color = "#add437"),
										spiegaMappa
								),br(),
								fluidRow(style="padding:40px;background-color:#ffffff",
								h3("Andamento dei casi confermati"),

								fluidRow(style="overflow-x:scroll;",align='center',addSpinner(plotlyOutput(outputId="lineProvince"), spin = "fading-circle", color = "#add437")), spiegaLinePlot
							),
							# confronto serie storiche
							fluidRow(style="padding:20px;background-color:#ffffff",
								h3("Confronto tra serie storiche"),
								fluidRow(style="padding:10px;",
									column(width=3,
										pickerInput(inputId = "provSelSerieStoriche", label = "Seleziona province", choices = provinceList,selected=province2fit, options = pickerOptions(size=10,actionsBox = F,maxOptionsText=HTML('<font color=#ad0000>È possibile confrontare al più 6 province!</font>') ,selectedTextFormat = "count >20",maxOptions = 6), multiple = TRUE)#,
										#selectizeInput("variabileCompareProvince", label="Variabile da confrontare", 	, choices=c("totale_casi", "deceduti"), selected = "Lazio", multiple=FALSE)
									),
									column(width=9,
										uiOutput('selLagProvince')
									)
								),
								fluidRow(style='padding:30px;overflow-x:scroll;',
									addSpinner(plotlyOutput(outputId="lineProvinceConfronto"), spin = "fading-circle", color = "#add437")
								),
								spiegaConfrontoSerieProvince
							),
							##################################################
							# CASI VS NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								h3("Nuovi casi in funzione del numero totale di casi"),
									uiOutput('inputProvinceCasiVsNuovicasi'),
										fluidRow(style="overflow-x:scroll;",align='center',addSpinner(plotlyOutput('lineProvinceCasiVsNuovicasi'), spin = "fading-circle", color = "#add437")),
									spiegaGraficoCasiVsCasiNuovi
							),
							##################################################
							# Tasso crescila logaritmico provinciale
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(3, h3("Tasso di crescita giornaliero")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStorichePrvPer", label = "Tipo Grafico", choices = c("regionale","globale"),selected="globale",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(3,
										uiOutput('inpProvincePositiviStoricoPrvPercentuale')
									),
									column(3,
										pickerInput(inputId = "regionSelSerieStorichexPrvPer", label = "Seleziona regione", choices = regioniList, selected=regioniList, options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
									)
								),
								addSpinner(plotlyOutput('nuoviPositiviStoricoPrvPercentuale'), spin = "fading-circle", color = "#add437"),
								spiegaGraficoNuoviPositiviStoricoPrvPercentuale
							),
							##################################################
							##################################################
							#  NUOVI CASI
							fluidRow(style="padding:30px;background-color:#ffffff",
								fluidRow(style="padding:20px;background-color:#ffffff",
									column(4, h3("Numero casi giornalieri")),
									column(1),
									column(2,
										pickerInput(inputId = "tipoPlotSerieStorichePrev", label = "Tipo Grafico", choices = c("provinciale","totale"),selected="provinciale",options = list(size=10,`actions-box` = TRUE, `selected-text-format` = "count >20"), multiple = FALSE)
									),
									column(3,
										pickerInput(inputId = "regionSelSerieStorichexProv", label = "Seleziona regioni", choices = regioniList,selected="Lombardia", options = pickerOptions(size=10,actionsBox = T ,selectedTextFormat = "count >20",deselectAllText='Deseleziona tutto',selectAllText='Seleziona tutto'), multiple = TRUE)
									),
									column(3,
										uiOutput("uiProvSelSerieStoricheProv")
									)
								),
								addSpinner(plotlyOutput('nuoviPositiviStoricoProv'), spin = "fading-circle", color = "#add437"),
								spiegaGraficoCasiGiornalieriProvincia
							),
							##################################################
							br(),
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
	dashboardHeader3( pagename="CoVid-19 in Italia", logo_img = "logo_pangea_esteso.png", width = 200,

		tags$li(uiOutput('data_agg') ,class='dropdown', style="color:#0b4975;font-weight: bold;margin-top:40px;margin-right: 40px;margin-left: 60px;")


												 ),


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
				menuItem2("Diario ", tabName = "diario", icon = icon("fas fa-book-open"))
			),
			sidebarMenu(id='presentazione',
				menuItem2("Chi Siamo", tabName = "chisiamo", icon = icon("fas fa-users"))
			),
			uiOutput('tasti_social')
		 #selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia")
	),

	dashboardBody(
		HTML('<script>
				  $( document ).on("shiny:sessioninitialized", function(event) {
					 var jsAgt = navigator.userAgent;
					 Shiny.onInputChange("GetNavUserAgent",jsAgt);
				 });</script>'),
	    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css"),
			tags$meta(property="og:title", content="CoVid-19 in Italia"),
			tags$meta(property="og:image",content="plot.png"),
			tags$meta(name="twitter:card",content="summary_large_image"),
			tags$link(rel = "icon",href ="logo_piccolo.png",type = "image/x-icon")

),


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
