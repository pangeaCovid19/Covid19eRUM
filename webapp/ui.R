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
											HTML(readChar("../docs/chisiamo.html",file.info("../docs/chisiamo.html")$size)))))

diariotab<-tabItem(tabName="diario",

				uiOutput("spaces_mobile_diario"),

							fluidRow(style="padding:30px;border-style: solid;border-color: rgb(243,118,37);",

											h1("Diario"),br(),

											fluidRow(style="padding:30px;background-color:#ffffff;",
											h2("Editoriale"),br(),
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

									h4("I dati relativi al numero di posti letto in terapia intensiva per regione sono aggiornati al 2018 e non vengono riaggiornati in base agli sforzi che il sistema sanitario sta portando avanti in questi giorni. Non ha scopo allarmistico ma solo di mostrare quali siano le criticità che il nostro paese sta affrontando a causa del CoVid19 "),
									 br(),

										fluidRow(style="padding:20px;background-color:#ffffff",
											h3("Previsione del numero di letti occupati da pazienti con Covid19 e disponibilità per regione (posti letto aggiornati al 2018)"),
											plotlyOutput("terapiaIntPlotPercPrev"),
#											plotlyOutput("terapiaIntPlotPercPrevNEW"), calcolo terapia intensiva su numero di terapie intensive per farlo partire ricaricare il demone
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
							fluidRow(style="overflow-x:scroll;padding:20px;",plotlyOutput(outputId="lineRegioni")),
							#uiOutput('legenda_regioni'),
							spiegaLinePlot,
							#plotlyOutput(outputId="puntiRegioni"),
							##################################################
							##################################################
							#FIXME per togliere i grafici nuovi commentare fino a plotlyOutput(outputId="puntiRegioni")
							fluidRow(style="padding:20px;background-color:#ffffff",
								h3("Confronto tra variabili"),
								fluidRow(style="padding:10px;",
								column(width=3,
									selectizeInput("confrontox", label="Variabile su asse X", 	choices=campiTotali , selected = "totale_casi", multiple=FALSE)
								#),
								#column(width=3,
									,selectizeInput("confrontoy", label="Variabile su asse Y", 	choices=campiTotali , selected = "tamponi", multiple=FALSE)
								),
								column(width=2,offset=1,fluidRow(h1(' ')),
									prettyRadioButtons('confrontoTipoGratico',"Assi del grafico",choices = c('Lineari', 'Logaritmici') , selected = "Logaritmici", status = "primary",shape = 'round',outline = FALSE,animation = 'jelly',icon = icon('check'))
									#selectizeInput("confrontoTipoGratico", label="Assi del grafico", 	choices=c('Lineari', 'Logaritmici') , selected = "Logaritmici", multiple=FALSE)
								),
								column(width=6,fluidRow(h1(' ')),
									uiOutput("confrontoGiornoUI")
								))
							),
							fluidRow(style='padding:30px;',fluidRow(style='padding:20px;overflow-x:scroll;',plotlyOutput(outputId="puntiRegioni"))),
							#uiOutput('legenda_regioni_bullet'),
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
							fluidRow(style='padding:30px;',
								uiOutput("selProvince")),
								h3("Mappa dei casi confermati"),
										#plotOutput(outputId="mapProvinceGG", height = 800),
										leafletOutput(outputId="mapProvince"),
										spiegaMappa
								),br(),
								fluidRow(style="padding:40px;background-color:#ffffff",
								h3("Andamento dei casi confermati"),

								fluidRow(style="overflow-x:scroll;",align='center',plotlyOutput(outputId="lineProvince")),uiOutput('spazi_plot_province'), spiegaLinePlot
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
	dashboardHeader3( pagename="CoVid-19 in Italia", logo_img = "logo_pangea_esteso.png", width = 200      #,uiOutput('tasti_social')
	# tags$li(actionButton("twitter_share",label = "Twitter",color='#1DA1F2',icon = icon("twitter"),
	#          onclick = sprintf("window.open('%s')", url_tweet)),class='dropdown' ,
	# 				 tags$style(type='text/css', "#twitter_share { background-color:#1DA1F2;color:#ffffff;margin-top: 20px;margin-bottom: 20px;margin-right: 10px;margin-left: 10px;}")),
	#
	#          tags$li(actionButton("linkedin_share",label = "LinkedIn",color='#1DA1F2',icon = icon("linkedin-in"),
	#                onclick = sprintf("window.open('%s')", url_link)),class='dropdown' ,
	# 							 tags$style(type='text/css', "#linkedin_share { background-color:#0e76a8;color:#ffffff;margin-top: 20px;margin-bottom: 20px;margin-right: 10px;margin-left: 10px;}")),
	#
	#          tags$li(actionButton("fb_share",label = "Facebook",color='#4267B2',icon = icon("fab fa-facebook-f"),
	#                onclick = sprintf("window.open('%s')", url_fb)),class='dropdown' ,
	#                tags$style(type='text/css', "#fb_share { background-color:#0e76a8;color:#ffffff;margin-top: 20px;margin-bottom: 20px;margin-right: 30px;margin-left: 10px;}"))


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
