library(shinydashboard)
source("dashboardPangea.R")

fitTab <- tabItem(tabName = "fitPlots",
#							h1("Analisi previsionale nelle province italiane"), br(), br(),
							fluidRow(
								box(width=12,
									h4("Sia gli andamenti totali che quelli delle regioni maggiormente colpite si stanno staccando dall'andamento esponenziale, questo sembra indicare che le misure preventive iniziano ad avere effetto."),
									 br(),
									fluidRow(
										column(width=4, selectizeInput("regionSelFit", label="Regione", choices=regioniList, selected = regioni2fit, multiple=TRUE, width='400px')),
										column(width=4, selectizeInput("regionLinLogFit", label="Tipo Grafico", choices=c("Lineare", "Logaritmico"), selected = "Lineare", width='200px'))
									),
									uiOutput('graficiPrevisioniUI')
#									box(width=6, title = tagList(shiny::icon("globe-europe"), "Totali Positivi per regione con previsione a 3 giorni"), status = "primary", solidHeader = F,
#											collapsible = T, plotlyOutput(outputId="fitRegion"), spiegaFitPos
#									),
#									box(width=6, title =  "Andamenti globali in Italia con previsione a 3", status = "primary", solidHeader = F,
#											collapsible = T,  plotlyOutput(outputId="fitIta"), spiegaFitTot
#									)
								)
							),
#              fluidRow(
 #               box(width=6, title = tagList(shiny::icon("globe-europe"), "Totali Positivi per regione con previsione a 3 in scala logaritmica"), status = "primary", solidHeader = F,
  #                  collapsible = T, plotlyOutput(outputId="fitRegLog"), spiegaFitPosLog
   #             ),
#								box(width=6, title = tagList( "Andamenti globali in Italia con previsione a 3 in scala logaritmica"), status = "primary", solidHeader = F,
 #                   collapsible = T,  plotlyOutput(outputId="fitItaLog"), spiegaFitTotLog
 #               )
 #             ),
              fluidRow(
                box(width=12, uiOutput("updatePrevisioniUI"), fontiDati
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
										selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia"),
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
				menuItem2("CoVid-19 previsioni", tabName = "fitPlots", icon = icon("vials"))
			),
      sidebarMenu(id='regCFG',
        menuItem2("CoVid-19 per regione", tabName = "regPlots", icon = icon("vials"))
      ),
      sidebarMenu(id='prvCFG',
        menuItem2("CoVid-19 per provincia", tabName = "prvPlots", icon = icon("vials"))
      )#,
		 #selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia")
	),

	dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css")),

    tabItems(
				fitTab,
        regTab,
        prvTab
	))
)
