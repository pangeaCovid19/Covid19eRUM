library(shinydashboard)
source("dashboardPangea.R")

regTab <- tabItem(tabName = "regPlots", h1("Diffusione nelle regioni italiane"), br(), br(),
              fluidRow(
                box(width=12, uiOutput("updateRegUI")
                )
              ),

              fluidRow(
                box(width=6, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaMappa, #plotOutput(outputId="mapRegionGG", height = 800)
                    leafletOutput(outputId="mapRegion", height = 800)
                ),
								box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaTabella, DTOutput(outputId="tabRegion")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaLinePlot, plotlyOutput(outputId="lineRegion")
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots", h1("Diffusione nelle province italiane"), br(), br(),
              fluidRow(
                box(width=12, uiOutput("updatePrvUI")
                )
              ),
              fluidRow(
                box(width=6, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaMappa, #plotOutput(outputId="mapProvinceGG", height = 800)
                    leafletOutput(outputId="mapProvince", height = 800)
                ),
								box(width=6, title = tagList(shiny::icon("table"), "Tabella con casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaTabella,  DTOutput(outputId="tabProvince")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, spiegaLinePlot, plotlyOutput(outputId="lineProvince")
                )
              )
            )

fitTab <- tabItem(tabName = "fitPlots", h1("Analisi previsionale nelle province italiane"), br(), br(),
              fluidRow(
                box(width=12, uiOutput("updatePrevisioniUI")
                )
              ),
              fluidRow(
                box(width=6, title = tagList(shiny::icon("globe-europe"), "Previsione a 3 giorni dei casi totali regionali"), status = "primary", solidHeader = F,
                    collapsible = T, plotlyOutput(outputId="fitRegion")
                ),
								box(width=6, title = tagList(shiny::icon("table"), "Andamento a livello nazionale"), status = "primary", solidHeader = F,
                    collapsible = T,  plotlyOutput(outputId="fitIta")
                )
              ),
              fluidRow(
                box(width=6, title = tagList(shiny::icon("globe-europe"), "Previsione a 3 giorni dei casi totali regionali"), status = "primary", solidHeader = F,
                    collapsible = T, plotlyOutput(outputId="fitRegLog")
                ),
								box(width=6, title = tagList(shiny::icon("table"), "Andamento a livello nazionale"), status = "primary", solidHeader = F,
                    collapsible = T,  plotlyOutput(outputId="fitItaLog")
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
      sidebarMenu(id='regCFG',
        menuItem2("CoVid-19 per regione", tabName = "regPlots", icon = icon("vials"))
      ),
      sidebarMenu(id='prvCFG',
        menuItem2("CoVid-19 per provincia", tabName = "prvPlots", icon = icon("vials"))
      ),
      sidebarMenu(id='fitCFG',
        menuItem2("CoVid-19 previsioni", tabName = "fitPlots", icon = icon("vials"))
      ),
      #uiOutput("drangeUI"),
      dateRangeInput("drangeSel", label="Periodo di interesse", start = date0, end = Sys.Date(), min = date0,
        max = Sys.Date(), format = "dd-mm-yyyy", startview = "month", weekstart = 1,
        language = "it", separator = " a "),
      selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lombardia")
	),

	dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css")),

    tabItems(
        regTab,
        prvTab,
				fitTab
	))
)
