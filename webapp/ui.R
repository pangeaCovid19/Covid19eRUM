library(shinydashboard)
source("dashboardPangea.R")

regTab <- tabItem(tabName = "regPlots", h1("Diffusione del virus nelle regioni italiane"), br(), br(),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati (ultima data)"), status = "primary", solidHeader = F,
                    collapsible = T, leafletOutput(outputId="mapRegion")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("table"), "Tabella con casi confermati (ultima data)"), status = "primary", solidHeader = F,
                    collapsible = T, dataTableOutput(outputId="tabRegion")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, plotlyOutput(outputId="lineRegion")
                )
              )
          )

prvTab <- tabItem(tabName = "prvPlots", h1("Diffusione del virus nelle province italiane"), br(), br(),
              #fluidRow(
              #  box(width=12, title = tagList(shiny::icon("filter"), "Seleziona regione"), status = "primary", solidHeader = F,
              #      collapsible = T, selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lazio")
              #  )
              #),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("globe-europe"), "Mappa dei casi confermati (ultima data)"), status = "primary", solidHeader = F,
                    collapsible = T, leafletOutput(outputId="mapProvince")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("table"), "Tabella con casi confermati (ultima data)"), status = "primary", solidHeader = F,
                    collapsible = T, DTOutput(outputId="tabProvince")
                )
              ),
              fluidRow(
                box(width=12, title = tagList(shiny::icon("analytics"), "Andamento dei casi confermati"), status = "primary", solidHeader = F,
                    collapsible = T, plotlyOutput(outputId="lineProvince")
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
        sidebarMenu(id='configs',
            menuItem2("CoVid-19 per regione", tabName = "regPlots", icon = icon("vials"))
        ),
        sidebarMenu(id='configs',
            menuItem2("CoVid-19 per provincia", tabName = "prvPlots", icon = icon("vials"),
            selectInput("regionSel", label="Regione", choices=regioniList, selected = "Lazio"))
        )
	),

	dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pangea.css")),

    tabItems(
        regTab,
        prvTab
	))
)
