dashboardHeader2<-function(..., title = NULL, disable = FALSE,pagename,.list = NULL,logo_img,link="http://www.pangeaformazione.it/",width,m_up=0,m_dx=0,m_dw=0,m_sx=0) {
	items <- c(list(...), .list)
	lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
	tags$header(class="main-header",style = if (disable) "display: none;",
		tags$span(class="logo",pagename),
		tags$nav(class="navbar navbar-static-top",role="navigation",span(shiny::icon("bars"), style = "display:none;"),
			tags$a(href="#",class="sidebar-toggle",role="button",`data-toggle`="offcanvas",
				tags$span(class="sr-only",'Toggle navigation')
			),
			tags$div(class="navbar-custom-menu",
				tags$ul(class="nav navbar-nav",items,
					tags$li(tags$a(href=link,target="_blank",(img(src=logo_img,class="img",width=width,border="0", style=paste("margin:",m_up,"px ",m_dx,"px ",m_dw,"px ",m_sx,"px;",sep="")))))
				)
			)
		)
	)
}

dashboardHeader3<-function(..., title = NULL, disable = FALSE, pagename, .list = NULL,
logo_img,link="http://www.pangeaformazione.it/",width,m_up=0,m_dx=0,m_dw=0,m_sx=0)
{
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
    tags$header(class = "main-header", style = if (disable) "display: none;",
        tags$li(class="logo",
					tags$a(href=link,target="_blank",(img(src=logo_img,class="img",width=width,border="0", style=paste("margin:",m_up,"px ",m_dx,"px ",m_dw,"px ",m_sx,"px;",sep="")))
					#))
				)
			),
		tags$nav(class = "navbar navbar-static-top",
        	role = "navigation",
        	span(shiny::icon("bars"),style = "display:none;"),
        	a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
            	role = "button", span(class = "sr-only", "Toggle navigation")),
            tags$span(class="navbar-text",pagename),
        tags$nav(class = "navbar-right",
        tags$ul(class = "nav navbar-nav",
        items
        )
        ))
       )
}

menuItem2<-function (text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green",
    tabName = NULL, href = NULL, newtab = TRUE, selected = NULL)
{
    subItems <- list(...)
    if (!is.null(icon))
        shinydashboard:::tagAssert(icon, type = "i")
    if (!is.null(href) + (!is.null(tabName) + (length(subItems) >
        0) != 1)) {
        stop("Must have either href, tabName, or sub-items (contained in ...).")
    }
    if (!is.null(badgeLabel) && length(subItems) != 0) {
        stop("Can't have both badge and subItems")
    }
    shinydashboard:::validateColor(badgeColor)
    isTabItem <- FALSE
    target <- NULL
    if (!is.null(tabName)) {
        isTabItem <- TRUE
        href <- paste0("#shiny-tab-", tabName)
    }
    else if (is.null(href)) {
        href <- "#"
    }
    else {
        if (newtab)
            target <- "_blank"
    }
    if (!is.null(badgeLabel)) {
        badgeTag <- tags$small(class = paste0("badge pull-right bg-",
            badgeColor), badgeLabel)
    }
    else {
        badgeTag <- NULL
    }
    if (length(subItems) == 0) {
        return(tags$li(a(href = href, `data-toggle` = if (isTabItem) "tab",
            `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
            target = target, icon, span(text), badgeTag)))
    }
    tags$li(class = "treeview", a(href = href, `data-toggle` = if (isTabItem) "tab",
            `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
            target = target, icon, span(text), badgeTag,shiny::icon("angle-left", class = "pull-right")), tags$ul(class = "treeview-menu",
        subItems))
}


menuSubItem2<-function (text, ...,tabName = NULL, href = NULL, newtab = TRUE, icon = shiny::icon("angle-double-right"),
    selected = NULL)
{
    subItems <- list(...)
    if (!is.null(href) && !is.null(tabName)) {
        stop("Can't specify both href and tabName")
    }
    isTabItem <- FALSE
    target <- NULL
    if (!is.null(tabName)) {
        isTabItem <- TRUE
        href <- paste0("#shiny-tab-", tabName)
    }
    else if (is.null(href)) {
        href <- "#"
    }
    else {
        if (newtab)
            target <- "_blank"
    }
     if (length(subItems) == 0) {
        return(tags$li(a(href = href, `data-toggle` = if (isTabItem) "tab",
            `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
            target = target, icon, span(text))))
    }
    tags$li(class = "treeview", a(href = href, `data-toggle` = if (isTabItem) "tab",
            `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
            target = target, icon, span(text),shiny::icon("angle-left", class = "pull-right")), tags$ul(class = "treeview-menu",
        subItems))

}

dropdownMenu2<-function (..., type = c("messages", "notifications", "tasks"),
    badgeStatus = "primary", icon = NULL, .list = NULL,width=230,header=NULL)
{
    type <- match.arg(type)
    if (!is.null(badgeStatus))
        shinydashboard:::validateStatus(badgeStatus)
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    dropdownClass <- paste0("dropdown ", type, "-menu")
    if (is.null(icon)) {
        icon <- switch(type, messages = shiny::icon("envelope"),
            notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
    }
    numItems <- length(items)
    if (is.null(badgeStatus)) {
        badge <- NULL
    }
    else {
        badge <- span(class = paste0("label label-", badgeStatus),
            numItems)
    }
    if(is.null(header))
    		return(tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",
        `data-toggle` = "dropdown", icon, badge), tags$ul(class = "dropdown-menu", style=paste0("width:",width,"px;"),
        tags$li(class = "header", paste("You have", numItems,
            type)), tags$li(tags$ul(class = "menu", items)))))
	    		return(tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",
        `data-toggle` = "dropdown", icon, badge), tags$ul(class = "dropdown-menu", style=paste0("width:",width,"px;"),
        tags$li(class = "header", paste(header,numItems)), tags$li(tags$ul(class = "menu", items)))))

}


jscodescreen <-
'	$(document).on("shiny:connected", function(e) {
	  var jsWidth = screen.width;
	  Shiny.onInputChange("GetScreenWidth",jsWidth);
	  var jsHeight = screen.height;
	  Shiny.onInputChange("GetScreenHeight",jsHeight);
	  var jsWWidth = window.innerWidth;
	  Shiny.onInputChange("GetWindowWidth",jsWWidth);
	  var jsWHeight = window.innerHeight;
	  Shiny.onInputChange("GetWindowHeight",jsWHeight);
	});
	$(window).resize(function(e) {
	  var jsWidth = screen.width;
	  Shiny.onInputChange("GetScreenWidth",jsWidth);
	  var jsHeight = screen.height;
	  Shiny.onInputChange("GetScreenHeight",jsHeight);
	  var jsWWidth = window.innerWidth;
	  Shiny.onInputChange("GetWindowWidth",jsWWidth);
	  var jsWHeight = window.innerHeight;
	  Shiny.onInputChange("GetWindowHeight",jsWHeight);
	});
'
dropdownMenuITA<-function (..., type = c("messages", "notifications", "tasks"),
    badgeStatus = "primary", icon = NULL, .list = NULL)
{
    type <- match.arg(type)
    if (!is.null(badgeStatus))
        shinydashboard:::validateStatus(badgeStatus)
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    dropdownClass <- paste0("dropdown ", type, "-menu")
    if (is.null(icon)) {
        icon <- switch(type, messages = shiny::icon("envelope"),
            notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
    }
    numItems <- length(items)
    if (is.null(badgeStatus)) {
        badge <- NULL
    }
    else {
        badge <- span(class = paste0("label label-", badgeStatus),
            numItems)
    }
    tipoITA<-setNames(c("messaggi","allarmi","incarichi"),c("messages", "notifications", "tasks"))
    tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",
        `data-toggle` = "dropdown", icon, badge), tags$ul(class = "dropdown-menu",
        tags$li(class = "header", paste("Ci sono", numItems,
            tipoITA[type])), tags$li(tags$ul(class = "menu", items))))
}
