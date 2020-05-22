library(ggplot2)
library(sf)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
library(zoo)
source("funzionifit.R")
source("gompertz.R")
options(bitmapType="cairo")

animazione <- FALSE
scalaSingolaProvincia <-FALSE
lingua <- "en"

verbose <- TRUE
assignout <- TRUE
saveRDSout <- FALSE

regioni2fit <- c('Lombardia', 'Emilia Romagna', 'Veneto')
province2fit <- c('Bergamo', 'Brescia', 'Milano')

dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"
dir_data	<- "www/"

provRDS <- "dataProvince.RDS"
regRDS <- "dataRegioni.RDS"
provShapeRDS <- "ProvinceShapeF.RDS"
regShapeRDS <- "RegioniShapeF.RDS"

campiPrevisioni <- c("totale_casi", "deceduti", "totale_ospedalizzati", "terapia_intensiva")

campiTotali <- c("totale_casi", "tamponi", "totale_attualmente_positivi", "deceduti", "totale_ospedalizzati", "terapia_intensiva", "ricoverati_con_sintomi" , "nuovi_attualmente_positivi" , "dimessi_guariti")


pop_file <- read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(pop_file) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")

letti2018 <- readRDS("www/letti2018.RDS")
TIindx <- grep("TERAPIA INTENSIVA", letti2018$Descrizione.disciplina)
Tintensiva <- aggregate(letti2018[TIindx, 'Totale.posti.letto'],by=list( denominazione_regione=letti2018[TIindx, 'Descrizione.Regione']), sum)
setnames(Tintensiva, old="x", new="lettiTI")

letality<-read.csv2(file="report_iss/Letality.csv",stringsAsFactors=FALSE)


# Temi dell'applicazione
d3cols <- "1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf"
d3hexcols <- paste0("#",regmatches(d3cols, gregexpr(".{6}", d3cols))[[1]])
d3col1 <- d3hexcols[1]

d3cols20 <- "1f77b4aec7e8ff7f0effbb782ca02c98df8ad62728ff98969467bdc5b0d58c564bc49c94e377c2f7b6d27f7f7fc7c7c7bcbd22dbdb8d17becf9edae5"
d3hexcols20 <- paste0("#",regmatches(d3cols20, gregexpr(".{6}", d3cols20))[[1]])
d3hexcols20[12]<-'#f5d742'
  d3hexcols20[11]<-'#d4a928'
  d3hexcols20[17]<-'#bcd11b'


colori<-c("#CED6A7", "#F4F49C", "#D162F1", "#5E4631", "#5F0D7C", "#2F7562", "#969881",
"#E74C63", "#DD43A7", "#16A02F", "#f3b0ff", "#BDA78E", "#2766BB", "#F193D4",
 "#ABD9E8", "#2676B8", "#e06936", "#F4CE99", "#8F9A15", "#2487B6", "#DF4596",
 "#DA41B8", "#BC8EEF", "#73CEDC", "#8A254E", "#E691F0", "#E94E51", "#B02424",
 "#2955BD", "#ACD081", "#2297B3", "#4B60DB", "#B4C0BB", "#E3AD44", "#ABE2E8",
 "#606F57", "#306F3B", "#D83FC9", "#D3B25A", "#ABE7AF", "#E9E7E7", "#C3B770",
 "#B3BB85", "#5030C7", "#BB2C49", "#9036CE", "#476930", "#7A34CC", "#626431",
 "#C68F61", "#FD7D62", "#93C5B1", "#880D51", "#F295AE", "#A5F8DE", "#24B024",
 "#EB6251", "#f5d742", "#1CAA7E", "#D0F59E", "#F4A92F", "#850D6B", "#3B2FC4",
 "#4098D1", "#8456E6", "#D13DD6", "#1FAFA7", "#1DAC92", "#E44A74", "#159E1D",
 "#334736", "#711173", "#593134", "#9A4C15", "#17A243", "#1AA76A", "#141F1C",
 "#7E0D81", "#36C6BB", "#430D78", "#A1543E", "#EE7853", "#280D74", "#20A7B1",
 "#A9E2C3", "#2CBB6F", "#2B44C0", "#A2F7B9", "#BB3AD3", "#6532C9", "#A538D1",
 "#D95E08", "#2D33C2", "#8F5D0F", "#A3C09B", "#B2DCA8", "#0F0D71", "#A5381C",
 "#5BA51C", "#83C9C6", "#CBA4A8", "#F3A697", "#CF7634", "#ADF6A0", "#0C216D",
"#E24885", "#B9AA5D")

color_province<-colori


my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 7),
              axis.text = element_text(size=7),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}

my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 10),
              axis.text = element_text(size=10),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}

# dati epidemiolocigi
allData_prv <- readRDS(paste0(dir_prov,provRDS))
allData_reg_flt <- readRDS(paste0(dir_prov,provRDS))
## globalizziamo le aggregazioni per le mappe...
allData_prv <- allData_prv[!grepl("aggiornamento", allData_prv$denominazione_provincia),]
allData_prv <- aggregate(list(totale_casi=allData_prv$totale_casi, pop=allData_prv$pop),
                  by=list(data=allData_prv$data, denominazione_regione=allData_prv$denominazione_regione,
                          denominazione_provincia=allData_prv$denominazione_provincia, codice_provincia=allData_prv$codice_provincia),
                  FUN=sum)
allData_prv$`casi su 10mila abit` <- round(allData_prv$totale_casi / allData_prv$pop * 10000, 3)

allData_reg_flt <- allData_reg_flt[!grepl("aggiornamento", allData_reg_flt$denominazione_provincia),]
allData_reg_flt <- aggregate(list(totale_casi=allData_reg_flt$totale_casi, pop=allData_reg_flt$pop),
                              by=list(data=allData_reg_flt$data,
                                  denominazione_regione=allData_reg_flt$denominazione_regione, codice_regione=allData_reg_flt$codice_regione),
                              FUN=sum)
allData_reg_flt$`casi su 10mila abit` <- round(allData_reg_flt$totale_casi / allData_reg_flt$pop * 10000, 3)
## fine parte per mappe
regioniList <- sort(unique(allData_reg_flt$denominazione_regione))
provinceList <- sort(unique(allData_prv$denominazione_provincia))

color_regioni<-d3hexcols20
names(color_regioni)<-regioniList

names(color_province)	 <- provinceList


allData_reg <- readRDS(paste0(dir_reg, regRDS))
mtimeProv <- file.info(paste0(dir_prov,provRDS))$mtime
mtimeReg 	<-file.info(paste0(dir_reg,regRDS))$mtime

modelliReg <- readRDS("www/modelliReg.RDS")
modelliIta <-readRDS("www/modelliIta.RDS")

modelliRegGomp <- readRDS("www/modelliRegGomp.RDS")
modelliItaGomp <-readRDS("www/modelliItaGomp.RDS")

if(file.exists("www/modelliRegExp.RDS"))
	modelliRegExp <- readRDS("www/modelliRegExp.RDS")
if(file.exists("www/modelliItaExp.RDS"))
	modelliItaExp <-readRDS("www/modelliItaExp.RDS")

date_range_prv <- range(allData_prv$data)
date_range_reg <- range(allData_reg$data)

date0_prv <- min(date_range_prv, na.rm=T)
date0_reg <- min(date_range_reg, na.rm=T)

province <- readRDS(paste0(dir_prov, provShapeRDS))
regioni <- readRDS(paste0(dir_reg, regShapeRDS))

eu_to_plot <- readRDS(file=paste0(dir_data, "eu_to_plot.RDS") )
italy <- readRDS(file=paste0(dir_data, "italy.RDS") )

map_italia <- readRDS(file=paste0(dir_data, "map_italia.RDS"))
map_regioni <- readRDS(file=paste0(dir_data, "map_regioni.RDS"))


risultatiFit <- HTML("<div style='padding-top:10px;'></div>")


prvReg <- unique(allData_prv[, c('denominazione_regione', 'denominazione_provincia')])
setDT(prvReg)

spiegaFitMedioTermine<- HTML("<div style='padding-top:10px;'>In questo grafico viene mostrato il numero di cati totali (deceduti) con una previsione che si estende fino a 10 giorni: in blu la serie storica, in arancione i valori previsti per i prossimi giorni.</div>")

spiegaVariazionePercentuale<- HTML("<div style='padding-top:10px;'>This graph shows the percentage increase of total cases compared to the previous day. In case of an exponential increase this value would be exponential on average, while in case of a quadratical exponential increase we'd see a downward straight line The decrease of percentage variation of total cases means that the virus' expansion rate is decreasing. This could be either due to the fact that the number of infected has grown up to a point so critical as to make further virus propagations slower, or it could be a result of containment measures. We are convinced this decrease is in fact due to an effective containment.</div>")

spiegaConfrontoSerieRegioni <- HTML("<div style='padding-top:10px;'>Here it's possible to confront the time series of infected (or deceased) cases for each region of Italy. Thanks to the input sliders, it's possible to move each time series back and forth.</div>")

spiegaConfrontoSerieProvince <- HTML("<div style='padding-top:10px;'>Here it's possible to confront the time series of infected (or deceased) cases for each province of Italy. Thanks to the input sliders, it's possible to move each time series back and forth.</div>")

spiegaFitPos <- HTML("<div style='padding-top:10px;'>This graph shows the time series of confirmed CoVid19 cases in the selected regions, plus a forecasting which extends up to three days (dashed line). All graphs are interactive (i.e., cursor hovering over points and lines displays their exact values).
We've used exponential models, prioritizing contributions from more recent data. By selecting &quot;Logaritmic&quot; as graph type, the y axis becomes logarithmic, so exponential trends become straight lines.</div>")

spiegaFitTot <- HTML("<div style='padding-top:10px;'>This graph shows the time series of confirmed cases, hospitalized patients, occupied ICUs and deaths due to CoVid19 in Italy, as well as a forecast which extends up to three days (dashed line). All graphs are interactive (i.e., it's possible to know the exact value of points and lines by hovering on them with the cursor. We've used exponential models, giving higher priority to more recent data. By selecting the &quot;Logarithmic&quot graph option, the Y axis becomes logarithmic, and exponential trends achieve a linear representation.</div>")

spiegaFitTotePos<- HTML("<div style='padding-top:10px;'>Both graphs show the time series of confirmed CoVid19 cases in Italy, plus a forecast extending up to three days (dashed line). In the first one they are represented by selected region, while in the second they're divided between hospitalized patients, occupied ICUs and deceased.
We've used exponential models, giving higher priority to more recent data. By selecting the &quot;Logarithmic&quot graph option, the Y axis becomes logarithmic, and exponential trends achieve a linear representation
</div>")

spiegaMappa <- HTML("<div style='padding-top:10px;'>This map shows territorial diffusion of confirmed CoVid19 cases.
<br>The color scale starts on yellow for areas with the lowest absolute number confirmed cases, and transitions all the way to red for areas with highest absolute number of confirmed cases.
<br>Hovering on the map, you can see the actual number of confirmed cases for each area.</div>")

spiegaTabella <- HTML("<div style='padding-top:10px;'>This table shows territorial diffusion of confirmed CoVid19 cases.
<br>The &quot;total cases&quot; column shows total number of confirmed CoVid19 in the area, while the column &quot;cases out of 10 thousand people&quot; shows the number of cases out of each 10 thousand people, so as to contextualize the diffusion compared to the total population living in the area.</div>")


spiegaTabellaMonitor <- HTML("<div style='padding-top:10px;'>This table shows a few quantities which are useful in order to monitor the epidemic spread in different Italian areas.
The growth rate (i.e., the percentage of new cases over total cases in the previous day), is also shown as an average over the last 7 days. Clearly, there are certain weekly regularities with regards to the number of analyzed swabs (and thus for the number of new confirmed cases): less swabs are performed at week-ends.</div>")

spiegaLinePlot <- HTML("<div style='padding-top:10px;'>This graph shows the tren of new confirmed CoVid19 cases.
<br>Each area is highlighted by a different color. It's possible to zoom specific areas on the graph, and to unfocus (or focus) on single areas by interacting with the graphs' legend.</div>")

spiegaTabellaCompare <- HTML("<div style='padding-top:10px;'>This table shows forecasts for the two implemented models and observed values for total cases, deaths, hospitalizations and ICU patients.
<br> Going back in time, it's possible to see how exponential model forecasts gradually depart from true values, overextimating both total cases and deaths. Tha quadratic exponential model manages to forecast the epidemic trend with better accuracy, although in the last days it's clear this model as well is constantly underestimating case increment: we're out of the quadratic regime as well. At the present, in order to model the phenomenon, we should also consider some factors that aren't identifiable given daily Civil Protection data only.</div>")


# Descrizione Terapia Intensiva
spiegaTerIntPrevisione <- HTML("<div style='padding-top:10px;'>This graph shows the number of ICUs, updated to 2018, and forecasts for the number of patients who are in need of intensive care. Since we don't have sufficient data for each region, projections are carried out multiplying the number of ICUs over total cases times the number of cases for each region. The vertical bars represent standard uncertainty. Data concerning availability of beds is updated to 2018, so it doesn't take into account the continuous effort that the National Health System is undergoing in order to rapidly increase availability.</div>")

spiegaTerIntPercentuale <- HTML("<div style='padding-top:10px;'>This graph shows the percentage of occupied ICUs over the total number of available beds, updated to 2018.</div>")


spiegaTerIntAttuale <-  HTML("<div style='padding-top:10px;'>This graph displays the number of ICU beds, updated to 2018, and the number of ICU patients with CoVid19 for each region.</div>")


spiegaTerIntNuoviCasi <-  HTML("<div style='padding-top:10px;'>This graph shows the number of patients that need to be hospitalized.</div>")





#########################

spiegaTabellaGompertz <- HTML("<div style='padding-top:10px;'>This table shows the asymptotical values (i.e. the highest reached values), using a Gompertz model updated to the specified date. By monitoring the trend of the asymptote (Total Cases and Deceased) it's possible to understand how the epidemic trend is changing: a growing trend implies the diffusion's speed is increasing. Between the second and the fourth week of Phase 2 we expect to witness an increase in the asymptotical values, which we hope will become stable around values that don't exceed too much the ones that have been forecasted today.</div>")


spiegaGraficoCasiGiornalieriRegioni <- HTML("<div style='padding-top:10px;'> This graph shows the number of new daily cases, divided by region.</div>")


spiegaGraficoNuoviPositiviStoricoRegPercentuale <- HTML("<div style='padding-top:10px;'> This graph displays the trend of the fraction between new cases and total cases on the previous day, as a function of time. This parameter, which is directly proportional to R0, has had an exponentiallly decreasing trend since the beginning of the lockdown. In the logarithmic y-axis' graph, its trend is linearly decreasing. During Phase 2 we expect to witness a deviation from this trend: the increase rate will probably start to grow again between the second and the third week of May.</div>")


spiegaGraficoNuoviPositiviStoricoPrvPercentuale <- HTML("<div style='padding-top:10px;'> This graph displays the trend of the fraction between new cases and total cases on the previous day, as a function of time. This parameter, which is directly proportional to R0, has had an exponentiallly decreasing trend since the beginning of the lockdown. In the logarithmic y-axis' graph, its trend is linearly decreasing. During Phase 2 we expect to witness a deviation from this trend: the increase rate will probably start to grow again between the second and the third week of May.</div>")


spiegaGraficoCasiGiornalieriProvincia <- HTML("<div style='padding-top:10px;'> This graph displays the number of new daily cases, divided by province.</div>")


spiegaGraficoCasiVsCasiNuovi <- HTML("<div style='padding-top:10px;'> This graph displays the number of new observed cases in the last 7 days as a function of the number of total cases. We know the growth tend for the number of cases can't grow exponentially for a long time, but how can we frame the moment in which the trend is changing? This graph helps us visualize that moment: the exponential growth is charachterized by a number of daily infections which is proportional to the number of total ones. Such a trend translates to a straight line in this graph. The moment when the growth will stop being exponential we'll start to see the curve bend down.</div>")


fontiDati <- HTML("<div>Data kindly provided by:
<ul>
<li><a href='https://github.com/pcm-dpc/COVID-19'>Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile
</a> (Monitoraggio COVID-19)</li>
<li><a href='https://www.istat.it/it/archivio/222527'>ISTAT</a> (shapefile unit&agrave; amministrative e dati popolazione)</li>
<li><a href='
http://www.dati.salute.gov.it/dati/dettaglioDataset.jsp?menu=dati&idPag=96'>Ministero della Salute</a>
(Numero di posti letto in terapia intensiva, dati aggiornati al 2018)</li>
</ul></div>")
