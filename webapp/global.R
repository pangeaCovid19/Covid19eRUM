library(ggplot2)
library(sf)
library(plotly)
library(leaflet)
library(DT)
options(bitmapType="cairo")


d3cols <- "1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf"
d3hexcols <- paste0("#",regmatches(d3cols, gregexpr(".{6}", d3cols))[[1]])
d3col1 <- d3hexcols[1]

d3cols20 <- "1f77b4aec7e8ff7f0effbb782ca02c98df8ad62728ff98969467bdc5b0d58c564bc49c94e377c2f7b6d27f7f7fc7c7c7bcbd22dbdb8d17becf9edae5"
d3hexcols20 <- paste0("#",regmatches(d3cols20, gregexpr(".{6}", d3cols20))[[1]])

my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 7),
              axis.text = element_text(size=7),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}


pop_file <- read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(pop_file) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")


data_files <- list.files("www/pcm_data", full.names=T)
data_files <- data_files[grepl(".csv$", data_files) | grepl(".txt$", data_files)]
date_range <- as.Date(gsub(".*/.*-(\\d{,8}).txt$", "\\1", data_files), format="%Y%m%d")
date0 <- min(date_range)

get_covid19_data <- function(flist) {
  do.call(rbind, lapply(flist, function(ff) {
    temp <- read.csv(ff, stringsAsFactors=F)
    temp$data <- as.Date(temp$data)
    temp$denominazione_regione[temp$denominazione_regione %in% c("Bolzano", "Trento")] <- "Trentino - Alto Adige"
    temp <- merge(temp, pop_file[,c("codice_provincia", "pop")], by="codice_provincia")
    temp
  }))
}

allData <- get_covid19_data(data_files)
regioniList <- sort(unique(allData$denominazione_regione))


regioni <- st_read("www/Reg01012019/Reg01012019_WGS84.shp")
regioni <- st_transform(regioni, crs="+proj=longlat +datum=WGS84 +no_defs")
hlpr <- st_coordinates(st_centroid(regioni))
colnames(hlpr) <- c("reg_long", "reg_lat")
regioni <- cbind(regioni, hlpr)

province <- st_read("www/ProvCM01012019/ProvCM01012019_WGS84.shp")
province <- st_transform(province, crs="+proj=longlat +datum=WGS84 +no_defs")
#hlpr <- st_coordinates(st_centroid(province))
#colnames(hlpr) <- c("prv_long", "prv_lat")
#province <- cbind.data.frame(province, hlpr)


spiegaMappa <- HTML("<div style='padding-bottom:10px;'>In questa mappa mostriamo la diffusione sul territorio dei casi confermati
di CoVid19, alla data pi&ugrave; recente del periodo di interesse selezionato nel men&ugrave; (o alla data di aggiornamento dei dati)
laterale. <br>La scala di colore parte dal giallo per le aree con il minor numero assoluto di
casi confermati e arriva al rosso per le aree con il maggior numero assoluto di casi confermati.
<br>Passando sulla mappa potete vedere l'effettivo numero di casi confermati in ciasuna area.</div>")

spiegaTabella <- HTML("<div style='padding-bottom:10px;'>In questa tabella mostriamo la diffusione sul territorio dei casi confermati
di CoVid19, alla data pi&ugrave; recente del periodo di interesse selezionato nel men&ugrave; (o alla data di aggiornamento dei dati)
laterale. <br>La colonna &quot;casi totali&quot; riporta il numero totale di casi confermati di CoVid19
nel territorio, mentre la colonna &quot;casi su 10^4 abit.&quot; riporta il numero di casi per ogni
10mila abitanti cos&igrave; da contestualizzare la diffusione rispetto alla popolazione presente
nel territorio.</div>")

spiegaLinePlot <- HTML("<div style='padding-bottom:10px;'>In questo grafico mostriamo l'andamento del numero di casi confermati
di CoVid19, nel periodo di interesse selezionato nel men&ugrave; laterale. <br>Ciascuna area territoriale
&egrave; indicata con un colore diverso. &Egrave; possibile ingrandire aree specifiche del grafico
e disabilitare (o riabilitare) singoli territori interagendo con la legenda del grafico.</div>")
