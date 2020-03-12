library(ggplot2)
library(sf)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
source("funzionifit.R")
options(bitmapType="cairo")
dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"
dir_data	<- "www/"

provRDS <- "dataProvince.RDS"
regRDS <- "dataRegioni.RDS"
provShapeRDS <- "ProvinceShapeF.RDS"
regShapeRDS <- "RegioniShapeF.RDS"


campiPrevisioni <- c("totale_casi", "decessi", "totale_ospedalizzati", "terapia_intensiva")

verbose <- TRUE

pop_file <- read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(pop_file) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")

# Temi dell'applicazione
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

# dati epidemiolocigi
allData <- readRDS(paste0(dir_prov,provRDS))
regioniList <- sort(unique(allData$denominazione_regione))
allData_reg <- readRDS(paste0(dir_reg, regRDS))
mtimeProv <- file.info(paste0(dir_prov,provRDS))$mtime
mtimeReg 	<-file.info(paste0(dir_reg,regRDS))$mtime


modelliReg <- readRDS("www/modelliReg.RDS")
modelliIta <-readRDS("www/modelliIta.RDS")


date_range 			<- range(allData$data)
date_range_reg 	<- range(allData_reg$data)

date0 		<- min(date_range, na.rm=T)
date0_reg <- min(date_range_reg, na.rm=T)

province <- readRDS(paste0(dir_prov, provShapeRDS))
regioni <- readRDS(paste0(dir_reg, regShapeRDS))

eu_to_plot <- readRDS(file=paste0(dir_data, "eu_to_plot.RDS") )
italy <- readRDS(file=paste0(dir_data, "italy.RDS") )

map_italia <- readRDS(file=paste0(dir_data, "map_italia.RDS"))
map_regioni <- readRDS(file=paste0(dir_data, "map_regioni.RDS"))



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












































# DEPRECATE


## shapefile addizionali per rimuovere uso di leaflet e alleggerire il carico della app

#eu_countries <- read_sf("www/EU_countries.shp/CNTR_BN_10M_2016_4326.shp")
#eu_info <- read.csv("www/EU_countries.shp/CNTR_RG_BN_10M_2016.csv")
#eu_info <- eu_info[!is.na(eu_info$CNTR_CODE),]
#eu_countries <- merge(eu_countries, eu_info, by.x="CNTR_BN_ID", by.y="CNTR_BN_CODE")

#italy <- eu_countries[eu_countries$CNTR_CODE=="IT",]
#ita_box <- st_as_sfc(st_bbox(italy), crs=st_crs(eu_countries))
#selezionati <- vapply(st_geometry(eu_countries), function(x) st_intersects(x, st_geometry(ita_box), sparse=F), TRUE)
#eu_to_plot <- eu_countries[selezionati,]
#st_agr(eu_to_plot) = "constant"
#eu_to_plot <- st_intersection(eu_to_plot, ita_box)
#eu_to_plot <- st_transform(eu_to_plot, crs="+proj=longlat +datum=WGS84 +no_defs")

#map_italia <- ggplot() +
#                  geom_sf(data = eu_to_plot, color="lightgrey", size=.5) +
#                  geom_sf(data = italy, color="black", size=.75) +
#                  labs(title="Casi in Italia", x="", y="") +
#                  my_ggtheme()

#rm(eu_countries)
#rm(eu_info)
#rm(ita_box)
#gc()

## a questo punto eu_to_plot rappresenta i confini delle nazioni vicino all'Italia
## e italy i contorni dell'Italia
## plot per provare
#ggplot() +
#  geom_sf(data = eu_to_plot, color="lightgrey", size=.5) +
#  geom_sf(data = italy, color="steelblue", size=.75) +
#  theme_minimal()





################################################################## DEPRECATE
##################################################################
# MAPPE ggplot
if(FALSE){

	map_italia <- ggplot() +
		geom_sf(data = eu_to_plot, color="lightgrey", size=.5) +
		geom_sf(data = italy, color="black", size=.75) +
		labs(title="Casi in Italia", x="", y="") +
		my_ggtheme()

	map_regioni <- lapply(regioni$COD_REG, function(cod) {
	                        box_strict <- st_as_sfc(st_bbox(regioni[regioni$COD_REG == cod,]), crs=st_crs(regioni))
	                        selezionati <- vapply(st_geometry(regioni), function(x) st_intersects(x, st_geometry(box_strict), sparse=F), TRUE)
	                        reg_to_plot <- st_boundary(regioni[selezionati,])
	                        reg_to_plot <- st_intersection(reg_to_plot, box_strict)
	                        ggplot() +
	                          geom_sf(data = reg_to_plot, color="lightgrey", size=.5) +
	                          geom_sf(data = regioni[regioni$COD_REG == cod,], color="black", size=.75) +
	                          labs(title=paste("Casi in", regioni$DEN_REG[regioni$COD_REG == cod]), x="", y="") +
	                          my_ggtheme()
	                      })
	names(map_regioni) <- regioni$COD_REG
}# SHAPE FILES
if (FALSE){
regioni <- st_read("www/Reg01012019/Reg01012019_WGS84.shp")
regioni <- st_transform(regioni, crs="+proj=longlat +datum=WGS84 +no_defs")
hlpr <- st_coordinates(st_centroid(regioni))
colnames(hlpr) <- c("reg_long", "reg_lat")
regioni <- cbind(regioni, hlpr)

#map_regioni <- lapply(regioni$COD_REG, function(cod) {
#                        box_strict <- st_as_sfc(st_bbox(regioni[regioni$COD_REG == cod,]), crs=st_crs(regioni))
#                       selezionati <- vapply(st_geometry(regioni), function(x) st_intersects(x, st_geometry(box_strict), sparse=F), TRUE)
#                      reg_to_plot <- st_boundary(regioni[selezionati,])
#                        reg_to_plot <- st_intersection(reg_to_plot, box_strict)
#                        ggplot() +
#                          geom_sf(data = reg_to_plot, color="lightgrey", size=.5) +
#                          geom_sf(data = regioni[regioni$COD_REG == cod,], color="black", size=.75) +
#                          labs(title=paste("Casi in", regioni$DEN_REG[regioni$COD_REG == cod]), x="", y="") +
#                          my_ggtheme()
#                      })
#names(map_regioni) <- regioni$COD_REG


province <- st_read("www/ProvCM01012019/ProvCM01012019_WGS84.shp")
province <- st_transform(province, crs="+proj=longlat +datum=WGS84 +no_defs")
#hlpr <- st_coordinates(st_centroid(province))
#colnames(hlpr) <- c("prv_long", "prv_lat")
#province <- cbind.data.frame(province, hlpr)
}
##################################################################
if(FALSE) {
	data_files <- list.files(dir_prov, full.names=T)
	data_files <- data_files[(grepl(".csv$", data_files) | grepl(".txt$", data_files)) & grepl('2020',data_files)]
	date_range <- as.Date(gsub(".*/.*-(\\d{,8}).csv$", "\\1", data_files), format="%Y%m%d")
	date0 <- min(date_range, na.rm=T)


	get_covid19_data <- function(flist) {
	  do.call(rbind, lapply(flist, function(ff) {
	    temp <- read.csv(ff, stringsAsFactors=F)
	    temp$data <- as.Date(temp$data)
			paTrentino <- grep('bolz|trent', temp$denominazione_regione, ignore.case=T)
			temp$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
			#temp$denominazione_regione[temp$denominazione_regione %in% c("Bolzano", "Trento")] <- "Trentino - Alto Adige"
	    temp <- merge(temp, pop_file[,c("codice_provincia", "pop")], by="codice_provincia")
	    temp
	  }))
	}

	allData <- get_covid19_data(data_files)
	regioniList <- sort(unique(allData$denominazione_regione))

	alld0 <- allData[allData$data==date0, ]
	pop_reg <- aggregate(alld0[, 'pop'], by=list(denominazione_regione=alld0$denominazione_regione), sum)
	setnames(pop_reg, old="x", new="pop")


	data_files_reg <- list.files(dir_reg, full.names=T)
	data_files_reg <- data_files_reg[(grepl(".csv$", data_files_reg) | grepl(".txt$", data_files_reg))  & grepl('2020',data_files_reg)]
	date_range_reg <- as.Date(gsub(".*/.*-(\\d{,8}).csv$", "\\1", data_files_reg), format="%Y%m%d")
	date0_reg <- min(date_range_reg, na.rm=T)

	get_covid19_data_reg <- function(flist) {
	  do.call(rbind, lapply(flist, function(ff) {
	    temp <- read.csv(ff, stringsAsFactors=F)
	    temp$data <- as.Date(temp$data)
	    temp <- merge(temp, pop_reg[,c("denominazione_regione", "pop")], by="denominazione_regione")
			paTrentino <- grep('bolz|trent', temp$denominazione_regione, ignore.case=T)
			temp$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
	    #temp$denominazione_regione[temp$denominazione_regione %in% c("Bolzano", "Trento")] <- "Trentino - Alto Adige"
	    temp
	  }))
	}

	allData_reg <- get_covid19_data_reg(data_files_reg)
}
