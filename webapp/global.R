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
