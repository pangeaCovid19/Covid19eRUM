library(sf)
library(ggplot2)

my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 7),
              axis.text = element_text(size=7),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}

dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"
dir_data		<- "www/"

provShapeRDS <- "ProvinceShapeF.RDS"
regShapeRDS <- "RegioniShapeF.RDS"

regioni <- st_read("www/Reg01012019/Reg01012019_WGS84.shp")
regioni <- st_transform(regioni, crs="+proj=longlat +datum=WGS84 +no_defs")
hlpr <- st_coordinates(st_centroid(regioni))
colnames(hlpr) <- c("reg_long", "reg_lat")
regioni <- cbind(regioni, hlpr)


province <- st_read("www/ProvCM01012019/ProvCM01012019_WGS84.shp")
province <- st_transform(province, crs="+proj=longlat +datum=WGS84 +no_defs")
hlpr <- st_coordinates(st_centroid(province))
colnames(hlpr) <- c("prv_long", "prv_lat")
province <- cbind(province, hlpr)


saveRDS(file=paste0(dir_prov, provShapeRDS), province)
saveRDS(file=paste0(dir_reg, regShapeRDS), regioni)


eu_countries <- read_sf("www/EU_countries.shp/CNTR_BN_10M_2016_4326.shp")
eu_info <- read.csv("www/EU_countries.shp/CNTR_RG_BN_10M_2016.csv")
eu_info <- eu_info[!is.na(eu_info$CNTR_CODE),]
eu_countries <- merge(eu_countries, eu_info, by.x="CNTR_BN_ID", by.y="CNTR_BN_CODE")

italy <- eu_countries[eu_countries$CNTR_CODE=="IT",]
ita_box <- st_as_sfc(st_bbox(italy), crs=st_crs(eu_countries))
selezionati <- vapply(st_geometry(eu_countries), function(x) st_intersects(x, st_geometry(ita_box), sparse=F), TRUE)
eu_to_plot <- eu_countries[selezionati,]
st_agr(eu_to_plot) = "constant"
eu_to_plot <- st_intersection(eu_to_plot, ita_box)
eu_to_plot <- st_transform(eu_to_plot, crs="+proj=longlat +datum=WGS84 +no_defs")


saveRDS(file=paste0(dir_data, "eu_to_plot.RDS"), eu_to_plot)
saveRDS(file=paste0(dir_data, "italy.RDS"), italy)


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


map_italia <- ggplot() +
  geom_sf(data = eu_to_plot, color="lightgrey", size=.5) +
  geom_sf(data = italy, color="black", size=.75) +
  labs(title="Casi in Italia", x="", y="") +
  my_ggtheme()


saveRDS(file=paste0(dir_data, "map_italia.RDS"), map_italia)
saveRDS(file=paste0(dir_data, "map_regioni.RDS"), map_regioni)



letti <- read.csv2("www/C_17_dataset_96_0_upFile.csv", stringsAsFactors=FALSE)
setDT(letti)
letti2018 <- letti[Anno=="2018", ]
letti2018[, Descrizione.Regione:= paste0(
	substr(toupper(Descrizione.Regione),1,1),
	substr(tolower(Descrizione.Regione),2,nchar(Descrizione.Regione))
	)]
paTrentino <- grep('bolz|trent', letti2018$Descrizione.Regione, ignore.case=T)
letti2018[paTrentino, Descrizione.Regione:="Trentino - Alto Adige"]

letti2018[, Descrizione.Regione := gsub("`", "'", Descrizione.Regione)]

letti2018[grepl("Emilia Romagna", Descrizione.Regione, ignore.case=T), Descrizione.Regione :='Emilia Romagna']
letti2018[grepl("Friuli Venezia Giulia", Descrizione.Regione, ignore.case=T), Descrizione.Regione :='Friuli Venezia Giulia']
letti2018[grepl("Valle d'Aosta", Descrizione.Regione, ignore.case=T), Descrizione.Regione :="Valle d'Aosta"]
setDF(letti2018)
saveRDS(file=paste0(dir_data, "letti2018.RDS"),letti2018 )
