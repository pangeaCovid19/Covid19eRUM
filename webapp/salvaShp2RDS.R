library(sf)


dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"

provShapeRDS <- "ProvinceShapeF.RDS"
regShapeRDS <- "RegioniShapeF.RDS"

regioni <- st_read("www/Reg01012019/Reg01012019_WGS84.shp")
regioni <- st_transform(regioni, crs="+proj=longlat +datum=WGS84 +no_defs")
hlpr <- st_coordinates(st_centroid(regioni))
colnames(hlpr) <- c("reg_long", "reg_lat")
regioni <- cbind(regioni, hlpr)


province <- st_read("www/ProvCM01012019/ProvCM01012019_WGS84.shp")
province <- st_transform(province, crs="+proj=longlat +datum=WGS84 +no_defs")

saveRDS(file=paste0(dir_prov, provShapeRDS), province)
saveRDS(file=paste0(dir_reg, regShapeRDS), regioni)
