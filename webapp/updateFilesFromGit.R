library(httr)
regRDS <- "dataRegioni.RDS"
provRDS <- "dataProvince.RDS"

git_path 	<- "/repos/pcm-dpc/COVID-19/contents/"
git_dir_prov <-'dati-province/'
git_dir_reg  <-'dati-regioni/'
dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"

pop_file <- read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(pop_file) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")

## LIST FILE PRESENTI
curFileList <- list.files(dir_prov, full.names=F)
curFileList_reg <- list.files(dir_reg, full.names=F)

# per ora lavoriamo sui file totali
ftot <- "dpc-covid19-ita-province.csv"
download.file(paste0("https://github.com/pcm-dpc/COVID-19/raw/master/dati-province/", ftot), paste0(dir_prov, ftot))
ftot_reg <- "dpc-covid19-ita-regioni.csv"
download.file(paste0("https://github.com/pcm-dpc/COVID-19/raw/master/", git_dir_reg, ftot_reg), paste0(dir_reg, ftot_reg))

# Lettura dati
pathRDSprov <- paste0(dir_prov, provRDS)
path <- paste0(dir_prov, ftot)
if (!file.exists(pathRDSprov) ) {
	dataprov <- read.csv(path, stringsAsFactors=FALSE)
	dataprov$data <- as.Date(dataprov$data)
	paTrentino <- grep('bolz|trent', dataprov$denominazione_regione, ignore.case=T)
	dataprov$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
  allData <- merge(dataprov, pop_file[,c("codice_provincia", "pop")], by="codice_provincia")
	saveRDS(allData, file=pathRDSprov)
}else if( file.info(path)$mtime > file.info(pathRDSprov)$mtime  ) {
	dataprov <- read.csv(path, stringsAsFactors=FALSE)
	dataprov$data <- as.Date(dataprov$data)
	paTrentino <- grep('bolz|trent', dataprov$denominazione_regione, ignore.case=T)
	dataprov$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
  allData <- merge(dataprov, pop_file[,c("codice_provincia", "pop")], by="codice_provincia")
	saveRDS(allData, file=pathRDSprov)
} else {
	allData <- readRDS(pathRDSprov)
}


alld0 <- allData[allData$data==min(allData$data,na.rm=T), ]
pop_reg <- aggregate(alld0[, 'pop'], by=list(denominazione_regione=alld0$denominazione_regione), sum)
names(pop_reg) <- c('denominazione_regione', 'pop')

pathRDSreg <-paste0(dir_reg, regRDS)
pathreg <- paste0(dir_reg, ftot_reg)
if (!file.exists(pathRDSreg) ) {
	datareg <- read.csv(pathreg, stringsAsFactors=FALSE)
	datareg$data <- as.Date(datareg$data)
	paTrentino <- grep('bolz|trent', datareg$denominazione_regione, ignore.case=T)
	datareg$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
  allData_reg <- merge(datareg, pop_reg[,c("denominazione_regione", "pop")], by="denominazione_regione")
	saveRDS(allData_reg, file=pathRDSreg)
} else if( file.info(pathreg)$mtime > file.info(pathRDSreg)$mtime  ) {
	datareg <- read.csv(pathreg, stringsAsFactors=FALSE)
	datareg$data <- as.Date(datareg$data)
	paTrentino <- grep('bolz|trent', datareg$denominazione_regione, ignore.case=T)
	datareg$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
  allData_reg <- merge(datareg, pop_reg[,c("denominazione_regione", "pop")], by="denominazione_regione")
	saveRDS(allData_reg, file=pathRDSreg)
}






if (FALSE) {
	## CHECK FILE SU GITHUB
	#province
	my_url <- modify_url("https://api.github.com", path = paste0(git_path,git_dir_prov))
	my_resp <- GET(my_url)
	my_json <- jsonlite::fromJSON(content(my_resp, "text"), simplifyVector = FALSE)
	git_files <- vapply(my_json, function(x) x$path, "a")
	git_files <- gsub(git_dir_prov,"", git_files)
	git_files <- git_files[grepl(".*-\\d{,8}.csv$",git_files)]

	## DOWNLOAD NEW FILES
	new_files <- setdiff(git_files, curFileList)
	if (length(new_files)) {
		for (ff in new_files)
			download.file(paste0("https://github.com/pcm-dpc/COVID-19/raw/master/dati-province/", ff), paste0(dir_prov, ff))
	}

	#regioni
	my_url <- modify_url("https://api.github.com", path = paste0(git_path,git_dir_reg))
	my_resp <- GET(my_url)
	my_json <- jsonlite::fromJSON(content(my_resp, "text"), simplifyVector = FALSE)
	git_files_reg <- vapply(my_json, function(x) x$path, "a")
	git_files_reg <- gsub(git_dir_reg,"", git_files_reg)
	git_files_reg <- git_files_reg[grepl(".*-\\d{,8}.csv$",git_files_reg)]

	## DOWNLOAD NEW FILES
	new_files_reg <- setdiff(git_files_reg, curFileList_reg)
	if (length(new_files_reg)) {
		for (ff in new_files_reg)
			download.file(paste0("https://github.com/pcm-dpc/COVID-19/raw/master/", git_dir_reg, ff), paste0(dir_reg, ff))
	}
}



#DEPRECATA
# Lettura dati
#if (!file.exists(paste0(dir_prov, provRDS)) ) {
#	data_files <- list.files(dir_prov, full.names=T)
#	data_files <- data_files[(grepl(".csv$", data_files) | grepl(".txt$", data_files)) & grepl('2020',data_files)]
#	date_range <- as.Date(gsub(".*/.*-(\\d{,8}).csv$", "\\1", data_files), format="%Y%m%d")
#	date0 <- min(date_range, na.rm=T)
#	get_covid19_data <- function(flist, popolazione) {
#	  do.call(rbind, lapply(flist, function(ff, pop) {
#	    temp <- read.csv(ff, stringsAsFactors=F)
#	    temp$data <- as.Date(temp$data)
#			paTrentino <- grep('bolz|trent', temp$denominazione_regione, ignore.case=T)
#			temp$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
#	    temp <- merge(temp, pop[,c("codice_provincia", "pop")], by="codice_provincia")
#	    temp
#	  }, pop=popolazione))
#	}
#	allData <- get_covid19_data(data_files, popolazione=pop_file)
#	saveRDS(allData, file=paste0(dir_prov, provRDS))
#}
