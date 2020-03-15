require(digest)
require(httr)
source("funzionifit.R")
writeLog<-function(message, log) {
	cat(sprintf("%s - %s\n",format(Sys.time()),message),file=log,append=TRUE)
}
readData<-function(file, popolazione) {
	tmp<-read.csv(file,stringsAsFactors=FALSE)
	tmp$data<-as.Date(tmp$data)
	paTrentino <- grep('bolz|trent', tmp$denominazione_regione, ignore.case=T)
	tmp$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
	if ("codice_provincia" %in% names(tmp)) {
		res<-merge(tmp, popolazione[,c("codice_provincia", "pop")], by="codice_provincia",all.x=TRUE)
		return(res)
	} else {
		res<-merge(tmp,popolazione,by="denominazione_regione",all.x=TRUE)
		return(res)
	}
}
campiPrevisioni <- c("totale_casi", "deceduti", "totale_ospedalizzati", "terapia_intensiva")

if (!dir.exists("logs")) dir.create("logs")
hashpath<-"logs/hashes.log"
logdemone<-"logs/demone.log"
if (file.exists("logs/hashes.log")) {
	hashes<-readRDS(hashpath)
} else {
	hashes<-c(Province = NA_character_, Regioni = NA_character_)
	saveRDS(hashes,hashpath)
}
provremotepath<-"https://github.com/pcm-dpc/COVID-19/raw/master/dati-province/dpc-covid19-ita-province.csv"
regremotepath<-"https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv"
provlocalpathcsv<-"www/pcm_data/dpc-covid19-ita-province.csv"
reglocalpathcsv<-"www/dati-regioni/dpc-covid19-ita-regioni.csv"

popolazione<-read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(popolazione) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")
popolazione$codice_provincia<-as.integer(popolazione$codice_provincia)
mapregprov<-readRDS("www/mapRegioneCodProv.RDS")
popreg<-merge(popolazione,mapregprov,by="codice_provincia")
popreg<-aggregate(popreg$pop, by=popreg["denominazione_regione"],FUN=sum)
names(popreg)<-c("denominazione_regione","pop")


i<-0
while (i==0) {
	i<-1
	writeLog("Inizio iterazione", logdemone)
	infofiles<-file.info(c(provlocalpathcsv,reglocalpathcsv))
	#Check dimensioni
	if (!is.na(infofiles$size[1])) {
		currentprov<-readBin(provlocalpathcsv,"raw",infofiles$size[1])
	} else {
		currentprov<-raw(0)
	}
	if (!is.na(infofiles$size[2])) {
		currentreg<-readBin(reglocalpathcsv,"raw",infofiles$size[1])
	} else {
		currentreg<-raw(0)
	}
	prov<-try(content(GET(provremotepath),type="raw"))
	if (!inherits(prov,"try-error")) {
		d1<- digest(prov)
		if (length(prov)>=length(currentprov) && (is.na(hashes["Province"]) || d1!=hashes["Province"])) {
			writeLog("Scarico Province\n",logdemone)
			writeBin(prov,provlocalpathcsv)
			hashes["Province"]<-d1
		}
	}
	reg<-try(content(GET(regremotepath),type="raw"))
	if (!inherits(reg,"try-error")) {
		d2<- digest(reg)
		if (length(reg)>=length(currentreg) && (is.na(hashes["Regioni"]) || d2!=hashes["Regioni"])) {
			writeLog("Scarico Regioni\n",logdemone)
			writeBin(reg,reglocalpathcsv)
			hashes["Regioni"]<-d2
		}
	}
	saveRDS(hashes,hashpath)
	uno<-readData(provlocalpathcsv,popolazione)
	due<-readData(reglocalpathcsv,popreg)

	tsReg <- getTimeSeries(due)
	modelliIta <- list()

	for(i in  1:length(campiPrevisioni)){
		modelliIta[[i]]<-loglinmodel3(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=T)
	}
	names(modelliIta) <- campiPrevisioni
	modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel3, quadratico=T)


	writeLog("Scrivendo i dati",logdemone)
	saveRDS(due,"www/dati-regioni/dataRegioni.RDS")
	saveRDS(uno,"www/pcm_data/dataProvince.RDS")
	writeLog("Scrivendo i modelli",logdemone)
	saveRDS(modelliReg,"www/modelliReg.RDS")
	saveRDS(modelliIta,"www/modelliIta.RDS")
}
