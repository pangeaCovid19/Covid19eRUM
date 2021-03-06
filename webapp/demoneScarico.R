require(digest)
require(httr)
require(rmarkdown)
source("funzionifit.R")
source("gompertz.R")
source("readTS.R")
writeLog<-function(message, log) {
	cat(sprintf("%s - %s\n",format(Sys.time()),message),file=log,append=TRUE)
}
readData<-function(file, popolazione) {
	tmpp<-readLines(file)
	righeheader<-grep("totale_casi",tmpp)
	if (length(righeheader)>1) {
		tmpp<-tmpp[-righeheader[-1]]
		writeLines(tmpp,file)
		rm(tmpp)
		rm(righeheader)
	}
	tmp<-read.csv(file,stringsAsFactors=FALSE)
	tmp$data<-as.Date(tmp$data)
	tmp[grep("note",names(tmp))]<-NULL
	paTrentino <- grep('bolz|trent', tmp$denominazione_regione, ignore.case=T)
	tmp$denominazione_regione[paTrentino] <- "Trentino - Alto Adige"
	tmp$codice_regione[paTrentino] <- 4
	paEmilia <- grep('Emilia', tmp$denominazione_regione, ignore.case=T)
	tmp$denominazione_regione[paEmilia] <- "Emilia Romagna"
	if ("codice_provincia" %in% names(tmp)) {
		#province
		res<-merge(tmp, popolazione[,c("codice_provincia", "pop")], by="codice_provincia",all.x=TRUE)
		return(res)
	} else {
		#regioni
		if("totale_positivi" %in% names(tmp))setnames(tmp, old=c("totale_positivi"), new=c("totale_attualmente_positivi") )
		if("variazione_totale_positivi" %in% names(tmp))setnames(tmp, old=c("variazione_totale_positivi"), new=c("nuovi_attualmente_positivi" ) )

		indTrentino <- which(tmp$denominazione_regione=="Trentino - Alto Adige")
		tmp$lat[indTrentino] 	<- mean(tmp$lat[indTrentino])
		tmp$long[indTrentino] 	<- mean(tmp$long[indTrentino])

		toAggBy <- c("data", "stato" , "codice_regione", "denominazione_regione", "lat", "long")
		toSum		<- names(tmp)[which(!(names(tmp) %in% toAggBy))]
		tmp2 <- aggregate(tmp[, toSum], by=tmp[, toAggBy], sum)
		res<-merge(tmp2,popolazione,by="denominazione_regione",all.x=TRUE)
		#correzioni aprile
		aprLomNew <- 282
		aprLomInd <- which(res$denominazione_regione=="Lombardia" & month(res$data)==4)
		aprLomTot <- sum(res$deceduti[aprLomInd])
		res$deceduti[aprLomInd] <- res$deceduti[aprLomInd] + round(seq(from=1,to=aprLomNew, length.out= length(aprLomInd)))
		res$deceduti[res$denominazione_regione=="Lombardia" & res$data=="2020-05-01"] <- aprLomNew + res$deceduti[res$denominazione_regione=="Lombardia" & res$data=="2020-05-01"]

		#res$deceduti[aprLomInd] <- round(res$deceduti[aprLomInd]*(1+aprLomNew/aprLomTot))
		#res$deceduti[res$denominazione_regione=="Lombardia" & res$data=="2020-05-02"] <- res$deceduti[res$denominazione_regione=="Lombardia" & res$data=="2020-05-02"]
		aprLazNew <- 33
		aprLazInd <- which(res$denominazione_regione=="Lazio" & month(res$data)==4)
		aprLazTot <- sum(res$deceduti[aprLazInd])
		res$deceduti[aprLazInd] <- res$deceduti[aprLazInd]+round(seq(from=1,to=aprLazNew, length.out= length(aprLazInd)))
		#res$deceduti[aprLazInd] <- round(res$deceduti[aprLazInd]*(1+aprLazNew/aprLazTot))
		#res$deceduti[res$denominazione_regione=="Lazio" & res$data=="2020-05-01"] <- res$deceduti[res$denominazione_regione=="Lazio" & res$data=="2020-05-01"]

		return(res)
	}
}



if(dir.exists("logs/")) dir.create("logs/")
logdemone<-"logs/demone.log"
writeLog("Aggiorno dati mondo\n",logdemone)
checkAndInitializeFolder()
pathfileD<-"worlddata/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
pathfileCC<-"worlddata/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
worldobject<-produceDataAndModels(pathfileCC,pathfileD)
saveRDS(worldobject,"DataAndModelsWorld.RDS")
rm(pathfileCC)
rm(pathfileD)
rm(worldobject)
writeLog("Finito\n",logdemone)


campiPrevisioni <- c("totale_casi", "deceduti", "totale_ospedalizzati", "terapia_intensiva")

if (!dir.exists("logs")) dir.create("logs")
hashpath<-"logs/hashes.log"
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
		if ((is.na(hashes["Province"]) || d1!=hashes["Province"])) {
			writeLog("Scarico Province\n",logdemone)
			writeBin(prov,provlocalpathcsv)
			hashes["Province"]<-d1
		}
	}
	reg<-try(content(GET(regremotepath),type="raw"))
	if (!inherits(reg,"try-error")) {
		d2<- digest(reg)
		if ((is.na(hashes["Regioni"]) || d2!=hashes["Regioni"])) {
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
	modelliItaExp <- list()
	modelliItaGomp <- list()
	for(i in  1:length(campiPrevisioni)){
		modelliIta[[i]]<-loglinmodel4(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=TRUE)
		modelliItaExp[[i]]<-loglinmodel4(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=FALSE)
		modelliItaGomp[[i]] <- try(gompertzModel(tsReg$Italia, var=campiPrevisioni[i], lastw=5, P=102.6891))
		if (inherits(modelliItaGomp[[i]],"try-error")) {
			modelliItaGomp[[i]]<-modelliIta[[i]]
		}

	}
	names(modelliIta) <- campiPrevisioni
	names(modelliItaExp) <- campiPrevisioni
	names(modelliItaGomp) <- campiPrevisioni

	modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel4, quadratico=TRUE)
	modelliRegExp <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel4, quadratico=FALSE)
	modelliRegGomp <-lapply( tsReg[which(names(tsReg)!='Italia')], function(x){
			tmp<-try(gompertzModel(x, var="totale_casi", lastw=5, P=102.6891))
			if (inherits(tmp,"try-error")) return(loglinmodel4(x,quadratico=TRUE))
			tmp
		}
	)
	modelliProvince<-makeProvinceModels(uno)
	writeLog("Scrivendo i dati",logdemone)
	saveRDS(due,"www/dati-regioni/dataRegioni.RDS")
	saveRDS(uno,"www/pcm_data/dataProvince.RDS")
	saveRDS(modelliProvince,"www/pcm_data/ModelliProvince.RDS")
	writeLog("Scrivendo i modelli",logdemone)

	saveRDS(modelliReg,"www/modelliReg.RDS")
	saveRDS(modelliIta,"www/modelliIta.RDS")
	saveRDS(modelliRegExp,"www/modelliRegExp.RDS")
	saveRDS(modelliItaExp,"www/modelliItaExp.RDS")
	saveRDS(modelliRegGomp,"www/modelliRegGomp.RDS")
	saveRDS(modelliItaGomp,"www/modelliItaGomp.RDS")

	# salvo lo storico dei modelli
	if(!dir.exists("www/pastModels/")) dir.create("www/pastModels/")
	dataMax <- max(due$data, na.rm=T)
	saveRDS(modelliReg,paste0("www/pastModels/modelliReg_", dataMax,".RDS"))
	saveRDS(modelliIta,paste0("www/pastModels/modelliIta_", dataMax,".RDS"))
	saveRDS(modelliRegExp,paste0("www/pastModels/modelliRegExp_", dataMax,".RDS"))
	saveRDS(modelliItaExp,paste0("www/pastModels/modelliItaExp_", dataMax,".RDS"))
	saveRDS(modelliRegGomp,paste0("www/pastModels/modelliRegGomp_", dataMax,".RDS"))
	saveRDS(modelliItaGomp,paste0("www/pastModels/modelliItaGomp_", dataMax,".RDS"))

}

if ( TRUE) {
	cat("\n ricalcolo modelli del passato:")
	date <- seq(as.Date('2020-03-08'), dataMax, by=1)
	modelswitch<-as.Date("2020-03-28")
	dataGomp <- as.Date("2020-03-08")

	longIta <- lapply(date, function(x){
		cat("->", strftime(x))
		modelliIta <- list()
		modelliItaExp <- list()
		modelliItaGomp <- list()
		delta<-x>=modelswitch
		for(i in  1:length(campiPrevisioni)){
			modelliIta[[i]]<-loglinmodel4(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=TRUE, dataMax=x,delta=delta)
			modelliItaExp[[i]]<-loglinmodel4(tsReg$Italia, var=campiPrevisioni[i], rangepesi=c(0,1), quadratico=FALSE, dataMax=x)
			if(x >= dataGomp){
				modelliItaGomp[[i]] <- try(gompertzModel(tsReg$Italia, var=campiPrevisioni[i], lastw=5, P=102.6891, dataMax=x))
				if (inherits(modelliItaGomp[[i]],"try-error")) {
					modelliItaGomp[[i]]<-modelliIta[[i]]
				}
			}
		}
		names(modelliIta) <- campiPrevisioni
		names(modelliItaExp) <- campiPrevisioni
		if(x >= dataGomp) names(modelliItaGomp) <- campiPrevisioni

		modelliReg <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel4, quadratico=TRUE, dataMax=x, delta=delta)
		modelliRegExp <-lapply( tsReg[which(names(tsReg)!='Italia')], loglinmodel4, quadratico=FALSE, dataMax=x)
		if(x >= dataGomp){
			modelliRegGomp <-lapply( tsReg[which(names(tsReg)!='Italia')], function(x){
					tmp<-try(gompertzModel(x, var="totale_casi", lastw=5, P=102.6891))
					if (inherits(tmp,"try-error")) return(loglinmodel4(x,quadratico=TRUE))
					tmp
				}
			)
		}
		saveRDS(modelliReg,paste0("www/pastModels/modelliReg_", x,".RDS"))
		saveRDS(modelliIta,paste0("www/pastModels/modelliIta_", x,".RDS"))
		saveRDS(modelliRegExp,paste0("www/pastModels/modelliRegExp_", x,".RDS"))
		saveRDS(modelliItaExp,paste0("www/pastModels/modelliItaExp_", x,".RDS"))
		if(x >= dataGomp){
			saveRDS(modelliRegGomp,paste0("www/pastModels/modelliRegGomp_", x,".RDS"))
			saveRDS(modelliItaGomp,paste0("www/pastModels/modelliItaGomp_", x,".RDS"))
		}
	})
}

if(FALSE) {
	rmarkdown::render("articolo.Rmd",output_file="www/tabReport.html")
	if(!dir.exists("www/pastDiary/")) dir.create("www/pastDiary/")
	rmarkdown::render("articolo.Rmd",output_file=paste0("www/pastDiary/tabReport_", dataMax,".html"))
	rmarkdown::render("art21mar2020.Rmd",output_file=paste0("www/Report21marzo2020.html"))
	rmarkdown::render("art04mag2020.Rmd",output_file=paste0("www/Report04maggio2020.html"))
}
