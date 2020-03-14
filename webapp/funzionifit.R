require(data.table)
#Ritorna le time series per regione e globale
#dati: oggetto ritornato da get_covid19_data
getTimeSeries<-function(dati) {
	setDT(dati)
	tmp<-dati[,list(
		totale_casi=sum(totale_casi),
		ricoverati_con_sintomi=sum(ricoverati_con_sintomi),
		terapia_intensiva=sum(terapia_intensiva),
		totale_ospedalizzati=sum(totale_ospedalizzati),
		isolamento_domiciliare=sum(isolamento_domiciliare),
		totale_attualmente_positivi=sum(totale_attualmente_positivi),
		nuovi_attualmente_positivi=sum(nuovi_attualmente_positivi),
		dimessi_guariti=sum(dimessi_guariti),
		deceduti=sum(deceduti),
		tamponi=sum(tamponi)),
		by=.(denominazione_regione,data)]
	tmp[,casi_per_giorno:=c(0,diff(totale_casi)),by=denominazione_regione]

	totitalia<-tmp[,list(totale_casi=sum(totale_casi)),by=data]

	totitalia<-dati[,list(
		totale_casi=sum(totale_casi),
		ricoverati_con_sintomi=sum(ricoverati_con_sintomi),
		terapia_intensiva=sum(terapia_intensiva),
		totale_ospedalizzati=sum(totale_ospedalizzati),
		isolamento_domiciliare=sum(isolamento_domiciliare),
		totale_attualmente_positivi=sum(totale_attualmente_positivi),
		nuovi_attualmente_positivi=sum(nuovi_attualmente_positivi),
		dimessi_guariti=sum(dimessi_guariti),
		deceduti=sum(deceduti),
		tamponi=sum(tamponi)),
		by=.(data)]
	totitalia[,denominazione_regione:="Italia"]
	totitalia[,casi_per_giorno:=c(0,diff(totale_casi)),by=denominazione_regione]

	tmp<-rbind(tmp,totitalia)
	setkey(tmp,denominazione_regione,data)
	setDF(tmp)
	setDF(dati)
	split(tmp,tmp$denominazione_regione)
}
#fa un modello lineare con il logaritmo
#regione: elemento dell'oggetto ritornato da getTimeSeries
#fit pesato con i dati recenti che pesano maggiormente
loglinmodel<-function(regione, rangepesi=c(0,1)) {
	regione$logcasi <- log(regione$totale_casi)
	regione<-regione[is.finite(regione$logcasi),]
	pesi<-seq(rangepesi[1],rangepesi[2],length.out=nrow(regione))
	fit<-lm(logcasi~data,regione,weights=pesi)
	#plot(regione$data,regione$logcasi,col="red",lwd=2,ty="b")
	#abline(fit2,col="green")
	fit
}
#Ritorna le predizioni del modello
predictNextDays<-function(dati,modello,nahead=3, all=FALSE) {
	if(all==TRUE){
		newdata <- data.frame(data = c(dati$data, max(dati$data)+seq_len(nahead))  )
	} else newdata<-data.frame(data = max(dati$data)+seq_len(nahead))

	predizioni<-try(exp(predict(modello,newdata,interval="confidence")))
	if (inherits(predizioni,"try-error")) {
		warning("Can't make predictions")
		tmp<-rep(NA_real_,nahead)
		return(data.frame(Attesi=tmp,LowerRange=tmp,UpperRange=tmp))
	}
	colnames(predizioni)<-c("Attesi","LowerRange","UpperRange")
	newdata<-cbind(newdata,predizioni)
	return(newdata)
}

loglinmodel2<-function(dati, var="totale_casi", rangepesi=c(0,1)) {
	regione <- copy(dati)
	setnames(regione, old=var, new='var2fit')
	regione$logcasi <- log(regione$var2fit)
	regione<-regione[is.finite(regione$logcasi),]
	pesi<-seq(rangepesi[1],rangepesi[2],length.out=nrow(regione))
	fit<-lm(logcasi~data,regione,weights=pesi)
	setnames(regione, old='var2fit', new=var)
	#plot(regione$data,regione$logcasi,col="red",lwd=2,ty="b")
	#abline(fit2,col="green")
	fit
}

loglinmodel3<-function(dati, var="totale_casi", rangepesi=c(0,1), quadratico = FALSE) {
	regione <- copy(dati)
	setnames(regione, old=var, new='var2fit')
	regione$logcasi <- log(regione$var2fit)
	regione<-regione[is.finite(regione$logcasi),]
	pesi<-seq(rangepesi[1],rangepesi[2],length.out=nrow(regione))
	if (quadratico) {
		regione$dataind<-as.numeric(regione$data-min(regione$data))+1
		regione$data2<-as.numeric(regione$dataind)^2
		fit<-lm(logcasi~data+data2,regione,weights=pesi)
		attr(fit,"quadratico")<-TRUE
		attr(fit,"mindata")<-min(regione$data)
	} else {
		fit<-lm(logcasi~data,regione,weights=pesi)
	}
	setnames(regione, old='var2fit', new=var)
	#plot(regione$data,regione$logcasi,col="red",lwd=2,ty="b")
	#abline(fit2,col="green")
	fit
}


ismodelloQuadratico<-function(modello) {
	tmp<-attr(modello,"quadratico")
	!is.null(tmp) && tmp
}


predictNextDays<-function(dati,modello,nahead=3, all=FALSE) {
	if(all==TRUE){
		newdata <- data.frame(data = c(dati$data, max(dati$data)+seq_len(nahead))  )
	} else newdata<-data.frame(data = max(dati$data)+seq_len(nahead))
	if (ismodelloQuadratico(modello)) {
		newdata$dataind<-as.numeric(newdata$data-attr(modello,"mindata"))+1
		newdata$data2<-newdata$dataind^2
	}
	predizioni<-exp(predict(modello,newdata,interval="confidence"))
	colnames(predizioni)<-c("Attesi","LowerRange","UpperRange")
	newdata<-cbind(newdata,predizioni)
	return(newdata)
	newdata
}

