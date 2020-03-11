require(data.table)
#Ritorna le time series per regione e globale
#dati: oggetto ritornato da get_covid19_data
getTimeSeries<-function(dati) {
	setDT(dati)
	on.exit(setDF(dati))
	tmp<-dati[,list(totale_casi=sum(totale_casi)),by=.(denominazione_regione,data)]
	tmp[,casi_per_giorno:=c(0,diff(totale_casi)),by=denominazione_regione]
	totitalia<-tmp[,list(totale_casi=sum(totale_casi)),by=data]
	totitalia[,denominazione_regione:="Italia"]
	totitalia[,casi_per_giorno:=c(0,diff(totale_casi)),by=denominazione_regione]
	tmp<-rbind(tmp,totitalia)
	setkey(tmp,denominazione_regione,data)
	setDF(tmp)
	split(tmp,tmp$denominazione_regione)
}
#fa un modello lineare con il logaritmo
#regione: elemento dell'oggetto ritornato da getTimeSeries
#fit pesato con i dati recenti che pesano maggiormente
loglinmodel<-function(regione, rangepesi=c(0,1)) {
	regione<-regione[is.finite(regione$logcasi),]
	pesi<-seq(rangepesi[1],rangepesi[2],length.out=nrow(regione))
	fit<-lm(logcasi~data,regione,weights=pesi)
	#plot(regione$data,regione$logcasi,col="red",lwd=2,ty="b")
	#abline(fit2,col="green")
	fit
}
#Ritorna le predizioni del modello
predictNextDays<-function(dati,modello,nahead=3) {
	newdata<-data.frame(data = max(dati$data)+seq_len(nahead))
	predizioni<-exp(predict(modello,newdata,interval="confidence"))
	colnames(predizioni)<-c("Attesi","LowerRange","UpperRange")
	newdata<-cbind(newdata,predizioni)
	return(newdata)
	newdata
}
