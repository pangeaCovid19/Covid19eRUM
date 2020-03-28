require(data.table)
require(rmarkdown)

source("funzionifit.R")
datiRegioni<-readRDS("www/dati-regioni/dataRegioni.RDS")
pvalue<-function(x,mean,sd) {
	y<-pnorm(x,mean,sd)
	2*min(y,1-y)
}
#contagi deve avere due colonne: data e variabile di interesse
confrontoModelloPrevisioni<-function(data, contagi, datastart=as.Date("2020-03-10")) {
	datelist<-seq(datastart,to=data-1,by="day")
	variabile<-setdiff(names(contagi),"data")
	modelliquad<-lapply(datelist, function(x) readRDS(sprintf("www/pastModels/modelliIta_%s.RDS",x)))
	modelliexp<-lapply(datelist, function(x) readRDS(sprintf("www/pastModels/modelliItaExp_%s.RDS",x)))
	contagi2<-contagi[[variabile]][match(datelist+1,contagi$data)]
	supporto<-contagi[[variabile]][match(datelist,contagi$data)]
	#contagi2<-contagi[data %in% datelist]
	#setkey(contagi2,data)
	funzione<-function(modexp, modquad, datoreale, currentdata, lastdatoreale, variabile) {
		modexp<-modexp[[variabile]]
		modquad<-modquad[[variabile]]
		prevexp<-predict(modexp,data.frame(data=currentdata+1),interval="confidence",level=1-pnorm(-1)*2)
		xxx<-setNames(data.frame(data=currentdata,altro=lastdatoreale),c("data",variabile))
		prevquad<-predictNextDays(xxx,modquad,nahead=1)
		sdexp<-prevexp[3]-prevexp[1]
		delta<-abs(log(datoreale)-prevexp[1])/sdexp
		pv<-pvalue(log(datoreale),prevexp[1],sdexp)
		sdquad<-log(prevquad$UpperRange/prevquad$Attesi)
		deltaquad<-abs(log(datoreale)-log(prevquad$Attesi))/sdquad
		pvquad<-pvalue(log(datoreale),log(prevquad$Attesi),sdquad)
		res<-data.frame(data = currentdata+1, variabile = format(datoreale), Esponenziale = round(exp(prevexp[1])), Delta_E = round(delta,1), `P-value_E` = round(pv,2) , Quadratico = format(prevquad$Attesi), Delta_q = round(deltaquad,1), `P-value_Q`= round(pvquad,2), stringsAsFactors=FALSE, check.names=FALSE)
		names(res)[2]<-variabile
		res
	}
	do.call(rbind,mapply(funzione, modelliexp, modelliquad, contagi2, datelist, supporto, MoreArgs=list(variabile=variabile),SIMPLIFY=FALSE))
}
#modello<-readRDS(sprintf("www/pastModels/modelliIta_%s.RDS"))
#modelloexp<-readRDS(sprintf("www/pastModels/modelliItaExp_%s.RDS"))
setindex(datiRegioni,NULL)
italia<-datiRegioni[,lapply(.SD,sum),by=.(data),.SDcols=7:15]
setkey(italia,data)
getLastDate<-function() max(italia$data)
getCoeffTS<-function(datastart=as.Date("2020-03-10"), datafinish = getLastDate()) {
	datelist<-seq(datastart,datafinish,by="day")
	modellist<-lapply(datelist, function(x) readRDS(sprintf("www/pastModels/modelliIta_%s.RDS",x)))
	#names(modellist)<-datelist
	res<-lapply(modellist,function(x) lapply(x,coef))
	nomi<-names(modellist[[1]])
	tmp<-matrix(unlist(res,recursive=FALSE),nrow=4)
	ritorno<-setNames(lapply(seq_len(nrow(tmp)),function(x) do.call(rbind,tmp[x,])),nomi)
	lapply(ritorno,function(x) cbind(Data=datelist,setNames(as.data.frame(x),c("B","K","M"))))
}

coeffts<-getCoeffTS()
italia[,decedutiGiorni:=c(deceduti[1],diff(deceduti))]
lastdaydata<-function(x=italia) x[data==getLastDate()]
segno<-function(x) c("una diminuzione","un incremento")[(x>0)+1]



data<-getLastDate()
fdata<-format(data,"%d %b %Y")
ldata<-data-1
modello<-readRDS(sprintf("www/pastModels/modelliIta_%s.RDS",ldata))
modelloexp<-readRDS(sprintf("www/pastModels/modelliItaExp_%s.RDS",ldata))


fldata<-format(ldata,"%d %b %Y")
ultimidati<-lastdaydata()
ultimidatiregioni<-lastdaydata(datiRegioni)
ultimidatiregioni<-ultimidatiregioni[order(totale_casi,decreasing=TRUE)]
treregioni<-ultimidatiregioni$denominazione_regione[order(ultimidatiregioni$totale_casi,decreasing=TRUE)][1:3]
treregioni2<-ultimidatiregioni[denominazione_regione %in% treregioni,.(denominazione_regione, totale_casi,deceduti)]
treregioniperc<-round(c(colSums(treregioni2[,2:3]))/ultimidati[,.(totale_casi,deceduti)]*100)
diffdeceduti<-diff(italia$decedutiGiorni)
diffcontagi<-diff(italia$nuovi_attualmente_positivi)
tabellaregioni<-ultimidatiregioni[,.(denominazione_regione,deceduti,totale_casi)]
setnames(tabellaregioni,old=c("denominazione_regione","deceduti","totale_casi"),new=c("Regione","Totale Deceduti","Totale Contagi"))
tabellaregioni[,`% Deceduti`:=round(`Totale Deceduti`/sum(`Totale Deceduti`),2)]
tabellaregioni[,`% Contagi`:=round(`Totale Contagi`/sum(`Totale Contagi`),2)]
coeffexptotalecasi<-coef(modelloexp[[1]])
trexptotalecasi<-round(1/coeffexptotalecasi[2]*log(2),2)
coeffexpdeceduti<-coef(modelloexp[[2]])
trexpdeceduti<-round(1/coeffexpdeceduti[2]*log(2),2)
previsioneesp<-predict(modelloexp[[1]],data.frame(data=data),interval="confidence",level=1-pnorm(-1)*2)
sdexp<-previsioneesp[3]-previsioneesp[1]
delta<-abs(log(ultimidati$totale_casi)-previsioneesp[1])/sdexp
pv<-pvalue(log(ultimidati$totale_casi),previsioneesp[1],sdexp)

previsionequad<-predictNextDays(data.frame(data=ldata,totale_casi=italia$totale_casi[italia$data==ldata]),modello[[1]],nahead=1)
sdquad<-log(previsionequad$UpperRange/previsionequad$Attesi)
deltaquad<-abs(log(ultimidati$totale_casi)-log(previsionequad$Attesi))/sdquad
pvquad<-pvalue(log(ultimidati$totale_casi),log(previsionequad$Attesi),sdquad)


tabuno<-confrontoModelloPrevisioni(data, italia[,.(data,totale_casi)])
tabdue<-confrontoModelloPrevisioni(data, italia[,.(data,deceduti)])
colnames(tabuno)[1:2]<-c("Data","Totale casi")
colnames(tabdue)[1:2]<-c("Data", "Deceduti")
