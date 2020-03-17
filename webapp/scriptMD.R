require(data.table)
require(rmarkdown)
datiRegioni<-readRDS("www/dati-regioni/dataRegioni.RDS")
pvalue<-function(x,mean,sd) {
	y<-pnorm(x,mean,sd)
	2*min(y,1-y)
}
#modello<-readRDS(sprintf("www/pastModels/modelliIta_%s.RDS"))
#modelloexp<-readRDS(sprintf("www/pastModels/modelliItaExp_%s.RDS"))


setindex(datiRegioni,NULL)
italia<-datiRegioni[,lapply(.SD,sum),by=.(data),.SDcols=7:15]
setkey(italia,data)
italia[,decedutiGiorni:=c(deceduti[1],diff(deceduti))]
getLastDate<-function() max(italia$data)
lastdaydata<-function(x=italia) x[data==getLastDate()]
segno<-function(x) c("una diminuzione","un incremento")[(x>0)+1]



data<-getLastDate()
fdata<-format(data,"%d %B %Y")
ldata<-data-1
modello<-readRDS(sprintf("www/pastModels/modelliIta_%s.RDS",ldata))
modelloexp<-readRDS(sprintf("www/pastModels/modelliItaExp_%s.RDS",ldata))


fldata<-format(ldata,"%d %B %Y")
ultimidati<-lastdaydata()
ultimidatiregioni<-lastdaydata(datiRegioni)
ultimidatiregioni<-ultimidatiregioni[order(totale_casi,decreasing=TRUE)]
treregioni<-ultimidatiregioni$denominazione_regione[order(ultimidatiregioni$totale_casi,decreasing=TRUE)][1:3]
treregioni2<-ultimidatiregioni[denominazione_regione %in% treregioni,.(denominazione_regione, totale_casi,deceduti)]
treregioniperc<-round(c(colSums(treregioni2[,2:3]))/ultimidati[,.(totale_casi,deceduti)]*100)
diffdeceduti<-diff(italia$decedutiGiorni)
diffcontagi<-diff(italia$nuovi_attualmente_positivi)
tabellaregioni<-ultimidatiregioni[,.(denominazione_regione,deceduti,totale_casi)]
setnames(tabellaregioni,old=c("denominazione_regione","deceduti","totale_casi"),new=c("Regione","Deceduti","Totale Contagi"))
tabellaregioni[,`Perc. Deceduti`:=round(Deceduti/sum(Deceduti),2)]
tabellaregioni[,`Perc. Contagi`:=round(`Totale Contagi`/sum(`Totale Contagi`),2)]
coeffexptotalecasi<-coef(modelloexp[[1]])
trexptotalecasi<-round(1/coeffexptotalecasi[2]*log(2),2)
coeffexpdeceduti<-coef(modelloexp[[2]])
trexpdeceduti<-round(1/coeffexpdeceduti[2]*log(2),2)
previsioneesp<-predict(modelloexp[[1]],data.frame(data=data),interval="confidence")
sdexp<-previsioneesp[3]-previsioneesp[1]
delta<-abs(log(ultimidati$totale_casi)-previsioneesp[1])/sdexp
pv<-pvalue(log(ultimidati$totale_casi),previsioneesp[1],sdexp)
