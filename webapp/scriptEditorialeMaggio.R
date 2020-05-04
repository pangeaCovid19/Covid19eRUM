require(data.table)
require(rmarkdown)

source("funzionifit.R")
source("gompertz.R")
dataStart<-as.Date("2020-05-03")
listadate<-seq(dataStart-10,dataStart, by="1 days")
datiRegioni<-readRDS("www/dati-regioni/dataRegioni.RDS")
datiRegioni<-datiRegioni[data<=dataStart]
setindex(datiRegioni,NULL)
italia<-datiRegioni[,lapply(.SD,sum),by=.(data),.SDcols=7:17]
setkey(italia,data)
itlist<-lapply(listadate, function(x) italia[data<=x,])
modelliGompertzDece<-lapply(listadate, function(x) gompertzModel(italia,"deceduti",dataMax=x))
previsioniDece<-rbindlist(Map(predictNextDaysGompertz,itlist,modelliGompertzDece,nahead=1))
previdelta<-previsioniDece$Attesi-tail(italia$deceduti,11)
veridecedelta<-tail(diff(italia$deceduti),10)

modelliGompertzCasi<-lapply(listadate, function(x) gompertzModel(italia,dataMax=x))
previsioniCasi<-rbindlist(Map(predictNextDaysGompertz,itlist,modelliGompertzCasi,nahead=1))
prevideltacasi<-previsioniCasi$Attesi-tail(italia$totale_casi,11)
vericasidelta<-tail(diff(italia$totale_casi),10)

tabella<-data.frame(Data=previsioniDece$data, `Deceduti Previsti`=previdelta, `Deceduti` = c(veridecedelta,NA), `Nuovi Casi Previsti` = prevideltacasi, `Nuovi Casi` = c(vericasidelta,NA),check.names=FALSE)

gomfunction<-function(x,abc) abc[1] * exp(-abc[2] * exp(-abc[3] * x))
fit19<-gompertz(1:19,italia$deceduti[1:19])
fit30<-gompertz(1:30,italia$deceduti[1:30])
fittutto<-gompertz(1:nrow(italia),italia$deceduti)
abc<-coefficients(fit19)
abc2<-coefficients(fit30)
abc3<-coefficients(fittutto)
curva1<-gomfunction(1:100,abc)
#abc[1]<-abc[1]/1.5
curva2<-gomfunction(1:100,abc2)
#abc[1]<-abc[1]*1.5*1.5
curva3<-gomfunction(1:100,abc3)

#plot(curva2,ty="l",lwd=2,col="red", xlab="Giorni dall'inizio epidemia", ylab="Deceduti",cex.lab=1.4)
#lines(curva1,lwd=2,col="green")
#lines(curva3,lwd=2,col="yellow")
#points(italia$deceduti[1:29])
