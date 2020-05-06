#pathfileD<-"worlddata/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
#pathfileCC<-"worlddata/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
source("gompertz.R")
source("funzionifit.R")
readWorldData<-function(pathfileCC,pathfileD) {
	datiCC<-read.csv(pathfileCC,stringsAsFactors=FALSE,check.names=FALSE)
	datiD<-read.csv(pathfileD,stringsAsFactors=FALSE,check.names=FALSE)
	datiCC2<-datiCC[datiCC[[1]]=="",]
	datiD2<-datiD[datiD[[1]]=="",]
	commonregions<-intersect(datiCC2[[2]],datiD2[[2]])
	datiCC2<-datiCC2[match(commonregions,datiCC2[[2]]),]
	datiD2<-datiD2[match(commonregions,datiD2[[2]]),]
	datilist<-vector("list",nrow(datiCC2))
	date<-as.Date(names(datiCC2)[-(1:4)],format="%m/%d/%y")
	names(datilist)<-commonregions
	for (i in seq_len(nrow(datiD2))) {
		tmp<-data.frame(data=date,deceduti=unlist(datiD2[i,-(1:4)]),totale_casi=unlist(datiCC2[i,-(1:4)]))
		tmp<-tmp[tmp$deceduti!=0 & tmp$totale_casi!=0,]
		rownames(tmp)<-NULL
		datilist[[i]]<-tmp
	}
	datilist
}


makeThreeModels<-function(statedata) {
	var<-c("deceduti","totale_casi")
	ritorno<-list(deceduti=list,totale_casi=NULL)
	ok<-TRUE
	for (i in var) {
		m1<-try(loglinmodel4(statedata,var=i,quadratico=FALSE))
		m2<-try(loglinmodel4(statedata,var=i,quadratico=TRUE))
		m3<-try(gompertzModel(statedata,var=i,P=1))
		ritorno[[i]]<-list(Exp=m1,Quad=m2,Gompertz=m3)
		cond<-!any(vapply(ritorno[[i]],function(x) inherits(x,"try-error"),TRUE))
		ok<-ok && cond
	}
	attr(ritorno,"allmodels")<-ok
	ritorno
}
produceDataAndModels<-function(pathfileCC,pathfileD) {
	worlddata<-readWorldData(pathfileCC,pathfileD)
	allmodels<-lapply(worlddata,makeThreeModels)
	qualiok<-vapply(allmodels,function(x) attr(x,"allmodels"),TRUE)
	allmodelsok<-allmodels[qualiok]
	worlddata<-worlddata[qualiok]
	list(data=worlddata, models=allmodelsok)
}

checkAndInitializeFolder<-function() {
	if (!dir.exists("worlddata")) {
		system("git clone https://github.com/CSSEGISandData/COVID-19.git worlddata")
	} else {
		oldwd<-getwd()
		setwd("worlddata")
		on.exit(setwd(oldwd))
		system("git pull")
	}
	invisible(TRUE)
}
