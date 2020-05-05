#generateDataPower<-function(datelist, maxGrade=6, step = 0.1, minimo=min(datelist)) {
#	#minimo<-min(datelist)
#	d1<-as.numeric(difftime(datelist,minimo,units="days"))*step+step
#	res<-outer(d1,seq_len(maxGrade),"^")
#	nomi<-sprintf("data%d",seq_len(maxGrade))
#	colnames(res)<-nomi
#	structure(res, mindata = minimo, step = step)
#}
#generalLogModel<-function(dati, var="totale_casi", rangepesi=c(0,1), delta=TRUE, dataMax=NULL, maxGrade = 6) {
#	if(!is.null(dataMax)){
#		if(class(dataMax) == "Date") {
#			dati <- dati[(dati$data <=dataMax), ]
#		} else warning(paste0("dataMax non è di class 'Date': class(dataMax) è ", class(dataMax)))
#	}
#	regione <- copy(dati)
#	setnames(regione, old=var, new='var2fit')
#	if (delta) {
#		regione<-addColumn(regione,c(regione$var2fit[1],diff(regione$var2fit)),"var2fit")
#		regione$var2fit[regione$var2fit<0]<-0
#	}
#	lambdas <- 10^seq(3, -5, by = -.2)
#	predictors<-generateDataPower(regione$data,maxGrade=maxGrade)
#	pesi<-seq(rangepesi[1],rangepesi[2],length.out=nrow(regione))
#	regione$logcasi <- log(regione$var2fit)
#	#model<-glmnet(predictors,regione$logcasi,weights=pesi,alpha=0,lambda=lambdas)
#	cv_fit <- cv.glmnet(predictors,regione$logcasi, alpha = 0, lambda = lambdas)
#	model <- cv_fit$glmnet.fit
#	s<-cv_fit$lambda.min
#	
#}



##linear model per i deceduti dai nuovi contagi
#modelcontagi<-function(nuovicontagi, deltadeceduti, lag.max=7) {
#	sequenza<-rev(seq_len(lag.max))
#	predittori<-do.call(cbind,shift(nuovicontagi,sequenza))
#	colnames(predittori)<-paste0("NC",sequenza)
#	res<-cbind(predittori,deceduti=deltadeceduti)
#	res<-as.data.frame(res)
#	modello<-lm(deceduti~.,res)
#}



gompertz<-function(x,y,weights=seq(0,1,length.out=length(x))^2) {
	dati<-data.frame(x=x,y=y)
	punti<-round(length(x)*1:2/3)
	a<-max(y)*10
	ly1<-log(y[punti[1]]/a)
	ly2<-log(y[punti[2]]/a)
	c<- -log(ly2/ly1)/(x[punti[2]]-x[punti[1]])
	b<--ly1/exp(-c*x[punti[1]])
	starts<-c(a=a,b=b,c=c)
	risulto<-nls(y ~ a * exp(-b * exp(-c * x)),data=dati,start=as.list(starts),control=list(warnOnly=TRUE,printEval=TRUE),weights=weights)
	#risultodelta<-nls(y ~ a*b*c* exp(b * (-exp(-c * x)-c*x)),data=datidelta,start=as.list(starts),control=list(warnOnly=TRUE,printEval=TRUE),weights=weights)
	#f<-function(x, a, b, c) a * exp(-b * exp(-c * x))
	risulto
	#formula<-" a * exp(-b * exp(-c * x))"
}

R2<-function (m) {
    gl <- length(fitted(m)) - 1
    sqt <- var((fitted(m) + resid(m))) * gl
    r1 <- (sqt - deviance(m))/sqt
    return(r1)
}

lastdaygompertz<-function(x,y,last=10,weights=NULL, weighttail=TRUE) {
	fw<-if (weighttail) tail else head
	datogliere<-rev(lapply(seq_len(last),function(z) length(x)-seq_len(z)+1))
	modelli<-lapply(datogliere, function(a) gompertz(x[-a],y[-a],weights=fw(weights,length(x)-length(a))))
	asintoto<-vapply(modelli,function(a) coef(a)[1],1)
	predizioni<-mapply(function(a,b) predict(a,data.frame(x=min(b))),modelli, datogliere)
	list(asintoto=asintoto,predizioni=predizioni)
}

varasintoto<-function(P,x,y,lastw=3) {
	N<-length(x)
	pesi<-c(rep(1,N-lastw),rep(P,lastw))
	pippo<-lastdaygompertz(x,y,weights=pesi)
	sd(pippo$asintoto)
}


gompertzModel<-function(dati, var="totale_casi", lastw=5, P=102.6891,dataMax=NULL) {
	if(!is.null(dataMax)){
		if(class(dataMax) == "Date") {
			dati <- dati[(dati$data <=dataMax), ]
		} else warning(paste0("dataMax non è di class 'Date': class(dataMax) è ", class(dataMax)))
	}
	x<-as.numeric(dati$data-min(dati$data))+1
	y<-dati[[var]]
	pesi<-c(rep(1,length(x)-lastw),rep(P,lastw))
	modello<-gompertz(x,y,weights=pesi)
	attr(modello,"gompertz")<-TRUE
	attr(modello,"var2fit")<-var
	attr(modello,"mindata")<-min(dati$data)
	attr(modello,"offset")<-tail(y,1)-tail(fitted(modello),1)
	modello
}



predictNextDaysGompertz<-function(dati,modello,nahead=3, all=FALSE, varfactor=4) {
	if(all){
		newdata <- data.frame(x = as.numeric(c(dati$data, max(dati$data)+seq_len(nahead))-attr(modello,"mindata")+1))
	} else {
		newdata <- data.frame(x=as.numeric(max(dati$data)+seq_len(nahead)-attr(modello,"mindata")+1))
	}
	predizioni<-round(predict(modello,newdata)+attr(modello,"offset"))
	if (!all) {
		deltas<-cumsum(diff(c(tail(fitted(modello),1),predizioni)))
		sigma<-sqrt(deltas)*varfactor
	} else {
		deltas<-c(predizioni[1],diff(predizioni))
		sigma<-suppressWarnings(sqrt(deltas))
		sigma[!is.finite(sigma)]<-0
		verepredizioni<-predizioni[-seq_len(nrow(dati))]
		deltas<-cumsum(diff(c(tail(fitted(modello),1),verepredizioni)))
		sigmavereprevisioni<-sqrt(deltas)*varfactor
		sigma[-seq_len(nrow(dati))]<-sigmavereprevisioni
	}
	predizioni2<-cbind(Attesi = predizioni, LowerRange = predizioni-sigma, UpperRange = predizioni+sigma)
	newdata$data<-newdata$x+attr(modello,"mindata")-1
	newdata$x<-NULL
	cbind(newdata,predizioni2)
}
