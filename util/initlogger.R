
require(log4r)
#Layout standard per il log, con formato TIMESTAMP - messaggio [livello] 

standardLayout<-function(level, ...) {
	msg<-paste0(...,collapse=" ")
	sprintf("%s - %s [%s]
",format(Sys.time(),"%Y-%m-%d %H:%M:%OS"),msg,level)
}
#Inizializza i logs da un file di configurazione
#conf: sia una stringa contenente il nome del file di configurazione, sia oggetto configurazione stesso (ottenuto tramite read.ini del pacchetto ini)
#Ritorna una lista di oggetti logger che hanno come nome quello della configurazione (tolto il prefisso log_) e puntano sul file indicato
#nella configurazione.
initLogs<-function(conf) {
	if (is.character(conf)) conf<-read.ini(conf)
	conf2<-conf$LOG
	logpath<-conf2$log_path
	level<-conf2$log_level
	lognames<-names(conf2)[!names(conf2) %in% c("log_path","log_level")]
	logs<-sprintf("%s/%s.log",logpath,tmp<-unlist(conf2[lognames],use.names=FALSE))
	names(logs)<-gsub("log_","",lognames)
	logs[tmp=="console"]<-""
	ritorno<-setNames(vector("list",length(logs)),names(logs))
	for (i in seq_along(ritorno)) {
		ritorno[[i]]<-logger(threshold=level,appenders=file_appender(logs[i], append = TRUE, layout=standardLayout))
	}
	ritorno
}
