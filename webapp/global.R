library(ggplot2)
library(sf)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
library(zoo)
source("funzionifit.R")
source("gompertz.R")
options(bitmapType="cairo")

animazione <- FALSE
scalaSingolaProvincia <-FALSE

verbose <- TRUE
assignout <- TRUE
saveRDSout <- FALSE

regioni2fit <- c('Lombardia', 'Emilia Romagna', 'Veneto')
province2fit <- c('Bergamo', 'Brescia', 'Milano')

dir_prov 	<- "www/pcm_data/"
dir_reg		<- "www/dati-regioni/"
dir_data	<- "www/"

provRDS <- "dataProvince.RDS"
regRDS <- "dataRegioni.RDS"
provShapeRDS <- "ProvinceShapeF.RDS"
regShapeRDS <- "RegioniShapeF.RDS"

campiPrevisioni <- c("totale_casi", "deceduti", "totale_ospedalizzati", "terapia_intensiva")

campiTotali <- c("totale_casi", "tamponi", "totale_attualmente_positivi", "deceduti", "totale_ospedalizzati", "terapia_intensiva", "ricoverati_con_sintomi" , "nuovi_attualmente_positivi" , "dimessi_guariti")


pop_file <- read.csv("www/tavola_pop_res01.csv", stringsAsFactors=F, skip=1)
colnames(pop_file) <- c("codice_provincia", "provincia", "pop_m", "pop_f", "pop")

letti2018 <- readRDS("www/letti2018.RDS")
TIindx <- grep("TERAPIA INTENSIVA", letti2018$Descrizione.disciplina)
Tintensiva <- aggregate(letti2018[TIindx, 'Totale.posti.letto'],by=list( denominazione_regione=letti2018[TIindx, 'Descrizione.Regione']), sum)
setnames(Tintensiva, old="x", new="lettiTI")

letality<-read.csv2(file="report_iss/Letality.csv",stringsAsFactors=FALSE)


# Temi dell'applicazione
d3cols <- "1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf"
d3hexcols <- paste0("#",regmatches(d3cols, gregexpr(".{6}", d3cols))[[1]])
d3col1 <- d3hexcols[1]

d3cols20 <- "1f77b4aec7e8ff7f0effbb782ca02c98df8ad62728ff98969467bdc5b0d58c564bc49c94e377c2f7b6d27f7f7fc7c7c7bcbd22dbdb8d17becf9edae5"
d3hexcols20 <- paste0("#",regmatches(d3cols20, gregexpr(".{6}", d3cols20))[[1]])
d3hexcols20[12]<-'#f5d742'
  d3hexcols20[11]<-'#d4a928'
  d3hexcols20[17]<-'#bcd11b'


colori<-c("#CED6A7", "#F4F49C", "#D162F1", "#5E4631", "#5F0D7C", "#2F7562", "#969881",
"#E74C63", "#DD43A7", "#16A02F", "#f3b0ff", "#BDA78E", "#2766BB", "#F193D4",
 "#ABD9E8", "#2676B8", "#e06936", "#F4CE99", "#8F9A15", "#2487B6", "#DF4596",
 "#DA41B8", "#BC8EEF", "#73CEDC", "#8A254E", "#E691F0", "#E94E51", "#B02424",
 "#2955BD", "#ACD081", "#2297B3", "#4B60DB", "#B4C0BB", "#E3AD44", "#ABE2E8",
 "#606F57", "#306F3B", "#D83FC9", "#D3B25A", "#ABE7AF", "#E9E7E7", "#C3B770",
 "#B3BB85", "#5030C7", "#BB2C49", "#9036CE", "#476930", "#7A34CC", "#626431",
 "#C68F61", "#FD7D62", "#93C5B1", "#880D51", "#F295AE", "#A5F8DE", "#24B024",
 "#EB6251", "#f5d742", "#1CAA7E", "#D0F59E", "#F4A92F", "#850D6B", "#3B2FC4",
 "#4098D1", "#8456E6", "#D13DD6", "#1FAFA7", "#1DAC92", "#E44A74", "#159E1D",
 "#334736", "#711173", "#593134", "#9A4C15", "#17A243", "#1AA76A", "#141F1C",
 "#7E0D81", "#36C6BB", "#430D78", "#A1543E", "#EE7853", "#280D74", "#20A7B1",
 "#A9E2C3", "#2CBB6F", "#2B44C0", "#A2F7B9", "#BB3AD3", "#6532C9", "#A538D1",
 "#D95E08", "#2D33C2", "#8F5D0F", "#A3C09B", "#B2DCA8", "#0F0D71", "#A5381C",
 "#5BA51C", "#83C9C6", "#CBA4A8", "#F3A697", "#CF7634", "#ADF6A0", "#0C216D",
"#E24885", "#B9AA5D")

color_province<-colori


my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 7),
              axis.text = element_text(size=7),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}

my_ggtheme <- function() {
  theme_minimal() +
            theme(legend.text = element_text(size = 10),
              axis.text = element_text(size=10),
              legend.title = element_text(size = 10),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 10),
              strip.text = element_text(size = 9))
}

# dati epidemiolocigi
allData_prv <- readRDS(paste0(dir_prov,provRDS))
allData_reg_flt <- readRDS(paste0(dir_prov,provRDS))
## globalizziamo le aggregazioni per le mappe...
allData_prv <- allData_prv[!grepl("aggiornamento", allData_prv$denominazione_provincia),]
allData_prv <- aggregate(list(totale_casi=allData_prv$totale_casi, pop=allData_prv$pop),
                  by=list(data=allData_prv$data, denominazione_regione=allData_prv$denominazione_regione,
                          denominazione_provincia=allData_prv$denominazione_provincia, codice_provincia=allData_prv$codice_provincia),
                  FUN=sum)
allData_prv$`casi su 10mila abit` <- round(allData_prv$totale_casi / allData_prv$pop * 10000, 3)

allData_reg_flt <- allData_reg_flt[!grepl("aggiornamento", allData_reg_flt$denominazione_provincia),]
allData_reg_flt <- aggregate(list(totale_casi=allData_reg_flt$totale_casi, pop=allData_reg_flt$pop),
                              by=list(data=allData_reg_flt$data,
                                  denominazione_regione=allData_reg_flt$denominazione_regione, codice_regione=allData_reg_flt$codice_regione),
                              FUN=sum)
allData_reg_flt$`casi su 10mila abit` <- round(allData_reg_flt$totale_casi / allData_reg_flt$pop * 10000, 3)
## fine parte per mappe
regioniList <- sort(unique(allData_reg_flt$denominazione_regione))
provinceList <- sort(unique(allData_prv$denominazione_provincia))

color_regioni<-d3hexcols20
names(color_regioni)<-regioniList

names(color_province)	 <- provinceList


allData_reg <- readRDS(paste0(dir_reg, regRDS))
mtimeProv <- file.info(paste0(dir_prov,provRDS))$mtime
mtimeReg 	<-file.info(paste0(dir_reg,regRDS))$mtime

modelliReg <- readRDS("www/modelliReg.RDS")
modelliIta <-readRDS("www/modelliIta.RDS")

modelliRegGomp <- readRDS("www/modelliRegGomp.RDS")
modelliItaGomp <-readRDS("www/modelliItaGomp.RDS")

if(file.exists("www/modelliRegExp.RDS"))
	modelliRegExp <- readRDS("www/modelliRegExp.RDS")
if(file.exists("www/modelliItaExp.RDS"))
	modelliItaExp <-readRDS("www/modelliItaExp.RDS")

date_range_prv <- range(allData_prv$data)
date_range_reg <- range(allData_reg$data)

date0_prv <- min(date_range_prv, na.rm=T)
date0_reg <- min(date_range_reg, na.rm=T)

province <- readRDS(paste0(dir_prov, provShapeRDS))
regioni <- readRDS(paste0(dir_reg, regShapeRDS))

eu_to_plot <- readRDS(file=paste0(dir_data, "eu_to_plot.RDS") )
italy <- readRDS(file=paste0(dir_data, "italy.RDS") )

map_italia <- readRDS(file=paste0(dir_data, "map_italia.RDS"))
map_regioni <- readRDS(file=paste0(dir_data, "map_regioni.RDS"))


risultatiFit <- HTML("<div style='padding-top:10px;'></div>")


prvReg <- unique(allData_prv[, c('denominazione_regione', 'denominazione_provincia')])
setDT(prvReg)

spiegaFitMedioTermine<- HTML("<div style='padding-top:10px;'>In questo grafico viene mostrato il numero di cati totali (deceduti) con una previsione che si estende fino a 10 giorni: in blu la serie storica, in arancione i valori previsti per i prossimi giorni.</div>")


spiegaVariazionePercentuale<- HTML("<div style='padding-top:10px;'>In questo grafico viene mostrato l'aumento percentuale dei casi totali rispetto al giorno precedente. Se l'aumento fosse esponenziale questo valore sarebbe costante in media, nel caso di andamento esponenziale quadratico avremmo una retta discendente. La diminuzione della variazione percentuale di casi totali indica che il rate di espansione del virus sta diminuendo. Questo potrebbe essere dovuto al raggiungimento di un numero di contagi così alto da rendere meno veloce un ulteriore propagazione del virus oppure essere il risultato delle misure di contenimento. Siamo convinti che questa diminutione sia il risultato del contenimento.</div>")

spiegaConfrontoSerieRegioni<- HTML("<div style='padding-top:10px;'>In questo è possibile confrontare l'andamento temporale di contagi (deceduti) nelle diverse regioni di Italia. Grazie agli slider input è possibile spostare in avanti e indietro le diverse serie storiche.</div>")


spiegaConfrontoSerieProvince<- HTML("<div style='padding-top:10px;'>In questo è possibile confrontare l'andamento temporale di contagi nelle diverse province italiane. Grazie agli slider input è possibile spostare in avanti e indietro le diverse serie storiche.</div>")


spiegaFitPos <- HTML("<div style='padding-top:10px;'>In questo grafico mostriamo la serie storica dei casi confermati di
CoVid19 nelle regioni selezionate ed una previsione che si estende fino a tre giorni, linea tratteggiata. I grafici sono interattivi, è possibile conoscere il valore esatto di punti e delle linee scorrendo con il mouse su di essi.
Abbiamo utilizzato modelli esponenziali dando maggiore importanza ai dati pi&ugrave; recenti. Se si seleziona l'opzione
&quot;Logaritmico&quot; come tipo di grafico, l'asse delle ordinate diventa logaritmico e un andamento esponenziale &egrave; rappresentato da una retta.</div>")

spiegaFitTot <- HTML("<div style='padding-top:10px;'>In questo grafico mostriamo la serie storica dei casi confermati,
dei pazienti ospedalizzati, dei pazienti in terapia intensiva e dei defunti a causa del CoVid19 in Italia ed una
previsione che si estende fino a tre giorni, linea tratteggiata. I grafici sono interattivi, è possibile conoscere il valore esatto di punti e delle linee scorrendo con il mouse su di essi. Abbiamo utilizzato modelli esponenziali dando maggiore
importanza ai dati pi&ugrave; recenti. Se si seleziona l'opzione &quot;Logaritmico&quot; come tipo di grafico, l'asse delle ordinate
diventa logaritmico e un andamento esponenziale &egrave; rappresentato da una retta.</div>")

spiegaFitTotePos<- HTML("<div style='padding-top:10px;'>In entrambi i grafici mostriamo la serie storica dei casi confermati di
CoVid19 in Italia ed una previsione che si estende fino a tre giorni, linea tratteggiata. Nel primo sono rappresentati per regione selezionata e nel secondo sono suddivisi tra
pazienti ospedalizzati, pazienti in terapia intensiva e defunti.
Abbiamo utilizzato modelli esponenziali dando maggiore importanza ai dati pi&ugrave; recenti. Se si seleziona l'opzione
&quot;Logaritmico&quot; come tipo di grafico, l'asse delle ordinate diventa logaritmico e un andamento esponenziale &egrave; rappresentato da una retta.</div>")


spiegaMappa <- HTML("<div style='padding-top:10px;'>In questa mappa mostriamo la diffusione sul territorio dei casi confermati
di CoVid19.
<br>La scala di colore parte dal giallo per le aree con il minor numero assoluto di
casi confermati e arriva al rosso per le aree con il maggior numero assoluto di casi confermati.
<br>Passando sulla mappa potete vedere l'effettivo numero di casi confermati in ciasuna area.</div>")

spiegaTabella <- HTML("<div style='padding-top:10px;'>In questa tabella mostriamo la diffusione sul territorio dei casi confermati
di CoVid19.
<br>La colonna &quot;casi totali&quot; riporta il numero totale di casi confermati di CoVid19
nel territorio, mentre la colonna &quot;casi su 10mila abit.&quot; riporta il numero di casi per ogni
10mila abitanti cos&igrave; da contestualizzare la diffusione rispetto alla popolazione presente
nel territorio.</div>")


spiegaTabellaMonitor <- HTML("<div style='padding-top:10px;'>In questa tabella mostriamo alcune grandezze utili per monitorare l'andamento della diffusione dell'epidemia nelle diverse località italiane. Il tasso di crescita, ovvero la percentuale dei nuovi casi sui casi totali del giorno precedente, viene mostrata anche come media degli ultimi 7 giorni. È evidente come ci sia una certa periodicità settimanale nel numero di tamponi analizzati e quindi dei nuovi casi riscontrati, nel week end vengono analizzati meno tamponi.</div>")

spiegaLinePlot <- HTML("<div style='padding-top:10px;'>In questo grafico mostriamo l'andamento del numero di casi confermati
di CoVid19. <br>Ciascuna area territoriale
&egrave; indicata con un colore diverso. &Egrave; possibile ingrandire aree specifiche del grafico
e disabilitare (o riabilitare) singoli territori interagendo con la legenda del grafico.</div>")


spiegaTabellaCompare <- HTML("<div style='padding-top:10px;'>In questa tabella vengono confrontate le previsioni per i due modelli implementati ed i valori osservati del numero di casi totali, dei decessi, del numero di ospedalizzati e del numero di ricoverati in terapia intensiva.
<br>Tornando indietro nel tempo è possibile vedere come le previsioni fatte con il modello esponenziale di distacchino sempre di più dai valori veri, sovrastimando sia i casi totali che i decessi. Il modello esponenziale quadratico riesce a prevedere meglio l'andamento dell'epidemia ma negli ultimi giorni è evidente come questo modello sottostimi costantemente l'incremento dei casi: siamo usciti anche dal regime quadratico, per modellizzare il fenomeno in questo momento andrebbero considerati  fattori che non sono identificabili a partire dai soli dati pubblicati ogni giorno dalla protezione civile.</div>")


# Descrizione Terapia Intensiva
spiegaTerIntPrevisione <- HTML("<div style='padding-top:10px;'>In questo grafico vengono rappresentati il numero di posti letto in terapia intensiva, aggiornati al 2018, e le previsioni del numero di pazienti che hanno bisogno di terapia intensiva. Non avendo sufficienti dati per ogni regione le proiezioni sono state fatte moltiplicanzo il numero di terapie intensive su contagiati totali per le previsioni del numero di contagiati per regione. Le barre verticali rappresentano l'incertezza standard.I dati sulla disponibilità dei posti letto è aggioranta al 2018 e non si tiene conto del continuo sforfo che sta affrontado il Sistema Sanitario Nazionale per aumentarne la disponibilità nel minor tempo possibile.</div>")

spiegaTerIntPercentuale <- HTML("<div style='padding-top:10px;'>In questo grafico viene rappresentata la percentuale di posti letto occupati da pazienti in terapia intensiva rispetto al numero di posti letto disponibili, aggiornati al 2018.</div>")


spiegaTerIntAttuale <- HTML("<div style='padding-top:10px;'>In questo grafico vengono rappresentati il numero di posti letto in terapia intensiva, aggiornati al 2018, ed il numero di pazienti in terapia intensiva con CoVid19 per ogni regione.</div>")


#########################

spiegaTabellaGompertz <- HTML("<div style='padding-top:10px;'> Nella tabella sono riportati i valori asintotici, ovvero i valori massimi raggiunti, utilizzanto un modello di tipo Gompertz aggiornato alla data indicata. Dal monitoraggio dell'andamento dell'asintoto (Totale Casi e Decessi) è possibile capire come si stia modificando l'andamento della diffusione: un andamento crecente, indica che la diffusione sta aumentando di velocità. Tra la seconda e la quarta settimana della fase 2 ci aspettiamo una crescita dei valori asintotici che speriamo si stabilizzino intorno a valori poco superiori a quelli previsti oggi. </div>")


spiegaGraficoCasiGiornalieriRegioni <- HTML("<div style='padding-top:10px;'> In questo grafico è stato riportato il numero di nuovi casi giornalieri divisi per regione.</div>")


spiegaGraficoNuoviPositiviStoricoRegPercentuale <- HTML("<div style='padding-top:10px;'> In questo grafico è stato riportato l'andamento del rapporto tra nuovi casi ed i casi totali del giorno precedente in funzione del tempo. Questo parametro, direttamente proporzionale ad R0, ha un andamento esponenzialmente descrescente dall'inizio del lock down. Nel grafico, il cui asse y è logaritmico, ha un andamento lineare decrescente. Nella fase due ci aspettiamo una deviazione rispetto a questo andamento: il tasso di crescita probabilmente ricomincerà a salire tra la seconda e la terza settimana di Maggio.</div>")


spiegaGraficoNuoviPositiviStoricoPrvPercentuale <- HTML("<div style='padding-top:10px;'> In questo grafico è stato riportato l'andamento del rapporto tra nuovi casi ed i casi totali del giorno precedente in funzione del tempo. Questo parametro, direttamente proporzionale ad R0, ha un andamento esponenzialmente descrescente dall'inizio del lock down. Nel grafico, il cui asse y è logaritmico, ha un andamento lineare decrescente. Nella fase due ci aspettiamo una deviazione rispetto a questo andamento: il tasso di crescita probabilmente ricomincerà a salire tra la seconda e la terza settimana di Maggio.</div>")


spiegaGraficoCasiGiornalieriProvincia <- HTML("<div style='padding-top:10px;'> In questo grafico è stato riportato il numero di nuovi casi giornalieri divisi per provincia.</div>")


spiegaGraficoCasiVsCasiNuovi <- HTML("<div style='padding-top:10px;'> In questo grafico viene rappresentato il numero dei nuovi casi negli ultimi 7 giorni in funzione del numero di casi totali. Sappiamo che il trend di crescita del numero di contagiati non può cresce in modo esponenziale molto a lungo, ma come facciamo ad accorgerci del momento in cui il trend sta cambiando? Questo grafico ci aiuta a visualizzare quel momento: la crescira esponenziale è caratterizzata da un numero di contagi giornalieri proporzionali al numero di contagi totali. Tale andamento è rappresentato da una retta in questo grafico. Nel momento in cui la crescita smetterà di essere esponenziale inizieremo a vedere la curva piegarsi verso il basso.</div>")


fontiDati <- HTML("<div>Dati provenienti da:
<ul>
<li><a href='https://github.com/pcm-dpc/COVID-19'>Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile
</a> (Monitoraggio COVID-19)</li>
<li><a href='https://www.istat.it/it/archivio/222527'>ISTAT</a> (shapefile unit&agrave; amministrative e dati popolazione)</li>
<li><a href='
http://www.dati.salute.gov.it/dati/dettaglioDataset.jsp?menu=dati&idPag=96'>Ministero della Salute</a>
(Numero di posti letto in terapia intensiva, dati aggiornati al 2018)</li>
</ul></div>")
