library(ggplot2)
library(sf)
library(plotly)
library(leaflet)
library(data.table)
library(DT)
library(zoo)
source("funzionifit.R")
options(bitmapType="cairo")



animazione <- FALSE
scalaSingolaProvincia <-FALSE

verbose <- TRUE
assignout <- FALSE
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

a<-lapply(regioniList,function(x){
    province<-sort(unique(allData_prv[allData_prv$denominazione_regione==x,]$denominazione_provincia))
    pal<-d3hexcols20[1:length(province)]
    names(pal)<-province
    return(pal)})
color_prov<-unlist(a)
color_province<-c( "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
        "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
        "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
        "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
        "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
        "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
        "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
        "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
        "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
        "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
        "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
        "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C",
        "#83AB58", "#001C1E", "#D1F7CE", "#004B28", "#C8D0F6", "#A3A489", "#806C66", "#222800",
        "#BF5650", "#E83000", "#66796D", "#DA007C", "#FF1A59", "#8ADBB4", "#1E0200", "#5B4E51",
        "#C895C5", "#320033", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94", "#7ED379", "#012C58")[1:length(provinceList)]
names(color_province)	 <- provinceList

allData_reg <- readRDS(paste0(dir_reg, regRDS))
mtimeProv <- file.info(paste0(dir_prov,provRDS))$mtime
mtimeReg 	<-file.info(paste0(dir_reg,regRDS))$mtime

modelliReg <- readRDS("www/modelliReg.RDS")
modelliIta <-readRDS("www/modelliIta.RDS")

if(file.exists("www/modelliTIReg.RDS")){
	modelliTIReg <- readRDS("www/modelliTIReg.RDS")
} else modelliTIReg <- NULL
if(file.exists("www/modelliTIRegExp.RDS")){
	modelliTIRegExp <- readRDS("www/modelliTIRegExp.RDS")
}else modelliTIRegExp <- NULL

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




spiegaFitMedioTermine<- HTML("<div style='padding-top:10px;'>In questo grafico viene mostrato il numero di cati totali (deceduti) con una previsione che si estende fino a 10 giorni: in blu la serie storica, in arancione i valori previsti per i prossimi giorni. Continuando a mantenere il distanziamento sociale come in atto da qualche settimana prevediamo che il numero di contagi totale non superi i 150 mila.</div>")


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

spiegaLinePlot <- HTML("<div style='padding-top:10px;'>In questo grafico mostriamo l'andamento del numero di casi confermati
di CoVid19. <br>Ciascuna area territoriale
&egrave; indicata con un colore diverso. &Egrave; possibile ingrandire aree specifiche del grafico
e disabilitare (o riabilitare) singoli territori interagendo con la legenda del grafico.</div>")


spiegaTabellaCompare <- HTML("<div style='padding-top:10px;'>In questa tabella vengono confrontate le previsioni ed i valori osservati del numero di casi totali, dei decessi, del numero di ospedalizzati e del numero di ricoverati in terapia intensiva.
<br>Sono stati confrontati i due modelli, l'esponeziale quadratico e l'esponenziale puro. La forte discordanza tra previsioni e valori osservati nel modello esponenziale indica che siamo usciti da quel regime di crescita: le misure preventive adottate dal governo iniziano ad avere effetto!</div>")


# Descrizione Terapia Intensiva
spiegaTerIntPrevisione <- HTML("<div style='padding-top:10px;'>In questo grafico vengono rappresentati il numero di posti letto in terapia intensiva, aggiornati al 2018, e le previsioni del numero di pazienti che hanno bisogno di terapia intensiva. Non avendo sufficienti dati per ogni regione le proiezioni sono state fatte moltiplicanzo il numero di terapie intensive su contagiati totali per le previsioni del numero di contagiati per regione. Le barre verticali rappresentano l'incertezza standard.I dati sulla disponibilità dei posti letto è aggioranta al 2018 e non si tiene conto del continuo sforfo che sta affrontado il Sistema Sanitario Nazionale per aumentarne la disponibilità nel minor tempo possibile.</div>")

spiegaTerIntPercentuale <- HTML("<div style='padding-top:10px;'>In questo grafico viene rappresentata la percentuale di posti letto occupati da pazienti in terapia intensiva rispetto al numero di posti letto disponibili, aggiornati al 2018.</div>")


spiegaTerIntAttuale <- HTML("<div style='padding-top:10px;'>In questo grafico vengono rappresentati il numero di posti letto in terapia intensiva, aggiornati al 2018, ed il numero di pazienti in terapia intensiva con CoVid19 per ogni regione.</div>")


#########################


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
