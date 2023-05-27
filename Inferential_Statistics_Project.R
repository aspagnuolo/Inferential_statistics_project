##########################################################################################

dati <- read.csv("neonati.csv",sep=",", stringsAsFactors = T)

#il dataset presenta 2500 osservazioni e 10 variabili.
#Le variabili contengono info riguardo sia la madre che il neonato.
#Variabili Quantitative:
#Anni.madre, N.gravidanze, Gestazione, Peso, Lunghezza, Cranio
#Variabili Qualitative:
#Fumatrici, Tipo.Parto, Ospedale, Sesso

#l'obiettivo del progetto è quello di prevedere il peso del neonato alla nascita
#conoscendo la relazione con le altre variabili.

#ai fini dell'analisi potrebbe essere comodo convertire in
#fattori anche la colonna "Fumatrici":
dati$Fumatrici <- as.factor(dati$Fumatrici)

fumP <- table(dati$Fumatrici)
addmargins(fumP)
fum <- round(100*prop.table(fumP),digits=2)
barplot(fum, xlab='Fumatrici',ylab='Percentuale',
        col=c("darkblue","lightcyan")
        ,legend=rownames(fum), args.legend = list(x = "topright"))


ParP <- table(dati$Tipo.parto)
addmargins(ParP)
Par <- round(100*prop.table(ParP),digits=2)
Par

OspP <- table(dati$Ospedale)
addmargins(OspP)
Osp <- round(100*prop.table(OspP),digits=2)
Osp

SexP <- table(dati$Sesso)
addmargins(SexP)
Sex <- round(100*prop.table(SexP),digits=2)
Sex

#il 96% delle madri è non fumatrice.
#il 70.88% dei neonati è nato con parto naturale, mentre
#le nascite sono distribuite in modo abbastanza bilanciato
#tra i 3 ospedali e c'è una proporzione bilanciata
#anche tra maschi e femmine.

summary(dati$Anni.madre)
#nella colonna Anni.madre sono presenti dei valori "anomali"
#infatti il min è zero. cerco i valori minori di 10.
dati$Anni.madre[dati$Anni.madre<10]
#ci sono due dati errati (1, 0). Li sostituisco con la mediana
#in quanto più robusta della media.
dati$Anni.madre[dati$Anni.madre == 0] <- 28
dati$Anni.madre[dati$Anni.madre == 1] <- 28
attach(dati)
n=nrow(dati)
colonne_quant<-data.frame(Anni.madre,N.gravidanze,
                          Gestazione,Peso,Lunghezza,
                          Cranio)
#indici di posizione:
summary(colonne_quant)
#indici di variabilità:
library(matrixStats)
mat <- data.matrix(colonne_quant)
data.frame(ID=colnames(mat), 
           Range=colRanges(mat),
           IQR=colIQRs(mat),
           Variance=colVars(mat),
           Standard_Dev=colSds(mat)
)

cv<- function(x){
  return(sd(x)/mean(x)*100)} #coeff di variazione
cv(Anni.madre)
cv(N.gravidanze)
cv(Gestazione)
cv(Peso)
cv(Lunghezza)
cv(Cranio)

library(moments)
skewness(colonne_quant)
kurtosis(colonne_quant)-3

#la variabile Anni.madre presenta un'asimmetria leggermente positiva,
#questo significa che ci sono più madri giovani. La sua distribuzione
#si leggermente platicurtica.
#la variabile N.gravidanze è asimmetrica positiva (le
#prime gravidanze sono la maggioranza):
gravidanze_CL <- cut(N.gravidanze,
                 breaks = c(0,1,3,6,12), include.lowest = TRUE)
N<-dim(dati)[1]
ni <- table(gravidanze_CL)
fi <- table(gravidanze_CL)/N
Ni <- cumsum(ni)
Fi <- Ni/N
cbind(ni,fi,Ni,Fi)
distr_freq_gravidanze_CL<- as.data.frame(cbind(ni,fi,Ni,Fi))
barplot(fi, 
        main= "Distribuzione delle classi di gravidanze",
        xlab = 'Numero di gravidanze in classi',
        ylab = 'Frequenze relative', col = 'blue',
        names.arg = rownames(distr_freq_gravidanze_CL))
#inoltre la sua distribuzione è fortemente leptocurtica (più appuntita di una normale).

#la variabile Gestazione presenta una distribuzione asimmeterica negativa
#e, con una curtosi > 0, leptocurtica.
prova <- as.factor(dati$Gestazione)
gestP <- table(dati$Gestazione)
addmargins(gestP)
gest <- round(100*prop.table(gestP),digits=2)
gest
#la maggior parte delle gestazioni ricade tra le 38 e
#le 41 settimane.

#le variabili peso, lunghezza e cranio presentano una 
#distribuzione asimmetrica negativa e leptocurtica

##########################################################################################
#da letteratura scientifica si riporta una media del peso
#per i neonati di 3.3kg ed una lunghezza di 50cm (nessuna informazione riguardo dev. standard).
#https://www.ospedalebambinogesu.it/da-0-a-30-giorni-come-si-presenta-e-come-cresce-80012/
#quindi effetuiamo un ttest sul Peso e sulla Lunghezza
#per saggiare le ipotesi nulle.

t.test(Peso,
       mu = 3300,
       alternative = 'two.sided',
       conf.level = 0.95)
#il pvalue è maggiore del livello di significatività.
#L'ipotesi nulla non si rigetta, quindi la media del peso
#dei neonati è significativamente simile a quella della popolazione.

t.test(Lunghezza,
       mu = 500,
       alternative = 'two.sided',
       conf.level = 0.95)
#il pvalue è molto basso.
#si rifiuta l'ipotesi nulla.quindi la media della lunghezza
#dei neonati è significativamente diversa da quella della popolazione.

##########################################################################################

t.test(data=dati,
       Peso~Sesso,
       paired=F)
#si rifiuta l'ipotesi nulla di uguaglianza tra medie.
#i maschi pesano più delle femmine.

t.test(data=dati,
       Lunghezza~Sesso,
       paired=F)
#si rifiuta l'ipotesi nulla di uguaglianza tra medie.
#i maschi sono più lunghi delle femmine.

t.test(data=dati,
       Cranio~Sesso,
       paired=F)
#si rifiuta l'ipotesi nulla di uguaglianza tra medie.
#il cranio dei maschi è più grande di quello delle femmine.

t.test(data=dati,
       Gestazione~Sesso,
       paired=F)
#si rifiuta l'ipotesi nulla di uguaglianza tra medie.
#il tempo di gestazione per le femmine è minore di quello dei maschi 

##########################################################################################
#Si vocifera che in alcuni ospedali si facciano più parti cesarei, 
#per verificare questa ipotesi utilizziamo il test di indipendenza
#chi quadrato:
dataset_ospedale= table(Ospedale,Tipo.parto)
dataset_ospedale
chisq.test(dataset_ospedale)
#non si rifiuta l'ipotesi di indipendenza.
#non sembrano esserci dipendenze tra gli ospedali e il tipo di parto.
#Analisi Grafica:
ggpubr::ggballoonplot(data = as.data.frame(dataset_ospedale),
                      fill='red', size.range = c(6, 10))
#le frequenze sono molto simili tra loro (no dipendenza)

##########################################################################################
#variabile risposta: Peso
#varibili esplicative: le altre


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(dati,upper.panel = panel.smooth, lower.panel = panel.cor)

#le variabili esplicative con un'alta correlazione 
#con la variabile risposta, verosimilmente saranno
#quelle che ci daranno più informazioni su di essa,
#mentre i regressori molto correlati tra loro potrebbero
#dare problemi di multicollinearità.

#esiste una correlazione pos. forte tra la variabile risposta e le var. Lunghezza e
#Cranio ed una correlazione pos. moderata tra Peso/Gestazione. Quest'ultima è 
#moderatamente correlata a sua volta con Lunghezza e Cranio. 
#Esiste una correlazione pos. debole tra Anni.madre/N.gravidanze e tra Peso/Sesso

##########################################################################################
#controlliamo se la variabile risposta ha una distribuzione normale:
moments::skewness(Peso)
moments::kurtosis(Peso)-3
shapiro.test(Peso)
#la variabile risposta ha una distribuzione non normale, questo
#ricadrà anche sui residui una volta stimato il modello, perchè 
#non riuscirà a filtrare bene le variabili non normali
plot(density(Peso))

mod1=lm(Peso~.,data=dati)
summary(mod1)
#con il summary abbiamo le stime dei coefficienti beta per
#tutte le variabili, che rappresentano gli effetti marginali di
#ogni singola variabile sulla variabile risposta.
#per ogni stima si controlla: segno, valore e significatività
#le settimane di gestazione, la lunghezza, il diametro del cranio
#e il sesso hanno un effetto positivo e molto significativo
#sulla variabile risposta. il n. gravidanze e il tipo di 
#parto hanno un effetto positivo e significativo sulla variabile
#risposta. Le altre varibili non hanno effetti significativi.
#L'Adjusted R2 = 0.7278: il modello riesce a spiegare circa il 
#73% della variabilità

##########################################################################################
#utilizzo la tecnica stepwise automatica, impostando
#il criterio BIC piuttosto che AIC, in quanto il primo
#tende a preferire modelli più semplici e meno sovraparametrizzati.
mod2 <- MASS::stepAIC(mod1, direction = 'both',
                              k=log(n))
summary(mod2)
BIC(mod1,mod2)
#naturalmente il criterio di valutazione BIC è minore 
#per il mod2.

#provo a reinserire nel modello la variabile Tipo.parto
#perchè penso possa essere importante.
mod3 <- update(mod2,~.+Tipo.parto)
summary(mod3)
#Adjusted R-squared migliora di poco 
BIC(mod2,mod3)
AIC(mod2,mod3)
#il BIC del modello 2 è leggermente inferiore, 
#l'AIC del modello 3 è leggermente inferiore.
anova(mod3,mod2)
#per capire se aggiungere o meno la variabile al modello,
#utilizzo l'ANOVA, che rapporta le varianze spiegate dei modelli
#e dice se c'è stato un aumento o diminuzione significativo.
#in questo caso l'aumento sembra essere significativo.

#dal grafico del pairs sembra esserci una relazione 
#quadratica tra il peso e la gestazione
mod4 <- update(mod3,~.+I(Gestazione^2))
summary(mod4)
anova(mod4,mod3)
#Adjusted R-squared migliora leggermente.
#all'interno del modello compare due volte la variabile
#Gestazione e potrebbero esserci problemi di multicollinearità,
#la verifichiamo con VIF (variance inflation factor)
library(car)
vif(mod4) #questi valori devono essere minori di 5
#elimino la variabile Gestazione:
mod5 <- update(mod4,~.-Gestazione)
summary(mod5)
BIC(mod4,mod5)
#Uso mod5 come modello.

##########################################################################################
#analisi GRAFICA dei residui del modello
#i residui del modello devono essere 'puliti':
#rispettare le assunzioni di normalità, omoschedasticità,
#Indipendenza fra loro e dalla X e media di zero.

par(mfrow=c(2,2))
plot(mod5)

#nel primo grafico i residui devono distribuirsi casualmente intorno alla 
#media di zero -> nel nostro caso sembra esserci un pattern ricurvo.
#nel secondo grafico i residui devono distribuirsi sulla bisettrice del grafico
#(seguono una distribuzione normale) -> nel nostro caso i residui si spostano
#dalla bisettrice sia nella parte inziale che nella parte finale del grafico.
#nel terzo grafico non si devono verificare dei pattern
#ma i valori devono essere distribuiti in modo casuale intorno 
#alla linea -> sembra esserci anche in questo caso una curvatura.
#nell'ultimo grafico si evidenziano i valori influenti
#(leverage e outliers), quando i punti superano la linea della
#distanza di Cook si dice che i punti sono potenzialmente
#influenti sulle stime di regressione -> nel nostro caso il punto 1551
#supera la soglia di Cook di 0.5

#analisi NUMERICA dei residui del modello
library(lmtest)
#Test di Breusch-Pagan (ipotesi di omoschedasticità)
bptest(mod5) #viene rifiutata l'ipotesi nulla: non omoschedasticità
#Test di Durbin–Watson (presenza di autocorrelazione dei residui)
dwtest(mod5) #non viene rifiutata l'ipotesi nulla: i residui non sono autocorrelati
#Test di Shapiro-Wilk (verifica della normalità dei residui)
shapiro.test(residuals(mod5)) #si rifiuta l'ipotesi nulla: i residui non sono normali
#leverage
leva <- hatvalues(mod5)
plot(leva)
p=sum(leva)
soglia=2*p/n #impostiamo la soglia per i valori di leva
abline(h=soglia,col=2)
length(leva[leva>soglia]) #sono 133 i valori che si trovano lontano 
                          #rispetto al resto delle osservazioni nello spazio dei regressori
#outliers
plot(rstudent(mod5))
abline(h=c(-2,2), col=2)
outlierTest(mod5) #TestT per identificare gli outliers (1551, 155, 1306)

#per considerare leverage e outliers contemporaneamente possiamo 
#utilizzare la distanza di cook
cook <- cooks.distance(mod5)
plot(cook)
which.max(cook) #osservazione 1551

##########################################################################################
#il modello è 'abbastanza' appropriato per fare delle prevesioni.
#L'Adjusted R-squared di 0.7273 non è molto alto e in più
#partiamo da una distribuzione della variabile risposta non normale, 
#con ripercussioni anche sui residui, i quali non rispettano tutte le
#assunzioni degli errori del modello.

##########################################################################################
#Prevedere il peso di una #neonata, considerato che la madre è alla terza gravidanza 
#e partorirà alla 39esima settimana. Niente misure dall’ecografia.

#non sono presenti le misure dall’ecografia, quindi uso 
#la mediana delle variabili Lunghezza e Cranio, inoltre 
#manca l'informazione sulla tipologia di parto, in questo caso uso "Naturale"
#in quanto circa il 71% dei parti presentano questa tipologia.
N.gravidanze <- c(3)
median(dati$Lunghezza)
Lunghezza <- c(500)
median(dati$Cranio)
Cranio <- c(340)
Sesso <- c('F')
Tipo.parto <- c('Nat')
Gestazione <- c(39)

dati_nuovo <- data.frame(N.gravidanze, Lunghezza,
                         Cranio, Sesso, Tipo.parto, Gestazione)
predict(mod5, newdata = dati_nuovo)
#PESO: 3333.745

##########################################################################################

library(rgl)
?scatter3d
scatter3d(dati$Peso~dati$Gestazione+dati$Lunghezza+dati$Sesso, grid=F)





