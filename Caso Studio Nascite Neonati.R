#ANALISI NASCITE NEONATI

#IMPORT
library(moments)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggthemes)
library(PerformanceAnalytics)
library(knitr)
library(magrittr)
library(corrplot)
library(car)
library(lmtest)
library(sandwich)
library(estimatr)
library(dplyr)
library(Metrics)
library(MASS)


#FUNZIONI

#Funzione che calcola la moda/classe modale
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#COEFFIECENTE DI VARIAZIONE
CV <- function(x){
  return((sd(x)/mean(x))*100)
} 

#INDICE DI ETEROGENEITA DI GINI Normalizzato
G <- function(x){
  ni=table(x) #frequenze assolute
  fi=ni/length(x)#frequenze relative
  fi2 = fi^2 #frequenze relative al quadrato
  J = length(table(x)) #tipi di classi che abbiamo
  gini = 1-sum(fi2) #G=1-sommatoria di frequenze relative al quadrato
  gini_norm = gini/((J-1)/J)
  return(gini_norm)
}

#funzione che data una variabile quantitava divisa in classi calcola la distribuzione assoluta
distribuzione_assoluta <- function(x){
  n=length(x)
  ni=table(x)
  fi=table(x)/n
  Ni=cumsum(table(x))
  Fi=cumsum(table(x))/n
  return(as.data.frame(cbind(ni,fi,Ni,Fi)))
}

#funzione che genera una tabella contenente tutti gli indici di una variabile
#indici di posizione, variabilità e forma
analisi.quantitative <- function(x,y,z){
  
  df=as.data.frame(cbind(summary(x)))
  colnames(df)<-c(y)
  df2=data.frame(summary=c(max(x)-min(x),IQR(x),Mode(x),var(x),
                              sd(x),CV(x),skewness(x),kurtosis(x)-3))
  rownames(df2) <- c("Range","IQR","Mode","Var","SD","CV","Asymmetry","Curtosi")
  colnames(df2)<-c(y)
  
  df_all = rbind(df,df2)
  png("report.png", height = 24*nrow(df_all), width = 215*ncol(df_all))
  grid.table(df_all)
  dev.off()
  
  
  plot=ggplot()+
    geom_density(aes(x=x),col="darkblue",fill="lightblue")+
    geom_vline(aes(xintercept=mean(x)),
               color="red", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=quantile(x,seq(0,1,0.25))),
               color="green3", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=median(x)),
               color="orange", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=Mode(x)),
               color="yellow", linetype="dashed", linewidth=1)+
    xlab("Sales")+
    ylab("Density")+
    labs(title = c(z))+
    theme_fivethirtyeight()
  
  print("Distribuzione, Summary, Range, Range Interquantile, Moda, Variaza, Devizione standard, Coefficente di variazione, Assiemtria, Curtosi")
  return(
    list(
      plot,
      summary(x),
      max(x)-min(x),
      IQR(x),
      Mode(x),
      var(x),
      sd(x),
      CV(x),
      skewness(x),
      kurtosis(x)-3
    )
  )
}

#funzione che fa un analisi compleata delle variabili quantitative
#esegue tabelle di frequenza assoluta, relativa, cumulata e indice di gini
analisi.qualitative <- function(x){
  n=length(x)
  ni=table(x)
  fi=table(x)/n
  Ni=cumsum(table(x))
  Fi=cumsum(table(x))/n
  
  df=as.data.frame(cbind(ni,fi,Ni,Fi))
  
  png("Frequenze.png", height = 50*nrow(df), width = 100*ncol(df))
  grid.table(df)
  dev.off()
  
  
  png("Gini.png", height = 50*nrow(df), width = 50*ncol(df))
  grid.table(data.frame(Gini=c(G(x))))
  dev.off()
}

#funzione che data una variabile quantitativa e una sequenza di valori
#divide la variabile in classi e stampa un grafico a barre
analisi.classi <- function(x,y){
  classi = cut(x,y)#divisa in 7 classi da 1 centimetro l'uno
  
  df_freq = distribuzione_assoluta(classi)
  
  ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
    geom_bar(stat="identity")+
    labs(title="Distribuzione in classi",
         x="Classi",
         y="Frequenza")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text())+
    guides(fill=guide_legend(title="Classi"))
}


format(1000, scientific = TRUE)


#import dataset

#se computer portatile
setwd("C:\\Users\\gabri\\Desktop\\PrivateProject\\Caso-studio-Nascite-Neonati-")

#se computer fisso
setwd("C:\\Users\\gabri\\Desktop\\Private Project\\Caso-studio-Nascite-Neonati-")

neonati <- read.csv("neonati.csv",stringsAsFactors = T)

attach(neonati)
head(neonati)
summary(neonati)


#Analisi variabili

#Anni madre---------------------------------------------------------------------
summary(neonati$Anni.madre)
table(neonati$Anni.madre)#ci sono due valori fuori scala
detach(neonati)

#rimuoviamo dal dataset tutte le registrazioni in cui gli anni della madre sono minori di 12
#di solito la possibilià di rimanere incinete avviene in concomitanza con l'inzio il primo ciclo che è intorno al 12 anno di età
neonati.filtrato <- subset(neonati,Anni.madre>=12)
attach(neonati.filtrato)
table(neonati.filtrato$Anni.madre)

summary.anni.madre=analisi.quantitative(Anni.madre,"Anni Madre","Distribuzione Anni Madre")
summary.anni.madre

analisi.classi(Anni.madre,seq(10,50,5))

#Numero gravidanze--------------------------------------------------------------
summary(neonati.filtrato$N.gravidanze)

summary.n.gravidanze=analisi.quantitative(N.gravidanze,"N.Gravidanze","Distribuzione N.Gravidanze")
summary.n.gravidanze

#distribuione di frequenza numero gravidanze
df_freq = distribuzione_assoluta(N.gravidanze)

#plot ditribuzioone numero gravidanze
ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  guides(fill=guide_legend(title="Classi"))

#Fumatrici----------------------------------------------------------------------
table(Fumatrici)

#distribuzione assoluta
df_freq = distribuzione_assoluta(Fumatrici)

#bar plot del nostro campione per la variabile
ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_fill_discrete(labels = c("Fumatrici", "Non Fumatrici"))+
  guides(fill=guide_legend(title="Classi",))

#Gestazione---------------------------------------------------------------------
table(Gestazione)
summary(Gestazione)
summary.gestazione=analisi.quantitative(Gestazione,"Gestazione","Distribuzione Gestazione")
summary.gestazione

analisi.classi(Gestazione,seq(25,45,4))

#Peso---------------------------------------------------------------------------
summary(Peso)
summary.peso=analisi.quantitative(Peso,"Peso","Distribuzione Peso")
summary.peso

classi = cut(Peso,seq(0,5000,1000))
df_freq = distribuzione_assoluta(classi)

row.names(df_freq) <- c("[0,1000)","[1000,2000)","[2000,3000)","[3000,4000)","[4000,5000)") 

ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  guides(fill=guide_legend(title="Classi"))

#Lunghezza----------------------------------------------------------------------
summary(Lunghezza)
summary.lunghezza=analisi.quantitative(Lunghezza,"Lunghezza","Distribuzione Lunghezza")
summary.lunghezza

analisi.classi(Lunghezza,seq(300,600,100))

#Cranio-------------------------------------------------------------------------
summary(Cranio)
summary.cranio=analisi.quantitative(Cranio,"Cranio","Distribuzione Cranio")
summary.cranio

analisi.classi(Cranio,seq(200,400,50))

#Tipo parto---------------------------------------------------------------------
table(Tipo.parto)

#distribuzione assoluta
df_freq = distribuzione_assoluta(Tipo.parto)

#bar plot del nostro campione per la variabile
ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_fill_discrete(labels = c("Cessarion", "Naturale"))+
  guides(fill=guide_legend(title="Classi",))

#Ospedale-----------------------------------------------------------------------
table(Ospedale)

#distribuzione assoluta
df_freq = distribuzione_assoluta(Ospedale)
df_freq
G(df_freq)

#bar plot del nostro campione per la variabile
ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_fill_discrete(labels = c("Ospedale 1", "Ospedale 2", "Ospedale 3"))+
  guides(fill=guide_legend(title="Classi",))

#Sesso--------------------------------------------------------------------------
table(Sesso)

#distribuzione assoluta
df_freq = distribuzione_assoluta(Sesso)
df_freq
G(df_freq)

#bar plot del nostro campione per la variabile
ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=ni,fill=row.names(df_freq))) +
  geom_bar(stat="identity")+
  labs(title="Distribuzione in classi",
       x="Classi",
       y="Frequenza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_fill_discrete(labels = c("Femmine", "Maschi"))+
  guides(fill=guide_legend(title="Classi",))

#TEST IPOTESI

#peso medio popolazione 3300 grammi (maschi di solito 150 grammi in più)
#useremo un test t non avendo tutte le infomazioni della distribuzione originale
#ipotesi nulla: che la la media del peso del nostro campione sia uguale alla media del peso della popolazione
t.test(Peso,
       mu=3300,
       conf.level = 0.95, #0.95 perche 1-alfa
       alternative = "two.sided")

#calcoliamo i valori soglia
valori.soglia = qt(c(0.05/2,1-0.05/2),Peso) 

#facciamo un plot del grafo con i valori soglia e il risultato del test
ggplot()+
  geom_density(aes(x=rt(100000,Peso)),col="darkblue",fill="lightblue")+
  geom_point(aes(x=-1.505,y=0),col="green4",size=4)+
  geom_vline(aes(xintercept=valori.soglia),
             color="red", linetype="dashed", linewidth=1)+
  theme_fivethirtyeight()

#lunghezza media popolazione 50 centimetri
#ipotesi nulla: che la media della lunghezza del campione sia uguale alla media della lunghezza della popolazione
t.test(Lunghezza,
       mu=500,
       conf.level = 0.95, #0.95 perche 1-alfa
       alternative = "two.sided")

#rifiutiamo ipotesi nulla di uguaglianza tra le distribuzioni
#quindi la media di lunghezza del nostro campione è significativamete diversa da quella della popolazione

#calcoliamo i valori soglia
valori.soglia = qt(c(0.05/2,1-0.05/2),Lunghezza)

#facciamo un plot del grafo con i valori soglia e il risultato del test
ggplot()+
  geom_density(aes(x=rt(100000,Lunghezza)),col="darkblue",fill="lightblue")+
  geom_point(aes(x=-10.069,y=0),col="green4",size=4)+
  geom_vline(aes(xintercept=valori.soglia),
             color="red", linetype="dashed", linewidth=1)+
  theme_fivethirtyeight()

#verifichiamo se ci sono differenze significative tra le caratteristiche dei bambini di sesso diverso

#test sulla lunghezza
pairwise.t.test(Lunghezza, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

ggplot(data=neonati.filtrato)+
  geom_boxplot(aes(x=Sesso,y=Lunghezza,fill=Sesso))+
  labs(title="Distribuzione Lunghezza dei neonato per sesso",
       x="Sesso",
       y="Lunghezza")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#test sul peso
pairwise.t.test(Peso, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

ggplot(data=neonati.filtrato)+
  geom_boxplot(aes(x=Sesso,y=Peso,fill=Sesso))+
  labs(title="Distribuzione Peso dei neonato per sesso",
       x="Sesso",
       y="Peso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#test sul cranio
pairwise.t.test(Cranio, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

ggplot(data=neonati.filtrato)+
  geom_boxplot(aes(x=Sesso,y=Cranio,fill=Sesso))+
  labs(title="Distribuzione Grandezza cranio per sesso",
       x="Sesso",
       y="Cranio")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#test sulle settimane di gestazione
pairwise.t.test(Gestazione, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

ggplot(data=neonati.filtrato)+
  geom_boxplot(aes(x=Sesso,y=Gestazione,fill=Sesso))+
  labs(title="Distribuzione Settimane Gestazione per sesso",
       x="Sesso",
       y="Gestazione")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),legend.position='none')

#verificare che in alcuni ospedali si facciano più parti cesari

#per questa ipotesi ci basta osservare la tabella di frequenza 
distr_freq_ass_osp_tipo_parto = table(Ospedale,Tipo.parto)
distr_freq_ass_osp_tipo_parto
distr_freq_rel_osp_tipo_parto = table(Ospedale,Tipo.parto)/length(Ospedale)
distr_freq_rel_osp_tipo_parto

#Grafico a barro più esplicativo
ggplot(data=neonati.filtrato, aes(x=Ospedale, y=Tipo.parto,fill=Tipo.parto)) +
  geom_bar(stat="identity")+
  labs(title="Tipi di parti per Ospedale",x="Ospedali",y="Tipo di parto")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),axis.text.y=element_blank())+
  guides(fill=guide_legend(title="Tipo parto"))

#matrice di correlazione

#di R
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

x11()
pairs(neonati,upper.panel = panel.smooth,lower.panel = panel.cor)

#con libreria esterna

#creiamo un dataset secondario per togliere i dati non numerici
neonati.numeric <- neonati.filtrato
neonati.numeric$Tipo.parto <- ifelse(neonati.filtrato$Tipo.parto=="Nat",1,0)
neonati.numeric$Sesso <- ifelse(neonati.filtrato$Sesso=="M",1,0)
neonati.numeric$Ospedale <- ifelse(neonati.filtrato$Ospedale=="osp1",1,
                                   ifelse(neonati.filtrato$Ospedale=="osp2",2,3))

chart.Correlation(neonati.numeric,histogram=F, pch=19)
#gli asterischi mostrano il livello di significativià con il pvalue 
#p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

corrplot(cor(neonati.numeric),
         method = "circle",       
         order = "hclust",         # Ordering method of the matrix
         hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
         addrect = 2,              # If order = "hclust", number of cluster rectangles
         rect.col = 3,             # Color of the rectangles
         rect.lwd = 3)  

#Quindi, ricordiamo che la variabile risposta è il peso
#dalla matrice di correlazione vediamo che le variabili più correlte positivamente sono
# 1)Lunghezza 2)Cranio 3)Gestazione 4) Sesso --> queste 4 variabili inoltre sono anche statisitcamente rilevanti

#Plotiamo degli scatterplot per analizzare meglio la correlazione e vedere se ci sono iterazioni non linerari
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Lunghezza,col=Sesso))+
  labs(title="Correlazione Peso-Lunghezza per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#ipotiziamo un modello lineare
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Lunghezza,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Lunghezza,col=Sesso),method = "lm", size = 1)+
  labs(title="Correlazione Peso-Lunghezza per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#ipotiziamo una modello con crescita quadratica
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Lunghezza,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Lunghezza,col=Sesso),method = "lm", formula = y ~ x + I(1/x), size = 1)+
  labs(title="Correlazione Peso-Lunghezza per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#ipotiziamo una modello con crescita logaritmica
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Lunghezza,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Lunghezza,col=Sesso),method = "lm", formula = y ~ x + log(x), size = 1)+
  labs(title="Correlazione Peso-Lunghezza per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())
#LOGARTIMICA CALZA A PENNELLO

#scatterplot 
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Cranio,y=Peso,col=Sesso))+
  labs(title="Correlazione Peso-Cranio per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#modello lineare con crescita quadratica
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Cranio,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Cranio,col=Sesso),method = "lm", formula = y ~ x + I(1/x), size = 1)+
  labs(title="Correlazione Peso-Cranio per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#mnodello lineare con cresciata logaritmica
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Cranio,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Cranio,col=Sesso),method = "lm", formula = y ~ x + log(x), size = 1)+
  labs(title="Correlazione Peso-Cranio per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#scatterplot
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Gestazione,col=Sesso))+
  labs(title="Correlazione Peso-Gestazione per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#crescita logaritmica
ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Gestazione,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Gestazione,col=Sesso),method = "lm", formula = y ~ x + log(x), size = 1)+
  labs(title="Correlazione Peso-Gestazione per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

ggplot(data = neonati.filtrato)+
  geom_point(aes(x=Peso,y=Gestazione,col=Sesso))+
  stat_smooth(aes(x=Peso,y=Gestazione,col=Sesso),method = "lm", formula = y ~ x + I(1/x), size = 1)+
  labs(title="Correlazione Peso-Gestazione per Sesso")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())

#prima di inizare a creare i modelli dividiamo il dataset in training e test set
neonati.filtrato$id <- 1:nrow(neonati.filtrato)

#use 70% of dataset as training set and 30% as test set 
train <- neonati.filtrato %>% dplyr::sample_frac(0.95)
test  <- dplyr::anti_join(neonati.filtrato, train, by = 'id')

#in tutte queste variabili i dati crescono in modo logaritmico

#ma ce un problema nello studio di queste variabili,aho paura che alcune siano troppo correlate e diano problemi di multicollinearita
#mqa verifichiamolo, costruiamo un modello
mod1<- lm(Peso~
            Anni.madre+
            N.gravidanze+
            Gestazione+
            Lunghezza+
            Cranio+
            Fumatrici+
            Tipo.parto+
            Ospedale+
            Sesso,
          data=train)
summary(mod1)



#sinceramente mi sento di rimuovere da questo modello la variabile ospedale poiche inutile ai fini dello studio
mod2 <- update(mod1,~.-Ospedale)
summary(mod2) #R quadro diminuisce di poco ma per prferiamo un modello piu semplice

#vediamo se la rimozione di osepdale e rilevante

anova(mod1,mod2)#il test rifiuta l'ipotesi nulla, quindi i modelli sono diversi

AIC(mod1,mod2)
BIC(mod1,mod2)
#entrambi i test hanno cambiamenti minimi, quini mi sento di poter rimuvo e ospedale in tranquilita

#test multicollinearita
vif(mod2) #tutti i valori sotto 5 quindi non abbiamo multicollinearita

#allo stato attuale il modello a mio parere sarebbe corretto, sebbene le variabili anni madre e fumatrici vengono escluse dal T test
# io le menterrei come varibili di controllo. Ma per ricurezza faro un modello con meno variabili e vedro se performa meglio
mod3 <- update(mod2,~.-Anni.madre)
summary(mod3)

anova(mod2,mod3)

AIC(mod2,mod3)
BIC(mod2,mod3)
#entrambi i test hanno cambiamenti minimi, quini mi sento di poter rimuvo e ospedale in tranquilita

#test multicollinearita
vif(mod3) #tutti i valori sotto 5 quindi non abbiamo multicollinearita

#anche in questo caso non abbiamo multicollinearita e dai tesi i modelli resultato uguali

#proviamo a togliere fumatrici
mod4 <- update(mod3,~.-Fumatrici)
summary(mod4)
plot(mod4)

anova(mod3,mod4)

AIC(mod3,mod4)
BIC(mod3,mod4)
#entrambi i test hanno cambiamenti minimi, quini mi sento di poter rimuvo e ospedale in tranquilita

#test multicollinearita
vif(mod4) #tutti i valori sotto 5 quindi non abbiamo multicollinearita
  
#da tutti i test ci risulta che la rimuzione di Anni.madre e Fumatrici non fa modificare la qualita del modello
#facciamo un ultimo test, creando un modello in cui evidenziamo che peso,lunghezza,circonferenza e gestazione anno crescita logaritmica
mod5<- lm(I(log(Peso))~
            N.gravidanze+
            I(log(Gestazione))+
            I(log(Lunghezza))+
            I(log(Cranio))+
            Tipo.parto+
            Sesso,
          data=train)
summary(mod5)

#confrontare i modelli con Peso e log(Peso) non e possibile, poiche la variabile target e a una crescita diversa e quindi qavranno segno diverso
#Rquadro del modello pero e migliore per il modello 5

vif(mod5) #test di multicollinearita per sicurezza

mod6 <- update(mod5,~.+Fumatrici)
summary(mod6)

mod7 <- update(mod5,~.+Anni.madre)
summary(mod7)

mod8 <- update(mod5,~.+Fumatrici+Anni.madre)
summary(mod8)

#confrontiamo i modelli con crescita logaritmica che hanno le variabili di controllo fumatrici e Anni.madre

anova(mod5,mod6)#il test accetta ipotesi di uguaglianza
anova(mod5,mod7)#il test accetta ipotesi di uguaglianza
anova(mod5,mod8)#il test accetta ipotesi di uguaglianza
#avere una delle variabili o entrmabe nonc cambia


AIC(mod5,mod6,mod7,mod8)
BIC(mod5,mod6,mod7,mod8)
#entrambe le stime dicono che il modello migliore e il 5

#personalmente intendo tenere la variabile di controllo Fumatrici e Anni madre
#poiche la qualita del modello non cambia di molto e mi sembra una variabile di controllo da mantere

#test per la multicollinearita
vif(mod6) #test di multicollinearita per sicurezza

#vediamo i plot dei residui
par(mfrow=c(2,2))
plot(mod6)

par(mfrow=c(1,1))
plot(mod6, 1, id.n = 5)
plot(mod6, 2, id.n = 5)
plot(mod6, 3, id.n = 5)
plot(mod6, 4, id.n = 5)
plot(mod6, 5, id.n = 5)

#facciamo dei test sui residui

#test di normalita --> ipotes di normalita
shapiro.test(residuals(mod6))#Rifiutiamo il test di nornalita
#quindi i residui non sono perfettamente normali

#test di Omoschedasticità --> ipotesi di omoschedacita
bptest(mod6)#rifiutiamo ipotesi di non omoschedasticita (NOT STONCKS)
#il modello e omoschedastico aka a poca varianza

#test di incorellazione 
dwtest(mod6)#Accetriamo ipoetesi (quindi non ce incorelazione)

#facciamo ora qualche test sui leverage ed outliars

#Funzione per evidenziare gli outliars
outlierTest(mod6)

#per valutare sia leverers che outliars abbiamo la distanza di cook
cook<-cooks.distance(mod6)
# Cook's distance
plot(mod6, 5, id.n = 5)
plot(mod6, 4, id.n = 5)
max(cook)

#dalle analisi notiamo che abbiamo un punto outliars in particolare che influneza il dataset, ovvero la registrazione 1551
#proviamo a toglierla e riseguire il modello

mod9<- lm(log(Peso)~
            N.gravidanze+
            log(Gestazione)+
            log(Lunghezza)+
            log(Cranio)+
            Tipo.parto+
            Sesso,
          data=train)
summary(mod9)

par(mfrow=c(2,2))
plot(mod9,id.n = 10)
#ci sono degli outliars che fatto leva sul modello

#Rieseguiamo i test
#test di normalita --> ipotes di normalita
shapiro.test(residuals(mod9))

#test di Omoschedasticità --> ipotesi di omoschedacita
bptest(mod9)

#test di incorellazione 
dwtest(mod9)

#il modello risulta eterostecastico e non normale

#per valutare sia leverers che outliars abbiamo la distanza di cook
cook<-cooks.distance(mod9)
# Cook's distance
par(mfrow=c(1,1))
plot(mod9, 5, id.n = 5)
plot(mod9, 4, id.n = 5)
max(cook)

#individuiamo valori che hanno distanza di cook molto alta e eliminiamoli
influential <- cook[(cook > (3 * mean(cook, na.rm = TRUE)))]
names_of_influential <- names(influential)
influential

train <- train %>% anti_join(train[names_of_influential,])
#riadestriamo e vediamo i test

#togliamo altri outliars
#per valutare sia leverers che outliars abbiamo la distanza di cook
cook<-cooks.distance(mod9)
influential <- cook[(cook > (10 * mean(cook, na.rm = TRUE)))]
names_of_influential <- names(influential)
influential

train <- train %>% anti_join(train[names_of_influential,])
#riadestriamo e vediamo i test

#finalmente i test son soddisfacenti, ma abbiamo un problema di eteschedasticità

#proviamo ora a cambiare lo standard error con il robust standard error, per risolvere 
coeftest(mod9, vcov = vcovHC(mod9, "HC1"))
#sembra che ci sia un piccolo miglioramento ma nulla di rilevante

#creiamo degli esempi fittizi
testMedian <- data.frame(N.gravidanze = 3,Gestazione=39,Lunghezza=500,Cranio=340,Tipo.parto="Nat",Sesso="F")

testMean <- data.frame(N.gravidanze = 3,Gestazione=39,Lunghezza=494.6958,Cranio=340.0292,Tipo.parto="Nat",Sesso="F")

#facciamo delle predizioni
predMedian = predict(mod9, newdata = testMedian)
predMean = predict(mod9, newdata = testMean)
predictionTest = predict(mod9, newdata = test)

exp(predMedian)
exp(predMean)

#rmse <- sqrt(sum((exp(predictionTest) - test$Peso)^2)/length(test$Peso))
#c(RMSE = rmse, R2=summary(mod9)$r.squared)

#calcoliamo la stima Root Mean Square Error
#che misura l'errore medio eseguito dal modello nel prevedere l'esito di un'osservazione.
rmse(test$Peso, exp(predictionTest))

#calcolimao la stima MAE
#simile al rsme ma è meno influenzata dagli outliars
mae(test$Peso, exp(predictionTest))

#faccimao infine un plot che mostra per ogni regressore (sulle x) la retta con la variabile target sulle y
avPlots(mod9)

