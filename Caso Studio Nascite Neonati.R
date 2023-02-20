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
#quindi la media del nostro campione è significativamete diversa da quella della popolazione

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
boxplot(Peso~Sesso)
boxplot(Lunghezza~Sesso)
boxplot(Cranio~Sesso)
boxplot(Gestazione~Sesso)

#test sulla lunghezza
pairwise.t.test(Lunghezza, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

#test sul peso
pairwise.t.test(Peso, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

#test sul cranio
pairwise.t.test(Cranio, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

#test sulle settimane di gestazione
pairwise.t.test(Gestazione, Sesso,
                paired = FALSE,
                pool.sd = TRUE,
                p.adjust.method = "bonferroni")

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

chart.Correlation(neonati,histogram=TRUE, pch=19)






















































































