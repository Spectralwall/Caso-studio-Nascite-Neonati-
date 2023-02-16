#ANALISI NASCITE NEONATI

#IMPORT
library(moments)
library(ggplot2)

#FUNZIONI

#Funzione che calcola la moda/classe modale
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#funzione che fa tutti le analisi di base di una variabile
AnalisiGenelare <- function(x){
  
  su
  
  return(
    list(
      summary(x),#summary
      mean(x),#media
      sd(x),#deviazione standard
      var(x),#varianza
      max(x)-min(x),#range
      IQR(x),#range interquantile
      skewness(x),#assimetria
      kurtosis(x)-3#curtosi
    )
  )
  
}

#import dataset
setwd("C:\\Users\\gabri\\Desktop\\PrivateProject\\Caso-studio-Nascite-Neonati-")

neonati <- read.csv("neonati.csv",stringsAsFactors = T)

attach(neonati)
head(neonati)
summary(neonati)
detach(neonati)

table(neonati$Anni.madre)

#rimuoviamo dal dataset tutte le registrazioni in cui gli anni della madre sono minori di 12
#di solito la possibilià di rimanere incinete avviene con il primo ciclo che è intorno al 12 anno di età
neonati.filtrato <- subset(neonati,Anni.madre>=12)
attach(neonati.filtrato)

table(neonati.filtrato$Anni.madre)

#Analisi variabili
























































































