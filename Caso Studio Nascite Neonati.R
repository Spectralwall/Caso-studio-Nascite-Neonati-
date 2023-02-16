#ANALISI NASCITE NEONATI

#IMPORT
library(moments)
library(ggplot2)
library(grid)
library(ggthemes)

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

#funzione che genera una tabella contenente tutti gli indici di una variabile
#indici di posizione, variabilità e forma
AnalisiGenelare <- function(x,y){
  
  df=as.data.frame(cbind(summary(x)))
  colnames(df)<-c(y)
  df2=data.frame(summary=c(max(x)-min(x),IQR(x),Mode(x),var(x),
                              sd(x),CV(x),skewness(x),kurtosis(x)-3))
  rownames(df2) <- c("Range","IQR","Mode","Var","SD","CV","Asymmetry","Curtosi")
  colnames(df2)<-c(y)
  df_all = rbind(df,df2)
  png("report.png", height = 30*nrow(df_all), width = 200*ncol(df_all))
  grid.table(df_all)
  dev.off()
  
  ggplot()+
    geom_density(aes(x=x),col="darkblue",fill="lightblue")+
    geom_vline(aes(xintercept=mean(x)),
               color="red", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=quantile(x,seq(0,1,0.25))),
               color="green3", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=median(x)),
               color="orange", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=Mode(x)),
               color="yellow", linetype="dashed", size=1)+
    xlab("Sales")+
    ylab("Density")+
    labs(title = "Distribuzione Sales")+
    theme_fivethirtyeight()
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
























































































