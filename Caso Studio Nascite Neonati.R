#ANALISI NASCITE NEONATI

#IMPORT
library(moments)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggthemes)
library(PerformanceAnalytics)

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
  
  ggplot(data=df_freq, aes(x=reorder(row.names(df_freq), +fi), y=fi,fill=row.names(df_freq))) +
    geom_bar(stat="identity")+
    labs(title="Distribuzione in classi",
         x="Classi",
         y="Frequenza")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text())+
    guides(fill=guide_legend(title="Classi"))
}


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






















































































