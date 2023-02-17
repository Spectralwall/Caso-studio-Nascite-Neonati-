#ANALISI NASCITE NEONATI

#Install
install.packages("PerformanceAnalytics")

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

#funzione che genera una tabella contenente tutti gli indici di una variabile
#indici di posizione, variabilità e forma
analisi.quantitative <- function(x,y){
  
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
               color="red", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=quantile(x,seq(0,1,0.25))),
               color="green3", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=median(x)),
               color="orange", linetype="dashed", linewidth=1)+
    geom_vline(aes(xintercept=Mode(x)),
               color="yellow", linetype="dashed", linewidth=1)+
    xlab("Sales")+
    ylab("Density")+
    labs(title = "Distribuzione Sales")+
    theme_fivethirtyeight()
  print("Summary, Range, Range Interquantile, Moda, Variaza, Devizione standard, Coefficente di variazione, Assiemtria, Curtosi")
  return(
    list(
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
  
  
  png("Gini.png", height = 50*nrow(df), width = 100*ncol(df))
  grid.table(data.frame(Gini=c(G(x))))
  dev.off()
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






















































































