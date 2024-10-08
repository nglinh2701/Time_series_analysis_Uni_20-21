#####################################################################################
#                                                                                   #
#                       W2453 - Angewandtes Projekt mit R                           #
#           Aufgabe 1. Trendschätzung und die Anpassung von ARMA-Prozessen          #
#                                                                                   #
#####################################################################################
#b
setwd("/Users/ngoclinh/Downloads/Zeitreihen")
output <- read.csv("output.productivity.csv")
O =output$productivity
n=length(O)
Jahr=(0:(n-1))/n*29+1991
plot.ts(Jahr,O, type="l")

title("Output per employed person in Germany from January 1991 to September 2019  ")
acf(O)




#c
library(smoots)
est = msmooth(O)        # Nichtparametrische Trendschaetzung
est$b0                    # Optimale Bandbreite
trend.est = est$ye       # Die geschaetzten Werte des Trends
resid = est$res          # Die Residuen
resid = O - trend.est  

## Plot der Daten mit dem geschaetztem Trend
par(mfrow = c(2, 1))
plot.ts(O, xlab = "Beobachtungszeitpunkt", ylab = expression(X[t]))
lines(trend.est, col = "red")
plot.ts(resid, xlab = "Beobachtungszeitpunkt", ylab = expression(Y[t]))

## Analyse der ACF der Residuen
acf(resid)                # -> Mehr als 5% der vertikalen Linien ueberschreiten die
# blauen Linien. -> Autokorrelation in den Residuen.

####
#d
library(smoots)
AIC = matrix(0, 4, 4)							### Festlegen der AIC Matrix, 4 Zeilen, 4 Spalten
for(i in 0:3) {	  							### Schleife f�r die AR-Ordnungen p
  for(j in 0:3)	{   						### Schleife f�r die MA-Ordnungen q
    Modell = arima(O, order=c(i, 0, j))	### Sch�tzung des Modelles f�r p = 1, q = j 
    AIC[i+1, j+1]=Modell$aic		### Schreibe die AIC Werte in die Matrix
  }
}

AIC	            								### Ausgabe der Tabelle
min(AIC)					         			### Anzeige des kleinstes Wertes der Tabelle



BIC = matrix(0, 4, 4)						### Festlegen der AIC Matrix, 4 Zeilen, 4 Spalten
for(i in 0:3) {		      				### Schleife f�r die AR-Ordnungen p
  for(j in 0:3) {				      	### Schleife f�r die MA-Ordnungen q 
    Modell2=arima(O, order=c(i, 0, j))				### Sch�tzung des Modelles f�r p = i, q = j
    BIC[i+1, j+1]=-2*Modell2$loglik+log(length(O))*(i+j+2)-log(length(O))*2		### Berechne und schreibe BIC Werte in Matrix
  }
}

BIC	            								### Ausgabe der Tabelle
min(BIC)				        				### Anzeige des kleinstes Wertes der Tabelle

Modell.opt=arima(O, order=c(1, 0, 3))	### Gew�hltes Modell nach AIC und BIC wird erneut gesch�tzt
Modell.opt                      ### Zeige optimales Modell an



