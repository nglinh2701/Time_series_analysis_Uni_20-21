#####################################################################################
#                                                                                   #
#                       W2453 - Angewandtes Projekt mit R                           #
#                         Aufgabe 2: Finanzmarkanalyse                              #
#                                                                                   #
#####################################################################################


# Geben Sie fuer das Objekt 'MatrNr' in Zeile 8 die Matrikelnummer
# eines Ihrer Gruppenmitglieder ein. Markieren Sie die Zeilen 8 bis 15 
# und fuehren Sie diese zusammen aus.

MatrNr = 6822695
# Geben Sie hier Ihre Matrikelnummer ein!
# Die Nummer eines einzelnen Gruppenmitglieds 
# genuegt.
setwd("/Users/lytra/Desktop/Ordner+Projekt-20191218/W2453 - Projekt 2019 Gruppe XXX")

source("W2453 - choose_data - nicht bearbeiten.txt")
daten = choose_data(MatrNr) 
U1 = daten[[1]]             # Daten fuer das erste Unternehmen
U2 = daten[[2]]             # Daten fuer das zweite Unternehmen
rm("daten", "choose_data")  # Löscht nicht mehr benötigte Objekte

U1 #CANON
U2 #WALMART 

# Nach der Ausfuehrung des Codes finden Sie in der Konsole einen Hinweis, welche Daten 
# fuer Ihre Projektgruppe zu bearbeiten sind.

# In dem Objekt 'U1' befinden sich nun die Finanzdaten des erst-
# genannten Unternehmens, in 'U2' hingegen die des zweiten 
# Unternehmens.

#-------------------------------------------------------------------
# Beginnen Sie hier mit Ihrem eigenen Code:
kurs1=U1$Close

kurs2=U2$Close

n1=length(kurs1)

n2=length(kurs2)
Rendite1=diff(log(kurs1))
Rendite2=diff(log(kurs2))

par(mfrow = c(2,1))	
Jahr = (0:(n1-1))/n1*5+2014
plot(Jahr, kurs1,type="l")
title("Canon Aktienkurs: Mai 2014 bis April 2018")		

Jahr = (0:(n1-2))/(n1-1)*5+2014
plot(Jahr, Rendite1, type = "l")
title("Canon - Renditen: Mai 2014 bis April 2018")

par(mfrow = c(2,1))	
Jahr = (0:(n2-1))/n2*5+2014
plot(Jahr, kurs2,type="l")
title("Walmart Aktienkurs: Mai 2014 bis April 2018")		

Jahr = (0:(n2-2))/(n2-1)*5+2014
plot(Jahr, Rendite2, type = "l")
title("Walmart - Renditen: Mai 2014 bis April 2018")

#b
acf(Rendite1)
acf(Rendite1**2)


acf(Rendite2)
acf(Rendite2**2)

#c
mean(Rendite1)

sd(Rendite1)
sd(Rendite1)**2


library(TSA)							      # Aufrufen des TSA Pakets (muss vorher installiert worden sein)					
skewness(Rendite1)		      # Schiefe der Renditen
kurtosis(Rendite1)					# Kurtosis der Renditen
hist(Rendite1)							# Histogramm der Renditen
qqnorm(Rendite1)						# QQ Normal Plot der Renditen
qqline(Rendite1)						# QQ line der Renditen

mean(Rendite2)
sd(Rendite2)
sd(Rendite2)**2

library(TSA)							      # Aufrufen des TSA Pakets (muss vorher installiert worden sein)					
skewness(Rendite2)		      # Schiefe der Renditen
kurtosis(Rendite2)					# Kurtosis der Renditen
hist(Rendite2)							# Histogramm der Renditen
qqnorm(Rendite2)						# QQ Normal Plot der Renditen
qqline(Rendite1)						# QQ line der Renditen

#-------------------------------------------------------------------
