#####################################################################################
#                                                                                   #
#                       W2453 - Angewandtes Projekt mit R                           #
#                           Aufgabe 3: GARCH-Modelle                                #
#                                                                                   #
#####################################################################################

# Beispiel zur Nutzung des lubridate-Paketes (können Sie löschen, auskommentieren oder so lassen)
  library(lubridate)                # load lubridate package
  time = "15:45:23"                 # example for time (works also if "time is a vector)
  
  seconds = as.numeric(hms(time))   # transform the time-format to seconds
                                    # where 00:00:00 (midnight) is 0 seconds
  hours = seconds/60^2              # transforming seconds in decimal hours
  
  
  
 # Staten Sie Ihren Code hier
  library(fGarch)       # lade fGarch Paket (vorher ggf. installieren)
  #a
  setwd("/Users/lytra/Downloads")
  K <- read.csv("ADS.DEEUR_06.01.2020.csv")
  Kurs = K$Close    # speichere den Kurs
  Day = K$Time  
  seconds = as.numeric(hms(Day))
  hours = seconds/60^2   
  
  n= length(Kurs)
  
  par(mfrow = c(2, 1))        # Öffne Plot mit zwei Zeilen
  plot(hours, Kurs, type = "l", xlab = "Time") 
  title("Aktienpreisreihe Addidas am 6. Januar,2020")
  
  
  K1 <- read.csv("DEU.IDXEUR_06.01.2020.csv")
  Kurs1 = K1$Close    # speichere den Kurs
  Day1 = K1$Time  
  seconds = as.numeric(hms(Day))
  hours = seconds/60^2   
  
  n1= length(Kurs1)
  par(mfrow = c(2, 1))        # Öffne Plot mit zwei Zeilen
  plot(hours, Kurs1, type = "l", xlab = "Time") 
  title("Germany 30 Index am 6. Januar,2020")

  #b
  #Addidas
  
  library(fGarch)       # lade fGarch Paket (vorher ggf. installieren)
  h=length(hours)
  h_=(1:(h-1))
  Rendite = diff(log(Kurs))   # Berechne die log-Rendite
  plot(h_, Rendite, type = "l", xlab = "Time")
  title("Log-Rendite Addidas Aktienkpreis Zeitreihe, 6.Januar 2020")
  g_model =  garchFit(~garch(1, 1), data = Rendite*100, trace = FALSE) 
  g_model                     # Zeige das GARCH(1,1) Modell
  g_SD=g_model@sigma.t
  lines(h_, g_SD, col = 2) # plotte die bed. Standardabweichungen
 
  
  summary(g_model)
  g_model@fit$coef
 
  
  h=length(hours)
  h_=(1:(h-1))
  g_SD=g_model@sigma.t
  plot.ts(g_SD)
  title("Geschätzte bedingte SD (Volatilität) für Addidas Aktienpreis Renditen")
  
  

  #Germany Index
  
  Rendite1=diff(log(Kurs1))  
  plot(h_, Rendite1, type = "l", xlab = "Time")
  title("Log-Rendite Germany Index Zeitreihe,6.Januar 2020")
  g_model1 =  garchFit(~garch(1, 1), data = Rendite1*100, trace = FALSE) 
  g_model1                     # Zeige das GARCH(1,1) Modell
  g_SD1=g_model1@sigma.t
  lines(h_, g_SD1, col = 2) # plotte die bed. Standardabweichungen
  
  
  summary(g_model1)
  g_model1@fit$coef
  
  g_SD1=g_model1@sigma.t
  plot.ts(g_SD1)
  title("Geschätzte bedingte SD (Volatilität) für Germany Index Renditen")
  

  #c
  Wartezeit=diff(Kurs)
gM=  garchFit(~garch(1, 1), data = Wartezeit, trace = FALSE)
gM  
plot(h_,Wartezeit, type = "l", xlab = "Time")
title("Wartezeit Addidas Aktienpreis,6.Januar.2020")

Wartezeit1= diff(Kurs1)  
gM1=garchFit(~garch(1, 1), data = Wartezeit1*100, trace = FALSE)
gM1
plot(h_,Wartezeit1, type = "l", xlab = "Time")
title("Wartezeit Germany Index,6.Januar.2020")



################################################
hist(Wartezeit)
qqnorm(Wartezeit)
qqline(Wartezeit)
hist(Wartezeit1)
qqnorm(Wartezeit1)
qqline(Wartezeit1)
acf(Rendite)
acf(Rendite1)
