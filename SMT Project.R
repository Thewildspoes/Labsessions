#------------------------------------------------------------------------------------
# Instellen werkruimte en inlezen van de gegevens 
#------------------------------------------------------------------------------------
# Hier zetten we de working directory vast. Dit moet verandert worden als 
# er een andere laptop gebruikt wordt. De working directories van iedereen worden
# hier neergezet om zo allemaal in hetzelfde bestand te kunnen werken. H
# Vergeet niet te kijken of je de juiste WD aan het staan. Een WD van iemand anders
# kan je uitzetten door voor "SetWD" een # te zetten. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")

setwd("/Users/amaniberkhof/Documents/Labsessions")

setwd("/Users/luliheerkens/Documents/Bedrijfskunde (BA)/practicum S&T/Data")

setwd("C:/Users/Barbara/Documents")


# Hier lees je de file in met de data van de enquete. Dit moet csv2 zijn
# omdat het een ; separated value lijst is en niet komma. 
dsCase <- read.csv2(file = "Data/SMT1920casus01.csv",
                   stringsAsFactors = FALSE)

View(dsCase)

#Hier wordt het psych pakket eenmalig geinstalleerd. Door de installatie
# van dit pakket kan men gemakkelijker functies berekenen die niet automatisch
# in R studio zitten.
# install.packages("psych", dependencies = TRUE)
library(psych)

#------------------------------------------------------------------------------------
# Constructie Likert-Schalen
#------------------------------------------------------------------------------------
# Eerst wordt er een overkoepelende term aangemaakt die Nep01 tot Nep05
# samenvat zodat er niet continu de hele rij herschreven hoeft te worden.
# Vervolgens wordt de Cronbach's Alpha berekend voor Environmental Beliefs (EnvironBelief). 
# Door "Keys = c(...)" wordt er rekening gehouden met de negatief geformuleerde vragen die 
# verandert moeten worden in omgedraaide antwoorden.
EnvironBelief <- c("Nep01", "Nep02", "Nep03", "Nep04", "Nep05")
psych::alpha(dsCase[EnvironBelief], 
             keys = c("Nep01", "Nep05"))

# Hier wordt Cronbach's Alpha berekend voor Guilt Feelings door middel van de 
# overkoepelende term "GuiltFeel". Door "Keys = c(...)" wordt er rekening gehouden met
# de negatief geformuleerde vragen die verandert moeten worden in omgedraaide antwoorden.
GuiltFeel <- c("Guilt01", "Guilt02", "Guilt03", "Guilt04", "Guilt05")
psych::alpha(dsCase[GuiltFeel], 
             keys = c("Guilt03", "Guilt04", "Guilt05"))

# Hier wordt Cronbach's Alpha berekend voor Personality door middel van de 
# overkoepelende term "Personal". Door "Keys = c(...)" wordt er rekening gehouden met
# de negatief geformuleerde vragen die verandert moeten worden in omgedraaide antwoorden.
Personal <- c("Big01", "Big02", "Big03","Big04","Big06", "Big07", "Big08", "Big09")
psych::alpha(dsCase[Personal], 
             keys = c("Big01", "Big03", "Big07", "Big09"))


# Hier bereken je het gemiddelde van de indicator EnvironBelief, vervolgens 
# geeft str(rsltEnvironBelief) aan welke waarden er in rsltEnvironBelief zitten,
# waaronder het gemiddelde.
rsltEnvironBelief <-
  alpha(dsCase[EnvironBelief],
        keys = c("Nep01", "Nep05"),
        cumulative = FALSE)
dsCase$avgEnvironBelief <- rsltEnvironBelief$scores

str(rsltEnvironBelief)

# Hier bereken je het gemiddelde van de indicator GuiltFeel, vervolgens 
# geeft str(rsltGuiltFeel) aan welke waarden er in rsltGuiltFeel zitten, waaronder
# het gemiddelde. 
rsltGuiltFeel <-
  alpha(dsCase[GuiltFeel],
        keys = c("Guilt03", "Guilt04", "Guilt05"),
        cumulative = FALSE)
dsCase$avgGuiltFeel <- rsltGuiltFeel$scores

str(rsltGuiltFeel)

# Hier bereken je het gemiddelde van de indicator Personal, vervolgens 
# geeft str(rsltPersonal) aan welke waarden er in rsltPersonal zitten, waaronder
# het gemiddelde.
rsltPersonal <-
  alpha(dsCase[Personal],
        keys = c("Big01", "Big03", "Big07", "Big09"),
        cumulative = FALSE)
dsCase$avgPersonal <- rsltPersonal$scores

str(rsltPersonal)

#dsCase$sumRisk <- alpha(dsCase[Personal], 
#             keys = c("Big02", "Big04","Big06", "Big08", "Big10"), 
#             cumulative = TRUE)$scores
#dsCase$respGroup <-
#  cut(dsCase$sumRisk,
 #     quantile(dsCase$sumRisk, probs = seq(0,1, by=0.2)),
  #    labels = FALSE)
#
#avg01 <- tapply(dsCase$RISK01,
 #               dsCase$RespGroup, mean, na.rm= TRUE)
#avg02 <- tapply (dsCase$RISK01, dsCase$respGroup, mean, na.rm= TRUE)

#barplot(avg01, main="Mean of RISK01 by group",
   #     ylab = "Mean", xlab = "Respondent group",
    #    las = 1)

#------------------------------------------------------------------------------------
# Beschrijvende Analyse 
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# Analyse Paarsgewijze Samenhangen
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# Doorkruisendheden en Interactie
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# Meervoudige Samenhang
#------------------------------------------------------------------------------------






