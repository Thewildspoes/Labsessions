#------------------------------------------------------------------------------------
# Instellen werkruimte en inlezen van gegevens en pakketten
#------------------------------------------------------------------------------------
# Hier zetten we de working directory vast. Dit moet verandert worden als 
# er een andere laptop gebruikt wordt. De working directories van iedereen worden
# hier neergezet om zo allemaal in hetzelfde bestand te kunnen werken. H
# Vergeet niet te kijken of je de juiste WD aan het staan. Een WD van iemand anders
# kan je uitzetten door voor "SetWD" een # te zetten. 
#setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")

setwd("/Users/amaniberkhof/Documents/Labsessions")

#setwd("/Users/luliheerkens/Documents/Bedrijfskunde (BA)/practicum S&T/Data")

#setwd("C:/Users/Barbara/Documents")


# Hier lees je de file in met de data van de enquete. Dit moet csv2 zijn
# omdat het een ; separated value lijst is en niet komma. 
dsCase <- read.csv2(file = "Data/SMT1920casus01.csv",
                   stringsAsFactors = FALSE)

View(dsCase)

#Hier wordt het psych pakket eenmalig geinstalleerd. Door de installatie
# van dit pakket kan men gemakkelijker functies berekenen die niet automatisch
# in R studio zitten. # moet weg voor "install.packages" wanneer je het pakket nog
# moet instaleren. 
# install.packages("psych", dependencies = TRUE)
library(psych)

# Hier wordt het ggplot2 pakket eenmalig geinstalleerd.
# Wordt gebruikt om graphische weergaven te kunnen maken.
# "#" moet weg voor "install.packages" wanneer je het pakket nog moet instaleren.
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Hier wordt het stargazer pakket eenmalig geinstalleerd.
# "#" moet weg voor "install.packages" wanneer je het pakket nog moet instaleren.
# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

# Hier wordt het gmodels pakket eenmalig geinstalleerd, wordt gebruikt voor
# contingency models.
#  "#" moet weg voor "install.packages" wanneer je het pakket nog moet instaleren.
# install.packages("gmodels", dependencies = TRUE)
library(gmodels)

# Hier wordt het Hmisc pakket eenmalig geinstalleerd.
# Wordt gebruikt voor correlatie test informatie.
# "#" moet weg voor "install.packages" wanneer je het pakket nog moet instaleren.
# install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

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


#------------------------------------------------------------------------------------
# Beschrijvende Analyse 
#------------------------------------------------------------------------------------
# Hier wordt de mean voor ManipDest / ManipInfo / ManipTax / Schiphol Train en
# SchipholCar gegeven.  
mean(dsCase$ManipDest)
mean(dsCase$ManipInfo)
mean(dsCase$ManipTax)
mean(dsCase$SchipholTrain)
mean(dsCase$SchipholCar)

# Hier wordt het gemiddelde (de Mean) berekend van alle items binnen 
# environmental beliefs.
rsltEnvironBelief <-
  alpha(dsCase[EnvironBelief],
        keys = c("Nep01", "Nep05"),
        cumulative = FALSE)
dsCase$avgEnvironBelief <- rsltEnvironBelief$scores
str(rsltEnvironBelief)

# Hier wordt het gemiddelde (de Mean) berekend van alle items binnen 
# Guilt Feelings dus Schuldgevoel. 
rsltGuiltFeel <-
  alpha(dsCase[Personal],
        keys = c("Guilt03", "Guilt04", "Guilt05"),
        cumulative = FALSE)
dsCase$avgGuiltFeel <- rsltGuiltFeel$scores
str(rsltGuiltFeel)

# Hier wordt het gemiddelde (de Mean) berekend van alle items binnen 
# Personality dus Persoonlijkheid met uitzondering van Big05 en Big10 omdat 
# Die eerder geelimineerd zijn. 
rsltPersonal <-
  alpha(dsCase[Personal],
        keys = c("Big01", "Big03", "Big07", "Big09"),
        cumulative = FALSE)
dsCase$avgPersonal <- rsltPersonal$scores
str(rsltPersonal)

# Hier wordt de standaard deviatie berekent voor ManipDest / ManipInfo / ManipTax / 
# Schiphol Train en SchipholCar / Alle items van NEP / Alle items van GuiltFeel
# en alle items van Personality.
sd(dsCase$ManipDest)
sd(dsCase$ManipInfo)
sd(dsCase$ManipTax)
sd(dsCase$SchipholTrain)
sd(dsCase$SchipholCar)
sd(dsCase$Nep01, dsCase$Nep02, dsCase$Nep03, dsCase$Nep04, dsCase$Nep05,
       na.rm = TRUE)
sd(dsCase$Guilt01, dsCase$Guilt02, dsCase$Guilt03, dsCase$Guilt04, 
       dsCase$Guilt05, na.rm = TRUE)
sd(dsCase$Big01, dsCase$Big02, dsCase$Big03, dsCase$Big04, dsCase$Big06,
       dsCase$Big07, dsCase$Big08, dsCase$Big09, na.rm = TRUE)


# Hier wordt de smediaan berekent voor ManipDest / ManipInfo / ManipTax / 
# Schiphol Train / SchipholCar / Alle items van NEP / Alle items van GuiltFeel
# en alle items van Personality.
median(dsCase$ManipDest)
median(dsCase$ManipInfo)
median(dsCase$ManipTax)
median(dsCase$SchipholTrain)
median(dsCase$SchipholCar)
median(dsCase$Nep01, dsCase$Nep02, dsCase$Nep03, dsCase$Nep04, dsCase$Nep05,
       na.rm = TRUE)
median(dsCase$Guilt01, dsCase$Guilt02, dsCase$Guilt03, dsCase$Guilt04, 
       dsCase$Guilt05, na.rm = TRUE)
median(dsCase$Big01, dsCase$Big02, dsCase$Big03, dsCase$Big04, dsCase$Big06,
       dsCase$Big07, dsCase$Big08, dsCase$Big09, na.rm = TRUE)

# Hier kan je de N vinden van ManipDest / ManipInfo / ManipTax / 
# Schiphol Train en SchipholCar.
describe(dsCase$ManipDest)
describe(dsCase$ManipInfo)
describe(dsCase$ManipTax)
describe(dsCase$SchipholTrain)
describe(dsCase$SchipholCar)
#------------------------------------------------------------------------------------
# Analyse Paarsgewijze Samenhangen
#------------------------------------------------------------------------------------

#Kwantitatief
---------------------------------------------------------------------------------
  
#ImportComfort en RateAirplane 
-------------------------------------------------------------------------------------
# Base plot
  ggplot(dsCase, aes(x=rateTrain, y=rateAirplane)) +
  geom_point(col="blue") + ylab("Likelihood to take the airplane (rateAirplane)") +
  xlab("Comfort is the most import element (rateTrain)")
  
  
#------------------------------------------------------------------------------------
# Doorkruisendheden en Interactie
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# Meervoudige Samenhang
#------------------------------------------------------------------------------------






