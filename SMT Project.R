# hier zetten we de working directory vast. Dit moet verandert worden als 
# er een andere laptop gebruikt wordt. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")

# Hier lees je de file in met de data van de enquete. Dit moet csv2 zijn
# omdat het een ; separated value lijst is en niet komma. 
dsCase <- read.csv2(file = "Data/SMT1920casus01.csv",
                   stringsAsFactors = FALSE)

#Hier wordt het psych pakket eenmalig geinstalleerd. Door de installatie
# van dit pakket kan men gemakkelijker functies berekenen die niet automatisch
# in R studio zitten.
# install.packages("psych", dependencies = TRUE)
library(psych)

# dsCase["PreferAirplane"]
# Checken hoeveel NA cases er zijn in PreferAirplane. 
# colSums(is.na(dsCase, PreferAirplane))

pref <- c("PreferTrain", "PreferAirplane")
# door middel van deze functie kan de Cronbach Alpha worden berekend. 
psych::alpha(dsCase[pref])

