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
install.packages("psych", dependencies = TRUE)
library(psych)

dsCase["PreferAirplane"]

# Love you
mean(dsCase$PreferAirplane, na.rm = TRUE)
dsCase$PreferAirplane[is.na(dsCase$PreferAirplane)] <-
  round(mean(dsCase$PreferAirplane, na.rm=TRUE))


