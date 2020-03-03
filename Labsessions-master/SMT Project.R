#------------------------------------------------------------------------------------
# Instellen werkruimte en inlezen van gegevens en pakketten
#------------------------------------------------------------------------------------
# Hier zetten we de working directory vast. Dit moet verandert worden als 
# er een andere laptop gebruikt wordt. De working directories van iedereen worden
# hier neergezet om zo allemaal in hetzelfde bestand te kunnen werken. H
# Vergeet niet te kijken of je de juiste WD aan het staan. Een WD van iemand anders
# kan je uitzetten door voor "SetWD" een # te zetten. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")

# setwd("/Users/amaniberkhof/Documents/Labsessions")

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
#install.packages("psych", dependencies = TRUE)
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
#install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

# Hier wordt het lmtest pakket eenmalig geinstalleerd.
# Package lmtest for the durbinWatsonTest function
# install.packages("lmtest", dependencies = TRUE)
library(lmtest)

# Hier wordt het lm.beta pakket eenmalig geinstalleerd.
# Package lm.beta for standardized regression effects
# coefficients with the lm.beta function
# install.packages("lm.beta", dependencies = TRUE)
library(lm.beta)

# Hier wordt het car pakket eenmalig geinstalleerd.
# Package car for type III anova and regression related
# install.packages("car", dependencies = TRUE)
library(car)

#----------------------------------
# CONSTRUCTIE LIKERT-SCHALEN
#----------------------------------
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
  psych::alpha(dsCase[EnvironBelief],
        keys = c("Nep01", "Nep05"),
        cumulative = FALSE)
dsCase$avgEnvironBelief <- rsltEnvironBelief$scores

str(rsltEnvironBelief)

# Hier bereken je het gemiddelde van de indicator GuiltFeel, vervolgens 
# geeft str(rsltGuiltFeel) aan welke waarden er in rsltGuiltFeel zitten, waaronder
# het gemiddelde. 
rsltGuiltFeel <-
  psych::alpha(dsCase[GuiltFeel],
        keys = c("Guilt03", "Guilt04", "Guilt05"),
        cumulative = FALSE)
dsCase$avgGuiltFeel <- rsltGuiltFeel$scores

str(rsltGuiltFeel)

# Hier bereken je het gemiddelde van de indicator Personal, vervolgens 
# geeft str(rsltPersonal) aan welke waarden er in rsltPersonal zitten, waaronder
# het gemiddelde.
rsltPersonal <-
  psych::alpha(dsCase[Personal],
        keys = c("Big01", "Big03", "Big07", "Big09"),
        cumulative = FALSE)
dsCase$avgPersonal <- rsltPersonal$scores

str(rsltPersonal)

#-------------------------------------------------
# BESCHRIJVENDE ANALYSE / UNIVARIAAT KWANTITATIEF
#-------------------------------------------------
# De Tabel wordt hier aangemaakt
tbl <- psych:: describe (dsCase[c("ManipDest", "ManipInfo", "ManipTax", "SchipholTrain", 
                          "SchipholCar", "avgEnvironBelief", "avgGuiltFeel", 
                          "avgPersonal")], skew=FALSE)
print(tbl, digits=3)

# Dit stuk code zorgt ervoor dat de net aangemaakte tabel wordt geexporteert. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")
VarsBA <- c("ManipDest", "ManipInfo", "ManipTax", "SchipholTrain", 
            "SchipholCar", "avgEnvironBelief", "avgGuiltFeel", 
            "avgPersonal")

library(stargazer)
stargazer(dsCase[VarsBA], type="html", out="Tabel_BeschrijvendeAnalyse.doc")

# Hier wordt de mediaan berekent voor ManipDest / ManipInfo / ManipTax / 
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

# Hier worden de waarden van de Modus berekent per kwantitatieve variabele.
ModeManipDest <- table(dsCase$ManipDest)
names(ModeManipDest)[ModeManipDest==max(ModeManipDest)]

ModeManipInfo <- table(dsCase$ManipInfo)
names(ModeManipInfo)[ModeManipInfo==max(ModeManipInfo)]

ModeManipTax <- table(dsCase$ManipTax)
names(ModeManipTax)[ModeManipTax==max(ModeManipTax)]

ModeSchipholTrain <- table(dsCase$SchipholTrain)
names(ModeSchipholTrain)[ModeSchipholTrain==max(ModeSchipholTrain)]

ModeSchipholCar <- table(dsCase$SchipholCar)
names(ModeSchipholCar)[ModeSchipholCar==max(ModeSchipholCar)]

ModeavgEnvironBelief <- table(dsCase$avgEnvironBelief )
names(ModeavgEnvironBelief )[ModeavgEnvironBelief==max(ModeavgEnvironBelief )]

ModeavgGuiltFeel <- table(dsCase$avgGuiltFeel)
names(ModeavgGuiltFeel)[ModeavgGuiltFeel==max(ModeavgGuiltFeel)]

ModeavgPersonal <- table(dsCase$avgPersonal)
names(ModeavgPersonal)[ModeavgPersonal==max(ModeavgPersonal)]

#---------------------------------------
# INTERVALSCHATTING KWANTITATIEVE VARS.
#---------------------------------------
# Aanmaken Alpha
alpha <- 0.05 

# Tabel aanmaken met alle kwantitatieve variabelen
tblInterSchat <- psych::describe(dsCase[c("CO2CompMax", "rateAirplane", "ImportTime", 
                                          "ImportPrice", "ImportComfort", "avgGuiltFeel", 
                                          "avgEnvironBelief", "avgPersonal")],
                          skew = FALSE, ranges = FALSE)

# Alle belangrijke kolommen uit tabel selecteren
tblInterSchat <- tblInterSchat[c(2:4)]

# Berekening van Kritieke Waarden
tblInterSchat$t_crit <- qt(1 - alpha/2, tblInterSchat$n - 1)

# Berekening lower en upper bounds
tblInterSchat$CI_low <- tblInterSchat$mean - tblInterSchat$t_crit*tbl$sd/sqrt(tblInterSchat$n)
tblInterSchat$CI_upp <- tblInterSchat$mean + tblInterSchat$t_crit*tbl$sd/sqrt(tblInterSchat$n)

tblInterSchat

myVars <- c("CO2CompMax", "rateAirplane", "ImportTime", 
            "ImportPrice", "ImportComfort", "avgGuiltFeel", 
            "avgEnvironBelief", "avgPersonal")

library(stargazer)
tblInterSchat <- describe(dsCase[myVars], skew = FALSE, ranges = FALSE)
write.csv2(tblInterSchat, file = "IntervalSchatting_tbl.csv")


#----------------------------------
# UITBIJTERANALYSE
#----------------------------------


#----------------------------------
# ANALYSE PAARSGEWIJZE SAMENHANG
#----------------------------------

#----------------------------------
# KWANTITATIEF
# ---------------------------------
# NA variabelen in ImportTime/ImportComfort/NEP02/
# identificeren en vervangen voor afgerond gemiddelde van die kolom.
colSums(is.na(dsCase))
dsCase$ImportTime[is.na(dsCase$ImportTime)] <-
  round(mean(dsCase$ImportTime, na.rm=TRUE))
dsCase$ImportComfort[is.na(dsCase$ImportComfort)] <-
  round(mean(dsCase$ImportComfort, na.rm=TRUE))
dsCase$Nep02[is.na(dsCase$Nep02)] <-
  round(median(dsCase$avgEnvironBelief, na.rm = TRUE))

# BASEPLOT
# -----------
# Outliers aanmaken van ImportComfort en rateAirplane 
outliersICRA <- dsCase[dsCase$ImportComfort > 80 & dsCase$rateAirplane > 80,
                   c("ImportComfort","rateAirplane")]
outliersICRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportComfort, y=rateAirplane)) +
                ggplot2::geom_point(col="blue") +
                ggplot2::labs(title = "titel") +
                ggplot2::geom_point(data=outliersICRA, shape = 1, stroke = 1.5,
                                     size = 10, colour="red") +
                ggplot2::ylim(0,105) +
                ggplot2::xlim(0,105) +
                ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
                ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("baseplot_1.pdf"))    

# PEARSONS CORRELATIECOËFFICIËNT BEREKEN
# --------------------------------------------------------
# RATEAIRPLANE EN IMPORTCOMFORT
# --------------------------------------------------
# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportComfort, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersICRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_ICRA.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("ImportComfort", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# RATEAIRPLANE EN IMPORTPRICE
# --------------------------------------------------
# Outliers van rateAirplane en ImportPrice
outliersIPRA <- dsCase[dsCase$ImportPrice < 40 & dsCase$rateAirplane < 40,
                       c("ImportPrice","rateAirplane")]
outliersIPRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersIPRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_IPRA.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("ImportPrice", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# RATEAIRPLANE & IMPORTTIME
# --------------------------------------------------
# Outliers aanmaken voor rateAirplane en ImportTime
outliersITRA <- dsCase[dsCase$ImportTime < 40 & dsCase$rateAirplane < 40,
                       c("ImportTime","rateAirplane")]
outliersITRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportTime, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersITRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_ITRA.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("rateAirplane", "ImportPrice"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# RATEAIRPLANE EN NEP (ENVIRONBELIEF)
# --------------------------------------------------
# Outliers aanmaken voor rateAirplane en NEP (EnvironBelief)
outliersEBRA <- dsCase[dsCase$avgEnvironBelief & dsCase$rateAirplane < 40,
                       c("avgEnvironBelief","rateAirplane")]
outliersEBRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgEnvironBelief, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersEBRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,6.5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_EBRA.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgEnvironBelief", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# GUILT (GUILTFEEL) EN NEP (ENVIRONBELIEF)
# --------------------------------------------------
# Outliers aanmaken voor Guilt (GuiltFeel) en NEP (EnvironBelief)

geom_sm

outliersGFEB <- dsCase[dsCase$avgGuiltFeel < 2 & dsCase$avgEnvironBelief < 2,
                       c("avgGuiltFeel","avgEnvironBelief")]
outliersGFEB

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgGuiltFeel, y=avgEnvironBelief)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersGFEB, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,6.5) +
  ggplot2::xlim(0,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_GFEB.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgGuiltFeel", "avgEnvironBelief"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# NEP (ENVIRONBELIEF) EN CO2COMPMAX
# --------------------------------------------------
# Outliers aanmaken voor NEP (EnvironBelief) en CO2CompMax
# Er zijn 10 values van kleiner dan 5 en groter dan 100 die buiten de GGplot liggen
# dit zijn natuurlijk absolute uitbijters.
outliersEBCO2 <- dsCase[dsCase$avgEnvironBelief < 5 & dsCase$CO2CompMax >= 50,
                       c("avgEnvironBelief","CO2CompMax")]
outliersEBCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgEnvironBelief, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersEBCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,7.5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_EBCO2.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgEnvironBelief", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# GUILT (GUILTFEEL) EN CO2COMPMAX
# --------------------------------------------------
# Outliers aanmaken voor GuiltFeel en CO2CompMax

outliersGFCO2 <- dsCase[dsCase$avgGuiltFeel < 5 & dsCase$CO2CompMax >= 50,
                       c("avgGuiltFeel","CO2CompMax")]
outliersGFCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgGuiltFeel, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersGFCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,7) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_GFCO2.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgGuiltFeel", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# BIG (PERSONAL) EN CO2COMPMAX
# --------------------------------------------------
# Outliers aanmaken voor Personal en CO2CompMax

outliersPSCO2 <- dsCase[dsCase$avgPersonal < 6 & dsCase$CO2CompMax > 40,
                       c("avgPersonal","CO2CompMax")]
outliersPSCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgPersonal, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersPSCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(1.9,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_PSCO2.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgPersonal", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# BIG (PERSONAL) EN GUILT (GUITLFEEL)
# --------------------------------------------------
# Outliers aanmaken voor Personal en GuiltFeel

outliersPSGF <- dsCase[dsCase$avgPersonal < 2 & dsCase$avgGuiltFeel < 40,
                       c("avgPersonal","avgGuiltFeel")]
outliersPSGF

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgPersonal, y=avgGuiltFeel)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersPSGF, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0.5,5) +
  ggplot2::xlim(2,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_PSGF.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("avgPersonal", "avgGuiltFeel"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")

# CO2COMPMAX EN IMPORTPRICE 
# --------------------------------------------------
# Outliers aanmaken voor CO2CompMax en ImportPrice

outliersIPCO2 <- dsCase[dsCase$CO2CompMax > 40 & dsCase$ImportPrice < 40,
                       c("CO2CompMax","ImportPrice")]
outliersIPCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliersIPCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,100) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_IPCO2.pdf")) 

# Uitvoeren correlatie
dsSub <- subset(dsCase,
                select=c("ImportPrice", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSub), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSub), type="pearson")


# T-TOETS BEREKENEN
# ------------------------------------------------------------
# Hier wordt een tabel van kengetallen van rateAirplane aangemaakt en een steekproef van
# rateAirplane gedefinieerd door de uitkomsten van de group variabele ManipInfo.
psych::describe(dsCase$rateAirplane, skew = FALSE, ranges = FALSE)
psych::describeBy(dsCase$rateAirplane, group=dsCase$ManipInfo, skew=FALSE, range=FALSE)

# Kengetallen uit steekproef worden in template gezet.
templateRateAir <- psych::describeBy(dsCase$rateAirplane, group=dsCase$ManipInfo,
                  skew=FALSE, range = FALSE)

# template wordt verbonden tot duidelijke tabel.
utils::str(templateRateAir)
rbind(
  data.frame(ManipInfo=1, templateRateAir$"1"),
  data.frame(ManipInfo=2, templateRateAir$"2"))

t.test(dsCase$rateAirplane ~ dsCase$ManipInfo)

# Hier wordt een tabel van kengetallen van ImportTime aangemaakt en een steekproef van
# ImportTime gedefinieerd door de uitkomsten van de group variabele ManipInfo.
psych::describe(dsCase$ImportTime, skew = FALSE, ranges = FALSE)
psych::describeBy(dsCase$ImportTime, group=dsCase$ManipInfo, skew=FALSE, range=FALSE)

# Kengetallen uit steekproef worden in template gezet.
templateImportTime <- psych::describeBy(dsCase$ImportTime, group=dsCase$ManipInfo,
                                     skew=FALSE, range = FALSE)

# template wordt verbonden tot duidelijke tabel.
utils::str(templateImportTime)
rbind(
  data.frame(ManipInfo=1, templateImportTime$"1"),
  data.frame(ManipInfo=2, templateImportTime$"2"))

t.test(dsCase$ImportTime ~ dsCase$ManipInfo)

# ONEWAY ANOVA
# ------------------------------------------------------------
# ManipTax en CO2CompMax
# ------------------------------------------------------------
# ManipTax: one-way tabel met frequenties
tabelMT <- table(dsCase$ManipTax)
tabelMT

cbind(Freq = tabelMT,
      CumFreq = cumsum(tabelMT),
      Perc = 100*tabelMT/sum(tabelMT),
      CumPerc = cumsum(100*tabelMT/sum(tabelMT)))

# Histogram van CO2CompMax met x-as van ManipTax met een xlim van 40 om betere verdeling weer
# te geven zonder extreme outliers. 
ggplot2::ggplot(dsCase, ggplot2::aes(x = CO2CompMax)) +
        ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
        ggplot2::xlab("X-as Naam") +
        ggplot2::facet_grid(~ ManipTax) +
        ggplot2::xlim(0,40)
ggplot2::ggsave(paste0("Histo_CO2MT.pdf")) 

# Boxplot van FacManipTax en CO2CompMax
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacManipTax, y=CO2CompMax, fill = FacManipTax)) +
                ggplot2::geom_boxplot(col = "black") +
                ggplot2::ylab("Y-as Naam") +
                ggplot2::xlab("X-as Naam") +
                ggplot2::ylim(0,40)
ggplot2::ggsave(paste0("Boxplot_CO2MT.pdf")) 

# Groepstatistieken van deelpopulaties CO2CompMax en ManipTax (4.3.3)
psych:: describeBy(dsCase$ManipTax, dsCase$CO2CompMax)

# Histogram
#ggplot2::ggplot(dsCase, ggplot2::aes(x = rateAirplane)) +
  #ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
  #ggplot2::xlab("Intentie om het contract te accepteren") +
  #ggplot2::facet_grid(~ ManipInfo)

#ggplot(dsCase, aes(x = ManipInfo, y=rateAirplane)) +
  #geom_point(col = "black") +
  #ylab("waarschijnlijkheid reizen met vliegtuig") +
  #xlab("aanwezigheid van informatie")

# Boxplot
#ggplot(dsCase, aes(x = ManipInfo, y=rateAirplane,
                #   fill = ManipInfo)) +
 # geom_boxplot(col = "black") +
 # ylab("waarschijnlijkheid reizen met vliegtuig") +
  #xlab("aanwezigheid van informatie")

#ggsave(paste0(dirRslt, "gg_anovaHistogram.pdf"))

# ggplot
#ggplot(dsCase, aes(x =ManipInfo, y=rateAirplane)) +
  #geom_point(col = "black") +
  #ylab("waarschijnlijkheid reizen met vliegtuig") +
 # xlab("aanwezigheid van informatie") +
 # stat_summary(aes(y = rateAirplane, group=1),
              # fun.y=mean, colour="blue", size=1.5,
              # geom="line") +
  #stat_summary(aes(y = rateAirplane, group=1),
               #fun.y=mean, colour="red", size=7, shape = 15,
               #geom="point")

#ManipTax en CO2CompMax
# ------------------------------------------------------------


#ManipInfo en Import
# ------------------------------------------------------------




# Base plot van ImportTime en rateAirplane
# Outliers aanmaken. 
outliers <- dsCase[dsCase$ImportTime < 40 & dsCase$rateAirplane < 40,
                   c("ImportTime","rateAirplane")]
outliers

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportTime, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliers, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# Base plot van ImportPrice en rateAirplane
# Outliers aanmaken. 
outliers <- dsCase[dsCase$ImportPrice < 40 & dsCase$rateAirplane < 40,
                   c("ImportPrice","rateAirplane")]
outliers

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliers, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")

# Base plot van ImportPrice en CO2CompMax DIT LUKT NIET
# Outliers aanmaken. 
outliers <- dsCase[dsCase$ImportPrice < 40 & dsCase$CO2CompMax > 30,
                   c("ImportPrice","CO2CompMax")]
outliers

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "titel") +
  ggplot2::geom_point(data=outliers, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) +
  ggplot2::geom_density_2d(col = "magenta")


#-----------------------------------
# DOORKRUISENDHEDEN EN INTERACTIES
#-----------------------------------
# Lu en Iris nog aan werken. 


#-----------------------
# MEERVOUDIGE SAMENHANG
#-----------------------
# Factoren aanmaken
FacManipInfo <- factor(dsCase$ManipInfo,labels=c("Yes", "No"))
levels(FacManipInfo)

FacManipTax <- factor(dsCase$ManipTax,labels=c("only price flight ticket", 
                                                   "price flight ticket including low tax", 
                                                   "price including high tax"))
levels(FacManipTax)

FacSchipholCar <- factor(dsCase$SchipholCar,labels=c("<30", "30-45", 
                                                           "45-60",">60", "no car"))
levels(FacSchipholCar)

FacSchipholTrain<-factor(dsCase$SchipholTrain, labels=c("<30", "30-45", "45-60",">60"))
levels(FacSchipholTrain)

FacManipDest <- factor(dsCase$ManipDest, labels=c("Berlin", "London", "Marseille"))
levels(FacManipDest)

#------------------------------------------------------------------------------------
# Hier worden de verschillende regressies uitgerekend voor de verklarende variabelen 
# Regressie rateAirplane 
ModelA <- rateAirplane ~ SchipholCar + SchipholTrain + ManipDest + ImportTime + 
  ManipInfo + ImportComfort + ImportPrice + avgEnvironBelief
  lm(ModelA, data=dsCase)
  rsltA<- lm(ModelA, data=dsCase)

# Regressie CO2CompMax
ModelB <- CO2CompMax ~ avgEnvironBelief + avgGuiltFeel + 
  Big01 + Big05 + ImportPrice + ManipTax
  lm(ModelB, data=dsCase)
  rsltC<- lm(ModelB, data=dsCase)

#----------------------------------
# REGRESSIE ANALYSE
#----------------------------------

