#------------------------------------------------------------------------------------
#                                     SMT CASE 2020 
# Amani Berkhof - Lu Li Heerkens - Barbara van Leeuwen - Iris de Ruyter de Wildt
#------------------------------------------------------------------------------------

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

#setwd("C:/Users/Barbara/Documents/Labsessions/Labsessions/Labsessions-master")


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
# install.packages("Hmisc", dependencies = TRUE)
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

# Hier wordt het plyr pakket eenmalig geinstalleerd.
#install.packages("plyr", dependencies = TRUE)
library(plyr)

# Hier wordt het ppcor pakket eenmalig geinstalleerd.
# install.packages("ppcor", dependencies = TRUE)
library(ppcor)

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
             keys = c("Nep02", "Nep03", "Nep04"))

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
               keys = c("Nep02", "Nep03", "Nep04"),
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
# De tabel wordt hier aangemaakt
tblBS <- psych:: describe (dsCase[c("ManipDest", "ManipInfo", "ManipTax", "SchipholTrain", 
                                  "SchipholCar", "avgEnvironBelief", "avgGuiltFeel", 
                                  "avgPersonal")], skew=FALSE)
print(tblBS, digits=3)

# Dit stuk code zorgt ervoor dat de net aangemaakte tabel wordt geexporteert. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")
VarsBA <- c("ManipDest", "ManipInfo", "ManipTax", "SchipholTrain", 
            "SchipholCar", "avgEnvironBelief", "avgGuiltFeel", 
            "avgPersonal")

library(stargazer)
stargazer(dsCase[VarsBA], type="html", out="Tabel_BeschrijvendeAnalyse.doc")

# Factoren aanmaken
FacManipInfo <- factor(dsCase$ManipInfo,labels=c("Yes", "No"))
levels(FacManipInfo)

FacManipTax <- factor(dsCase$ManipTax,labels=c("only price flight ticket", 
                                               "price flight ticket including low tax", 
                                               "price including high tax"))
levels(FacManipTax)

FacManipDest <- factor(dsCase$ManipDest, labels=c("Berlin", "London", "Marseille"))
levels(FacManipDest)

FacSchipholCar <- factor(dsCase$SchipholCar,labels=c("<30", "30-45", 
                                                     "45-60",">60", "no car"))
levels(FacSchipholCar)

FacSchipholTrain<-factor(dsCase$SchipholTrain, labels=c("<30", "30-45", "45-60",">60"))
levels(FacSchipholTrain)

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

# Frequenties van ManipDest / ManipTax / ManipInfo
tblManipDest <- table(dsCase$ManipDest)
round(cbind(Freq = tblManipDest,
            CumFreq = cumsum(tblManipDest),
            RelFreq = 100*tblManipDest/sum(tblManipDest),
            CumRelFreq = 100*cumsum(tblManipDest)/sum(tblManipDest)), 3)

tblManipTax <- table(dsCase$ManipTax)
round(cbind(Freq = tblManipTax,
            CumFreq = cumsum(tblManipTax),
            RelFreq = 100*tblManipTax/sum(tblManipTax),
            CumRelFreq = 100*cumsum(tblManipTax)/sum(tblManipTax)), 3)

tblManipInfo <- table(dsCase$ManipInfo)
round(cbind(Freq = tblManipInfo,
            CumFreq = cumsum(tblManipInfo),
            RelFreq = 100*tblManipInfo/sum(tblManipInfo),
            CumRelFreq = 100*cumsum(tblManipInfo)/sum(tblManipInfo)), 3)

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
# z scores berekenen
zCO2CompMax <- scale(dsCase$CO2CompMax)
which(zCO2CompMax>3)
which(abs(zCO2CompMax)>3)

zrateAirplane <- scale(dsCase$rateAirplane)
which(zrateAirplane>3)
which(abs(zrateAirplane)>3)

zImportComfort <- scale(dsCase$ImportComfort)
which(zImportComfort>3)
which(abs(zImportComfort)>3)

zImportPrice <- scale(dsCase$ImportPrice)
which(zImportPrice>3)
which(abs(zImportPrice)>3)

zImportTime <- scale(dsCase$ImportTime)
which(zImportTime>3)
which(abs(zImportTime)>3)

zavgEnvironBelief <- scale(dsCase$avgEnvironBelief)
which(zavgEnvironBelief>3)
which(abs(zavgEnvironBelief)>3)

zavgGuiltFeel <- scale(dsCase$avgGuiltFeel)
which(zavgGuiltFeel>3)
which(abs(zavgGuiltFeel)>3)

zavgPersonal <- scale(dsCase$avgPersonal)
which(zavgPersonal>3)
which(abs(zavgPersonal)>3)

# Weglaten uitbijters
#----------------------------------
# CO2CompMax
CO2CompMaxBefore <- mean(dsCase$CO2CompMax, na.rm=TRUE)

CO2CompMaxAfter <- mean(dsCase$CO2CompMax[-which(abs(zCO2CompMax)>3)],
                        na.rm=TRUE)

cat("Absolute difference after and before",
    (CO2CompMaxAfter - CO2CompMaxBefore))
cat("Relative difference after and before (in pct)",
    100*(CO2CompMaxAfter - CO2CompMaxBefore)/CO2CompMaxBefore)

# rateAirplane
rateAirplaneBefore <- mean(dsCase$rateAirplane, na.rm=TRUE)

rateAirplaneAfter <- mean(dsCase$rateAirplane[-which(abs(zrateAirplane)>3)],
                          na.rm=TRUE)

cat("Absolute difference after and before",
    (rateAirplaneAfter - rateAirplaneBefore))
cat("Relative difference after and before (in pct)",
    100*(rateAirplaneAfter - rateAirplaneBefore)/rateAirplaneBefore)

# ImportComfort
ImportComfortBefore <- mean(dsCase$ImportComfort, na.rm=TRUE)

ImportComfortAfter <- mean(dsCase$ImportComfort[-which(abs(zImportComfort)>3)],
                           na.rm=TRUE)

cat("Absolute difference after and before",
    (ImportComfortAfter - ImportComfortBefore))
cat("Relative difference after and before (in pct)",
    100*(ImportComfortAfter - ImportComfortBefore)/ImportComfortBefore)


# ImportPrice
ImportPriceBefore <- mean(dsCase$ImportPrice, na.rm=TRUE)

ImportPriceAfter <- mean(dsCase$ImportPrice[-which(abs(zImportPrice)>3)],
                         na.rm=TRUE)

cat("Absolute difference after and before",
    (ImportPriceAfter - ImportPriceBefore))
cat("Relative difference after and before (in pct)",
    100*(ImportPriceAfter - ImportPriceBefore)/ImportPriceBefore)

# ImportTime
ImportTimeBefore <- mean(dsCase$ImportTime, na.rm=TRUE)

ImportTimeAfter <- mean(dsCase$ImportTime[-which(abs(zImportTime)>3)],
                        na.rm=TRUE)

cat("Absolute difference after and before",
    (ImportTimeAfter - ImportTimeBefore))
cat("Relative difference after and before (in pct)",
    100*(ImportTimeAfter - ImportTimeBefore)/ImportTimeBefore)

# avgEnvironBelief
avgEnvironBeliefBefore <- mean(dsCase$avgEnvironBelief, na.rm=TRUE)

avgEnvironBeliefAfter <- mean(dsCase$avgEnvironBelief[-which(abs(zavgEnvironBelief)>3)],
                              na.rm=TRUE)

cat("Absolute difference after and before",
    (avgEnvironBeliefAfter - avgEnvironBeliefBefore))
cat("Relative difference after and before (in pct)",
    100*(avgEnvironBeliefAfter - avgEnvironBeliefBefore)/avgEnvironBeliefBefore)

# avgGuiltFeel
avgGuiltFeelBefore <- mean(dsCase$avgGuiltFeel, na.rm=TRUE)

avgGuiltFeelAfter <- mean(dsCase$avgGuiltFeel[-which(abs(zavgGuiltFeel)>3)],
                          na.rm=TRUE)

cat("Absolute difference after and before",
    (avgGuiltFeelAfter - avgGuiltFeelBefore))
cat("Relative difference after and before (in pct)",
    100*(avgGuiltFeelAfter - avgGuiltFeelBefore)/avgGuiltFeelBefore)

# avgPersonal
avgPersonalBefore <- mean(dsCase$avgPersonal, na.rm=TRUE)

avgPersonalAfter <- mean(dsCase$avgPersonal[-which(abs(zavgPersonal)>3)],
                         na.rm=TRUE)

cat("Absolute difference after and before",
    (avgPersonalAfter - avgPersonalBefore))
cat("Relative difference after and before (in pct)",
    100*(avgPersonalAfter - avgPersonalBefore)/avgPersonalBefore)

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

#-----------------------------------------------------------------------------------------------
# PEARSONS CORRELATIECOËFFICIËNT BEREKENEN
#-----------------------------------------------------------------------------------------------
# RATEAIRPLANE EN IMPORTCOMFORT
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken van ImportComfort en rateAirplane 
outliersICRA <- dsCase[dsCase$ImportComfort > 80 & dsCase$rateAirplane > 80,
                       c("ImportComfort","rateAirplane")]
outliersICRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportComfort, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "ImportComfort / rateAirplane") +
  ggplot2::geom_point(data=outliersICRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_ICRA.pdf")) 

# Uitvoeren correlatie
dsSubICRA <- subset(dsCase,
                    select=c("ImportComfort", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSubICRA), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubICRA), type="pearson")

#-----------------------------------------------------------------------------------------------
# RATEAIRPLANE EN IMPORTPRICE
#-----------------------------------------------------------------------------------------------
# Outliers van rateAirplane en ImportPrice
outliersIPRA <- dsCase[dsCase$ImportPrice < 40 & dsCase$rateAirplane < 40,
                       c("ImportPrice","rateAirplane")]
outliersIPRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "ImportPrice / rateAirplane") +
  ggplot2::geom_point(data=outliersIPRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_IPRA.pdf")) 

# Uitvoeren correlatie
dsSubIPRA <- subset(dsCase,
                    select=c("ImportPrice", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSubIPRA), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubIPRA), type="pearson")

#-----------------------------------------------------------------------------------------------
# RATEAIRPLANE & IMPORTTIME
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor rateAirplane en ImportTime
outliersITRA <- dsCase[dsCase$ImportTime < 40 & dsCase$rateAirplane < 40,
                       c("ImportTime","rateAirplane")]
outliersITRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportTime, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "ImportTime / rateAirplane") +
  ggplot2::geom_point(data=outliersITRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,105) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_ITRA.pdf")) 

# Uitvoeren correlatie
dsSubITRA <- subset(dsCase,
                    select=c("rateAirplane", "ImportPrice"))

Hmisc::rcorr(as.matrix(dsSubITRA), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubITRA), type="pearson")

#-----------------------------------------------------------------------------------------------
# RATEAIRPLANE EN NEP (ENVIRONBELIEF)
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor rateAirplane en NEP (EnvironBelief)
outliersEBRA <- dsCase[dsCase$avgEnvironBelief & dsCase$rateAirplane < 40,
                       c("avgEnvironBelief","rateAirplane")]
outliersEBRA

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgEnvironBelief, y=rateAirplane)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde EnvironBelief / rateAirplane") +
  ggplot2::geom_point(data=outliersEBRA, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,105) +
  ggplot2::xlim(0,6.5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_EBRA.pdf")) 

# Uitvoeren correlatie
dsSubEBRA <- subset(dsCase,
                    select=c("avgEnvironBelief", "rateAirplane"))

Hmisc::rcorr(as.matrix(dsSubEBRA), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubEBRA), type="pearson")

#-----------------------------------------------------------------------------------------------
# GUILT (GUILTFEEL) EN NEP (ENVIRONBELIEF)
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor Guilt (GuiltFeel) en NEP (EnvironBelief)
geom_sm

outliersGFEB <- dsCase[dsCase$avgGuiltFeel < 2 & dsCase$avgEnvironBelief < 2,
                       c("avgGuiltFeel","avgEnvironBelief")]
outliersGFEB

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgGuiltFeel, y=avgEnvironBelief)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde GuiltFeel / Gemiddelde EnvironBelief") +
  ggplot2::geom_point(data=outliersGFEB, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,6.5) +
  ggplot2::xlim(0,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_GFEB.pdf")) 

# Uitvoeren correlatie
dsSubGFEB <- subset(dsCase,
                    select=c("avgGuiltFeel", "avgEnvironBelief"))

Hmisc::rcorr(as.matrix(dsSubGFEB), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubGFEB), type="pearson")

#-----------------------------------------------------------------------------------------------
# NEP (ENVIRONBELIEF) EN CO2COMPMAX
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor NEP (EnvironBelief) en CO2CompMax
outliersEBCO2 <- dsCase[dsCase$avgEnvironBelief < 5 & dsCase$CO2CompMax > 40,
                        c("avgEnvironBelief","CO2CompMax")]
outliersEBCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgEnvironBelief, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde EnvironBelief / CO2CompMax") +
  ggplot2::geom_point(data=outliersEBCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,7.5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_EBCO2.pdf")) 

# Uitvoeren correlatie
dsSubEBCO2 <- subset(dsCase,
                     select=c("avgEnvironBelief", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSubEBCO2), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubEBCO2), type="pearson")

#-----------------------------------------------------------------------------------------------
# GUILT (GUILTFEEL) EN CO2COMPMAX
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor GuiltFeel en CO2CompMax
outliersGFCO2 <- dsCase[dsCase$avgGuiltFeel < 5 & dsCase$CO2CompMax >= 30,
                        c("avgGuiltFeel","CO2CompMax")]
outliersGFCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgGuiltFeel, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde GuiltFeel / CO2CompMax") +
  ggplot2::geom_point(data=outliersGFCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,7) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE)

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_GFCO2.pdf")) 

# Uitvoeren correlatie
dsSubGFCO2 <- subset(dsCase,
                     select=c("avgGuiltFeel", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSubGFCO2), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubGFCO2), type="pearson")

#-----------------------------------------------------------------------------------------------
# BIG (PERSONAL) EN CO2COMPMAX
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor Personal en CO2CompMax
outliersPSCO2 <- dsCase[dsCase$avgPersonal < 6 & dsCase$CO2CompMax > 40,
                        c("avgPersonal","CO2CompMax")]
outliersPSCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgPersonal, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde Personal / CO2CompMax") +
  ggplot2::geom_point(data=outliersPSCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(1.9,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) 

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_PSCO2.pdf")) 

# Uitvoeren correlatie
dsSubPSCO2 <- subset(dsCase,
                     select=c("avgPersonal", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSubPSCO2), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubPSCO2), type="pearson")

#-----------------------------------------------------------------------------------------------
# BIG (PERSONAL) EN GUILT (GUITLFEEL)
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor Personal en GuiltFeel
outliersPSGF <- dsCase[dsCase$avgPersonal < 2 & dsCase$avgGuiltFeel < 40,
                       c("avgPersonal","avgGuiltFeel")]
outliersPSGF

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=avgPersonal, y=avgGuiltFeel)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "Gemiddelde Personal / Gemiddelde GuiltFeel") +
  ggplot2::geom_point(data=outliersPSGF, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0.5,5) +
  ggplot2::xlim(2,5) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) 

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_PSGF.pdf")) 

# Uitvoeren correlatie
dsSubPSGF <- subset(dsCase,
                    select=c("avgPersonal", "avgGuiltFeel"))

Hmisc::rcorr(as.matrix(dsSubPSGF), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubPSGF), type="pearson")

#-----------------------------------------------------------------------------------------------
# CO2COMPMAX EN IMPORTPRICE 
#-----------------------------------------------------------------------------------------------
# Outliers aanmaken voor CO2CompMax en ImportPrice
outliersIPCO2 <- dsCase[dsCase$CO2CompMax > 40 & dsCase$ImportPrice < 40,
                        c("CO2CompMax","ImportPrice")]
outliersIPCO2

# ggplot aanmaken met lijn, outliers en density weergave.
ggplot2::ggplot(dsCase, ggplot2::aes(x=ImportPrice, y=CO2CompMax)) +
  ggplot2::geom_point(col="blue") +
  ggplot2::labs(title = "ImportPrice / CO2CompMax") +
  ggplot2::geom_point(data=outliersIPCO2, shape = 1, stroke = 1.5,
                      size = 10, colour="red") +
  ggplot2::ylim(0,55) +
  ggplot2::xlim(0,100) +
  ggplot2::geom_smooth(method = lm, col = "green", lwd = 1.0, se = FALSE) 

# ggplot opslaan.
ggplot2::ggsave(paste0("Plot_IPCO2.pdf")) 

# Uitvoeren correlatie
dsSubIPCO2 <- subset(dsCase,
                     select=c("ImportPrice", "CO2CompMax"))

Hmisc::rcorr(as.matrix(dsSubIPCO2), type=c("spearman"))
Hmisc::rcorr(as.matrix(dsSubIPCO2), type="pearson")

#-----------------------------------------------------------------------------------------------
# T-TOETS BEREKENEN
#-----------------------------------------------------------------------------------------------

# RATEAIRPLANE EN MANIPINFO
#-----------------------------------------------------------------------------------------------
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

# gelijkheid van varianties berekenen
var.test(dsCase$rateAirplane ~ dsCase$ManipInfo)

# t test uitvoeren
t.test(dsCase$rateAirplane ~ dsCase$ManipInfo)

# Mann-Whitney toets uitvoeren
wilcox.test(dsCase$rateAirplane ~ dsCase$ManipInfo)

#-----------------------------------------------------------------------------------------------
# IMPORTTIME EN MANIPINFO
#-----------------------------------------------------------------------------------------------
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


# gelijkheid van varianties berekenen
var.test(dsCase$ImportTime ~ dsCase$ManipInfo)

# t toets uitvoeren
t.test(dsCase$ImportTime ~ dsCase$ManipInfo)

# Mann-Whitney toets uitvoeren
wilcox.test(dsCase$ImportTime ~ dsCase$ManipInfo)

#-----------------------------------------------------------------------------------------------
# ONEWAY ANOVA
#-----------------------------------------------------------------------------------------------

# MANIPTAX EN CO2COMPMAX
#-----------------------------------------------------------------------------------------------
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
  ggplot2::xlab("CO2CompMax") +
  ggplot2::facet_grid(~ ManipTax) +
  ggplot2::xlim(0,50)
ggplot2::ggsave(paste0("Histo_CO2MT.pdf")) 

# Boxplot van FacManipTax en CO2CompMax
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacManipTax, y=CO2CompMax, fill = FacManipTax)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::ylab("CO2CompMax") +
  ggplot2::xlab("FacManipTax") +
  ggplot2::ylim(0,50)
ggplot2::ggsave(paste0("Boxplot_CO2MT.pdf")) 

# Groepstatistieken van deelpopulaties CO2CompMax en ManipTax
psych:: describeBy(dsCase$ManipTax, dsCase$CO2CompMax)

# Uitvoeren Anova toets
resultANOCO2MT <- aov(dsCase$CO2CompMax ~ dsCase$ManipTax)

summary(resultANOCO2MT)

#-------------------------------------------------------------------------------------------------
# MANIPTAX EN IMPORTPRICE
#-----------------------------------------------------------------------------------------------
# Histogram van ManipTax met x-as van ImportPrice met een xlim van 100 om betere verdeling weer
# te geven zonder extreme outliers. 

ggplot2::ggplot(dsCase, ggplot2::aes(x = ImportPrice)) +
  ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
  ggplot2::xlab("ImportPrice") +
  ggplot2::facet_grid(~ ManipTax) +
  ggplot2::xlim(0,100)
ggplot2::ggsave(paste0("Histo_IPMT.pdf")) 

# Boxplot van FacManipTax en ImportPrice
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacManipTax, y=ImportPrice, fill = FacManipTax)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::ylab("ManipTax") +
  ggplot2::xlab("ImportPrice") +
  ggplot2::ylim(0,100)
ggplot2::ggsave(paste0("Boxplot_IPMT.pdf")) 

# Groepstatistieken van deelpopulaties ImportPrice en ManipTax
psych:: describeBy(dsCase$ManipTax, dsCase$ImportPrice)

# Uitvoeren Anova toets
resultANOIPMT <- aov(dsCase$ImportPrice ~ dsCase$ManipTax)

summary(resultANOIPMT)

#-------------------------------------------------------------------------------------------------
# MANIPDEST EN RATEAIRPLANE
#-----------------------------------------------------------------------------------------------
# ManipDest: one-way tabel met frequenties
tabelMD <- table(dsCase$ManipDest)
tabelMD

cbind(Freq = tabelMD,
      CumFreq = cumsum(tabelMD),
      Perc = 100*tabelMD/sum(tabelMD),
      CumPerc = cumsum(100*tabelMD/sum(tabelMD)))

# Histogram van ManipDest met x-as van rateAirplane met een xlim van 100 en een ylim van 55
# om betere verdeling weer te geven zonder extreme outliers. 
ggplot2::ggplot(dsCase, ggplot2::aes(x = rateAirplane)) +
  ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::facet_grid(~ ManipDest) +
  ggplot2::xlim(0,100) +
  ggplot2::ylim(0,55)
ggplot2::ggsave(paste0("Histo_RAMD.pdf")) 

# Boxplot van FacManipDest en rateAirplane
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacManipDest, y=rateAirplane, fill = FacManipDest)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::ylab("ManipDest") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::ylim(0,100)
ggplot2::ggsave(paste0("Boxplot_RAMD.pdf")) 

# Groepstatistieken van deelpopulaties rateAirplane en ManipDest
psych:: describeBy(dsCase$ManipDest, dsCase$rateAirplane)

# Uitvoeren Anova toets
resultANORAMD <- aov(dsCase$rateAirplane ~ dsCase$ManipDest)

summary(resultANORAMD)

#-------------------------------------------------------------------------------------------------
# SCHIPHOLTRAIN EN RATEAIRPLANE
#-----------------------------------------------------------------------------------------------
# SchipholTrain: one-way tabel met frequenties
tabelST <- table(dsCase$SchipholTrain)
tabelST

cbind(Freq = tabelST,
      CumFreq = cumsum(tabelST),
      Perc = 100*tabelST/sum(tabelST),
      CumPerc = cumsum(100*tabelST/sum(tabelST)))

# Histogram van SchipholTrain met x-as van rateAirplane met een xlim van 100 en een ylim van 45
# om betere verdeling weer te geven zonder extreme outliers. 
ggplot2::ggplot(dsCase, ggplot2::aes(x = rateAirplane)) +
  ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::facet_grid(~ SchipholTrain) +
  ggplot2::xlim(0,100) +
  ggplot2::ylim(0,45)
ggplot2::ggsave(paste0("Histo_RAST.pdf")) 

# Boxplot van FacSchipholTrain en rateAirplane
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacSchipholTrain, y=rateAirplane, fill = FacSchipholTrain)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::ylab("SchipholTrain") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::ylim(0,100)
ggplot2::ggsave(paste0("Boxplot_RAST.pdf")) 

# Groepstatistieken van deelpopulaties SchipholTrain en rateAirplane
psych:: describeBy(dsCase$SchipholTrain, dsCase$rateAirplane)

# Uitvoeren Anova toets
resultANORAST <- aov(dsCase$rateAirplane ~ dsCase$SchipholTrain)

summary(resultANORAST)

#-------------------------------------------------------------------------------------------------
# SCHIPHOLCAR EN RATEAIRPLANE
#------------------------------------------------------------------------------------------------
# SchipholCar: one-way tabel met frequenties
tabelSC <- table(dsCase$SchipholCar)
tabelSC

cbind(Freq = tabelSC,
      CumFreq = cumsum(tabelSC),
      Perc = 100*tabelSC/sum(tabelSC),
      CumPerc = cumsum(100*tabelSC/sum(tabelSC)))

# Histogram van SchipholCar met x-as van rateAirplane met een xlim van 100 en een ylim van 45
# om betere verdeling weer te geven zonder extreme outliers. 
ggplot2::ggplot(dsCase, ggplot2::aes(x = rateAirplane)) +
  ggplot2::geom_histogram(bins=15, fill = "darkgreen", col = "black") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::facet_grid(~ SchipholCar) +
  ggplot2::xlim(0,100) +
  ggplot2::ylim(0,45)
ggplot2::ggsave(paste0("Histo_RASC.pdf")) 

# Boxplot van FacSchipholCar en rateAirplane
ggplot2::ggplot(dsCase, ggplot2::aes(x = FacSchipholCar, y=rateAirplane, fill = FacSchipholCar)) +
  ggplot2::geom_boxplot(col = "black") +
  ggplot2::ylab("SchipholCar") +
  ggplot2::xlab("rateAirplane") +
  ggplot2::ylim(0,100)
ggplot2::ggsave(paste0("Boxplot_RASC.pdf")) 

# Groepstatistieken van deelpopulaties SchipholTrain en rateAirplane (4.3.3)
psych:: describeBy(dsCase$SchipholCar, dsCase$rateAirplane)

# Uitvoeren Anova toets
resultANORASC <- aov(dsCase$rateAirplane ~ dsCase$SchipholCar)

summary(resultANORASC)

#-----------------------------------------------------------------------------------------------
# KRUISTABEL
#-----------------------------------------------------------------------------------------------

# SCHIPHOLCAR EN SCHIPHOLTRAIN
#-----------------------------------------------------------------------------------------------
# Hier wordt de kruistabel aangemaakt van SchipholCar en SchipholTrain.
tabelSCST <- table(dsCase$SchipholCar, dsCase$SchipholTrain)
addmargins(tabelSCST)

chisq.test(tabelSCST)
rsltChisqSCST<-chisq.test(tabelSCST)
str(rsltChisqSCST)

rsltChisqSCST$statistic
rsltChisqSCST$parameter
rsltChisqSCST$p.value

cbind(ChisqStat = rsltChisqSCST$statistic, 
      df        = rsltChisqSCST$parameter, 
      ChisqSig  = rsltChisqSCST$p.value)

#-----------------------------------------------------------------------------------------------
# DOORKRUISENDHEDEN EN INTERACTIES
#-----------------------------------------------------------------------------------------------
# TWO-WAY ANOVA
#-----------------------------------------------------------------------------------------------
# Two-Way Anova SchipholCar, rateAirplane --> ManipTax
anova(aov(rateAirplane ~ FacSchipholCar * FacManipTax, data = dsCase))
anova(aov(rateAirplane ~ FacSchipholCar + FacManipTax, data = dsCase))

# Type III Anova
rsltAOV <- aov(rateAirplane ~ FacSchipholCar * FacManipTax,
               data=dsCase)

Anova(rsltAOV,type=c("III"))
summary.lm(rsltAOV)
summary.lm(rsltAOV)$fstatistic

# ANOVA MET COVARIAAT
#-----------------------------------------------------------------------------------------------
# GuiltFeel
summary(aov(avgGuiltFeel ~ avgEnvironBelief,data=dsCase))
summary(aov(CO2CompMax ~ avgEnvironBelief, data=dsCase))
rcorr(as.matrix(dsCase[c("avgGuiltFeel","CO2CompMax")]))

rslt1AOVCOBefore <- aov(avgGuiltFeel ~ avgEnvironBelief,data=dsCase)
rslt1AOVCOAfter <- aov(CO2CompMax ~ avgEnvironBelief, data=dsCase)

# Resultaten voor en na purgen covariaat EnvironBelief
Anova(rslt1AOVCOBefore,type=c("III"))
Anova(rslt1AOVCOAfter,type=c("III"))

# BIG Extraversion (Personal)
summary(aov(avgPersonal ~ avgGuiltFeel,data=dsCase))
summary(aov(CO2CompMax ~ avgGuiltFeel, data=dsCase))
rcorr(as.matrix(dsCase[c("avgPersonal","CO2CompMax")]))

rslt2AOVCOBefore <- aov(avgPersonal ~ avgGuiltFeel,data=dsCase)
rslt2AOVCOAfter <- aov(CO2CompMax ~ avgGuiltFeel, data=dsCase)

# Resultaten voor en na purgen covariaat GuiltFeel
Anova(rslt2AOVCOBefore,type=c("III"))
Anova(rslt2AOVCOAfter,type=c("III"))

# ANALYSE VAN DEELPOPULATIES
#-----------------------------------------------------------------------------------------------
# Voor purgen kwantitatieve variabelen:
rcorr(as.matrix(dsCase[c("ImportTime","rateAirplane")]))

# Na purgen kwantitatieve variabelen:
by(dsCase[c("ImportTime","rateAirplane")],
   FacManipInfo,
   function(x) rcorr(as.matrix(x)))

# PEARSONS PARTIELE CORRELATIE
#-----------------------------------------------------------------------------------------------
# Uitvoeren partiële correlatie
dsSubDKH <- subset(dsCase,
                   select=c("avgGuiltFeel", "CO2CompMax", "avgPersonal"))

ppcor::pcor.test(dsCase$avgGuiltFeel, dsCase$CO2CompMax, dsCase$avgPersonal)

Hmisc::rcorr(as.matrix(dsSubDKH), type="pearson")

Hmisc::rcorr(as.matrix(dsSubDKH))

#-----------------------------------------------------------------------------------------------
# MEERVOUDIGE SAMENHANG
#-----------------------------------------------------------------------------------------------
# Factoren ophalen uit beschrijvende analyse. 
levels(FacManipDest)
levels(FacManipInfo)
levels(FacManipTax)
levels(FacSchipholCar)
levels(FacSchipholTrain)

#------------------------------------------------------------------------------------
# Hier worden de verschillende regressies uitgerekend voor de verklarende variabelen 
# Regressie rateAirplane 
ModelA <- rateAirplane ~ SchipholCar + SchipholTrain + ManipDest + ImportTime + 
  ManipInfo + ImportComfort + ImportPrice + avgEnvironBelief
lm(ModelA, data=dsCase)

rsltA<- lm(ModelA, data=dsCase)

summary(rsltA)
vif(rsltA)
1 /vif(rsltA) 

# Regressie CO2CompMax
ModelB <- CO2CompMax ~ avgEnvironBelief + avgGuiltFeel + 
  avgPersonal + ImportPrice + ManipTax
lm(ModelB, data=dsCase)
rsltB<- lm(ModelB, data=dsCase)

summary(rsltB)

#----------------------------------------------------------------------------------------------
# Regressiemodel RateAirplane
mdlA <- rateAirplane ~ FacSchipholCar + FacSchipholTrain +
  ImportTime + FacManipInfo + ImportComfort + 
  ImportPrice + avgPersonal + FacManipDest


