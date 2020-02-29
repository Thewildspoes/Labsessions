#------------------------------------------------------------------------------------
# Instellen werkruimte en inlezen van gegevens en pakketten
#------------------------------------------------------------------------------------
# Hier zetten we de working directory vast. Dit moet verandert worden als 
# er een andere laptop gebruikt wordt. De working directories van iedereen worden
# hier neergezet om zo allemaal in hetzelfde bestand te kunnen werken. H
# Vergeet niet te kijken of je de juiste WD aan het staan. Een WD van iemand anders
# kan je uitzetten door voor "SetWD" een # te zetten. 
setwd("/Users/irisderuyterdewildt/Desktop/EUR/SMT/Labsessions")

#setwd("/Users/amaniberkhof/Documents/Labsessions")

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


#------------------------------------------------------------------------------------
# Beschrijvende Analyse 
#------------------------------------------------------------------------------------
# de Tabel wordt hier aangemaakt
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

# In dit stuk code wordt een intervalschatting gemaakt voor de kwantitatieve var.
# Hier worden alle kwantitatieve variabelen gepakt
tbl <- tbl[, c(2:4)]
alpha <- 0.05
tbl$t_crit <- qt(1 - alpha/2, tbl$n - 1)

# De upper en lower bound worden aangemaakt.
tbl$CI_low <- tbl$mean - tbl$t_crit*tbl$sd/sqrt(tbl$n)
tbl$CI_upp <- tbl$mean + tbl$t_crit*tbl$sd/sqrt(tbl$n)


#De tabel wordt geexporteerd als CSV file. 
library(stargazer)
tbl <- psych::describe(dsCase[VarsBA], skew = FALSE, ranges = FALSE)
write.csv2(tbl, file = "Interval_BA.csv")

# In het volgende stuk code wordt de uitbijteranalyse opgezet.


#------------------------------------------------------------------------------------
# Analyse Paarsgewijze Samenhangen
#------------------------------------------------------------------------------------

# Kwantitatief
-------------------------------------------------------------------------------------
  
# Hier wordt wat gedaan met ImportComfort en RateAirplane  ????????????????????????????????????
-------------------------------------------------------------------------------------
# Base plot van ImportPrice en rateAirplane
# Deze gekke modus berekening uit de bschrijvende analyse doet het niet maar zorgt ervoor
# dat NewBoxPlot het wel doet... Ligt aan de interne code, vraag me niet waarom. 
ModeSchipholTrain <- table(dsCase$SchipholTrain)
names(ModeSchipholTrain)[ModeSchipholTrain==max(ModeSchipholTrain)]

# Outliers aanmaken. 
outliers <- dsCase[dsCase$ImportPrice < 40 & dsCase$rateAirplane < 40,
                   c("ImportPrice","rateAirplane")]

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

# ggplot opslaan.
ggplot2::ggsave(paste0("baseplot_1.pdf"))         
#------------------------------------------------------------------------------------
# Doorkruisendheden en Interactie
#------------------------------------------------------------------------------------

# Lu en Iris nog aan werken. 


#------------------------------------------------------------------------------------
# Meervoudige Samenhang
#------------------------------------------------------------------------------------
# Factoren aanmaken
# In de eerste FacManip... gaat er altijd iets fout, onafhankelijk van de factor die je als eerste noemt.
# Dit ligt aan de interne code in R en dus niet aan ons. 
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

# Regressie ...................

