#----------------------------------------------------------
# Programmacode labsessie 2, 2019-2020
#----------------------------------------------------------

# Remove all the information in memory
remove(list=ls())
cat("\f")

#----------------------------------------------------------
# 1. Import/Read data 
#----------------------------------------------------------

# Set working directory, as discussed during the first lab
# session. The work directory is where R will be looking 
# for information, or send information to. You have to 
# adjust the working directory to your own situation 
# settings before the thing will run. 

setwd("~/Name of your working directory/Data")

# Use read.csv to read the data
dsCase <- read.csv(file = "SMT1920casus01.csv", 
                   stringsAsFactors = FALSE)

# The previous shows a file with 702 observations over 1 
# variable. Not good. Flawed input because R is instructed 
# to read a comma separated file, while in reality the file
# is semicolon separated.

# Use read.csv to read the data
dsCase <- read.csv2(file = "SMT1920casus01.csv", 
                    stringsAsFactors = FALSE)

# This time the resulting data frame has 693 observations 
# over 62 variables, which is correct.


# Check some observations of the data frame
head(dsCase)
tail(dsCase)
# View(dsCase)


#----------------------------------------------------------
# Import/Read data - alternative
#----------------------------------------------------------

# In all future code an alternative, more flexible approach is 
# applied to defining the appropriate directories. For this
# approach to succeed it is necessary to have a 'main' directory
# with three subfolders, named Data, Programs, Results.

# The advantage will be that different files will be taken from
# or stored to a natural location, while collaborators working
# on the same code on their own machines will only need to change
# the name of the 'main' directory in order to let the code work
# (assuming that the collaborator has the same three subfolders)

# Before showing the alternative it is important to know that
# pieces of text can be glued together by means of the paste
# and paste0 functions.
paste("Apples", "Bananas")
paste0("Apples", "Bananas")

x <- "Apples"
y <- "Bananas"
paste0(x, y)

# The paste0 function will be used to construct thenames of the
# directories. 'dir' will be the name of the main directory, 
# 'dirData' the name of the subfolder Data, et cetera. Note that 
# the tilde ~ supposedly defines the path of the documents
# folder (saves typing). Also note that all path names end 
# with a forward slash.

dir <- "~/Name of your working directory with forward slash/"

dirProg <- paste0(dir,"Programs/")
dirData <- paste0(dir,"Data/")
dirRslt <- paste0(dir,"Results/")

dsCase <- read.csv2(file = paste0(dirData, "SMT1920casus01.csv"), 
                    stringsAsFactors = FALSE)



#------------------
# Summarize data 
#------------------

# Make a summary of the data and check the structure of the 
# data frame. The summary shows that missing values (NA's) 
# occur in the Big04, Big08 and some other columns. 

head(dsCase)
tail(dsCase)

summary(dsCase)

str(dsCase)




#----------------------------------------------------------
# 2. Data cleaning
#----------------------------------------------------------

# Typically, empirical analysis requires an extensive phase
# of data cleansing, and defining and redefining variables.
# Here, a simple example of defining a new variable is
# illustrated. Other forms of data cleansing will be 
# introduced later


#------------------
# 2.1 Define new variables
#------------------

# Define variable Age as 2020 minus the year of birth, and
# add this new variable to the data frame
dsCase$Age <- 2020 - dsCase$YrBirth

# Alternatively, one could define a variable Age without
# adding it to the data frame (which has not been done, as
# we want to have it available as part of the avaialble 
# data) -- note that in this instruction the dsCase part 
# has not been mentioned
# Age <- 2020 - dsCase$YrBirth


#------------------
# 2.2 Missing values
#------------------

# Many columns/variables have missing values (NA's). The 
# question is how this impacts the results, how to identify
# them, and how to treat them.


#-------------------
# Presence and consequences of missing values (example)
#-------------------

# Check if NA's are present in the data
summary(dsCase)

# Unexpected results may be found, if NA's are not 
# considered. na.rm = TRUE means that missings are excluded
# from the calculations
mean(dsCase$Nep02)
mean(dsCase$Nep02, na.rm = TRUE)


#-------------------
# Check pattern of missings
#-------------------

# Complete and incomplete cases/rows in the data frame
complete.cases(dsCase)                     # Check
sum(complete.cases(dsCase))                # Complete
nrow(dsCase) - sum(complete.cases(dsCase)) # Incomplete

# Identify cells with missing values
is.na(dsCase)            # Check if NA
which(is.na(dsCase))     # Find positions (columns wise)
which(is.na(dsCase), arr.ind = TRUE)  
colSums(is.na(dsCase))   # Number of missings per colums
rowSums(is.na(dsCase))   # Number of missings per row

# Show rows with missing (not complete) cases
dsCase[!complete.cases(dsCase), ]

# Determine the number of missing observations for each
# respondent, and temporarily add the column to the data
# frame
nmissing <- rowSums(is.na(dsCase))
table(nmissing)


#-------------------
# Treat missing values
#-------------------

# Missing values can be handled in different ways. Values
# can be imputed (given a value), or observations with NA's
# can be deleted.

# Replace a missing value for a particular variable a
# specific value. This may work if the missing values are
# isolated, like in the single case of Nep02. Other values 
# may be preferred.
dsCase$Nep02[is.na(dsCase$Nep02)] <- 2

# Alternative one could replace a missing with the mean
# or median of a particular variable. (Rounding is applied 
# to keep in pace with the other values, but this may not be 
# deemed relevant)
dsCase$Big04[is.na(dsCase$Big04)] <- 
   round(mean(dsCase$Big04, na.rm=TRUE))

dsCase$Big08[is.na(dsCase$Big08)] <- 
   round(median(dsCase$Big08, na.rm=TRUE))

# Remove rows (observations) which are incompletely 
# observed. This may be done when observations are cheap 
# and there is limited risk of biasing the results. Again,
# several options apply.

# One option is to check if perhaps there are observations
# with relatively many missings as opposed to observations
# that have only one missing value, which may be imputed 
# with the previously illustrated techniques. The following
# instruction removes observations with two or more missings.
dsCase <- dsCase[!(nmissing > 1),]

# Another, more radical option is to remove observations with 
# missing valiues all together.
dsCase <- dsCase[complete.cases(dsCase),]

# Whatever the prefered approach, always motivate and 
# explain in the paper. 


#----------------------------------------------------------
# 2.3 Outlier analysis
#----------------------------------------------------------

#-----------
# Graphical approaches
#-----------

# Histogram, 
hist(dsCase$HrStud,col="Orange", main = "",
     ylim = c(0,200),
     xlab="Study time (in hours/week)", las=1)

# Box plot
boxplot(dsCase$HrStud, col="blue", las=1)


#-----------
# Numerical approaches: z-scores
#-----------

# Calculate z-scores with the scale function
zAge     <- scale(dsCase$Age)
zHrStud  <- scale(dsCase$HrStud)

# same, with the mean and sd functions, which
# may be considered less transparent
zAge <- (dsCase$Age - mean(dsCase$Age, na.rm = TRUE))/
   sd(dsCase$Age, na.rm = TRUE)


#-----------
# Find records with large z-values
#-----------

# Find records with large HrStud
which(dsCase$HrStud > 50)

# Find observation numbers of extremes
which(zHrStud > 3.5) 
which(abs(zHrStud) > 3.5) 

# HrStud (!) associated with zHrStud > 3.5
dsCase$HrStud[which(zHrStud > 3.5)] 


#-----------
# Check robustness for outliers
#-----------

# Sample averHrStud before removing outliers
avgHrStud.Before <- mean(dsCase$HrStud, na.rm=TRUE)

# Sample averHrStud after removing outliers
avgHrStud.After <- 
   mean(dsCase$HrStud[-which(abs(zHrStud)>3.5)],
        na.rm=TRUE)

# Reporting consequences
cat("Absolute difference after and before         ",
    (avgHrStud.After - avgHrStud.Before), "\n")
cat("Relative difference after and before (in pct)",
    100*(avgHrStud.After - avgHrStud.Before)/avgHrStud.Before, "\n")


#----------------------------------------------------------
# 3. Visualization
#----------------------------------------------------------

#------------------
# Plots using native R functions
#------------------

# Simple plots
hist(dsCase$rateTrain)                     # histogram
boxplot(dsCase$rateTrain)                  # boxplot
plot(dsCase$rateTrain,dsCase$rateAirplane) # scatter plot

# The bar and pie charts for qualitative data require
# a frequency table as input, which needs to be made
# prior to making the figures
tbl <- table(dsCase$cLiving)
tbl

barplot(tbl)              # bar chart
pie(tbl)                  # pie chart


# Enhanced plots using graphical parameters
hist(dsCase$rateTrain,
     col = "green", las = 1,
     xlab = "Likelihood to take the train",
     main = "")

plot(dsCase$rateTrain,dsCase$rateAirplane,
     col = "red", las = 1, pch = 19,
     ylab = "Likelihood to take the airplane", 
     xlab = "Likelihood to take the train",
     main = "")


#------------------
# Plots using ggplot
#------------------

# A more coherent, structured approach to making figures is
# based on the grammar-of-graphics. This grammar underlies 
# the ggplot function, which is part of the ggplot2 package.
# The package needs to be installed and loaded before it can 
# be used

# Install ggplot2 (once), and load it for this session
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Plots with ggplot involve the following inputs: (i) a data
# frame; (ii) aesthetics (the columns that contain the 
# coordinates, or values, to process); (iii) geometries (the
# kind of symbols, like bars or points, to present); and (iv)
# scaling definitions and other.


# Histogram (basic and enhanced)
ggplot(dsCase, aes(x = rateTrain)) +
   geom_histogram()

ggplot(dsCase, aes(x = rateTrain)) +
   geom_histogram(bins = 15, fill = "purple", col = "black") +
   xlab("Likelihood to take the train")
ggsave(paste0(dirRslt, "ggplotHistogram.pdf"))


# Bar chart (basic and enhanced; no frequency table needed)
ggplot(dsCase, aes(x = cLiving)) +
   geom_bar()

ggplot(dsCase, aes(x = cLiving)) +
   geom_bar(fill = "Red2", col = "black") + 
   xlab("Typering leefomstandigheden")

# Same, but ensure value labels. This is done by
# changing data type fo factor (for qualitative variables)
dsCase$f.cLiving <- 
   factor(dsCase$cLiving,
          labels=c("Student housing", "Private rent",
                   "Parents", "Own house", "Other"))
# levels(dsCase$f.cLiving)

ggplot(dsCase, aes(x = f.cLiving)) +
   geom_bar(fill = "violet", col = "black") + 
   xlab("Typering leefomstandigheden")
ggsave(paste0(dirRslt, "ggplotBarSingle.pdf"))


# Bar chart grouped
dsCase$f.dFraternity <- factor(dsCase$dFraternity,
                               labels=c("Ja", "Nee"))

ggplot(dsCase, aes(x=f.dFraternity, fill=f.cLiving)) +
   geom_bar(position="dodge") +
   ylab("Frequentie") + xlab("Lid studentenvereniging") +
   scale_fill_brewer("Lid vereniging",palette="Set1")
ggsave(paste0(dirRslt, "ggplotBarGrouped.pdf"))


# Scatter plot (basic and enhanced)
ggplot(dsCase, aes(x = HrPWork, y = rateTrain)) +
   geom_point()

ggplot(dsCase, aes(x = HrPWork, y = rateTrain)) +
   geom_point(col = "blue") +
   xlab("Betaald werk (uren/week)") +
   ylab("Likelihood to take the train")
ggsave(paste0(dirRslt, "ggplotScatterplot.pdf"))

ggplot(dsCase, aes(x = HrPWork, y = rateTrain, colour = f.cLiving)) +
   geom_point() +
   xlab("Betaald werk (uren/week)") +
   ylab("Likelihood to take the train") +
   scale_colour_discrete("Woonsituatie")

# ggplot(dsCase, aes(x = HrPWork, y = rateTrain, colour = f.cLiving)) +
#    geom_point(size = 2) +
#    xlab("Betaald werk (uren/week)") +
#    ylab("Likelihood to take the train") +
#    scale_colour_discrete("Woonsituatie") +
#    geom_smooth(method = "lm", colour = "black")









#----------------------------------------------------------
# 4. Evaluate scale reliability and construct Likert-indicators
#----------------------------------------------------------

# Scale analysis and scale construction is done with the 
# alpha function from the psych package, which has been 
# installed before.
# install.packages("psych", dependencies = TRUE)
library(psych)

# The example is about Guilt feelings consisting of five
# items Guilt01 to Guilt05.

#------------------
# 4.1 Store items in a vector 
#------------------

# It can be convenient to store the names of the items in 
# a vector, which saves repeated retyping

itmsGuilt <- c("Guilt01", "Guilt02", "Guilt03", 
               "Guilt04", "Guilt05")

# It would make sense to make a descriptive summary of 
# these items to see if anything unusual is present in
# the outcomes. The way to do so has been illustrated 
# before, and will not be repeated


#------------------
# 4.2 Basic scale analysis
#------------------

# Determine scale reliability (Cronbach's alpha) with the 
# scale function from the psych package
alpha(dsCase[itmsGuilt])

# It may be necessary to explicitly add the package name
# to the function if the function name is already in use 
# by another package, such as ggplot2
# psych::alpha(dsCase[itmsGuilt])


#------------------
# 4.3 Recoding items
#------------------

# The function run issues a warning saying that 'Some items
# were negatively correlated with the total scale and 
# probably should be reversed.' Two approaches are presented 
# here, of which the second is prefered.

# (i) Automatic check of item codes. This works fine but 
# is not recommended, as it may lead to ignore relevant 
# warnings and yield the exact opposite scale. Reverse 
# coded items are indicated by a minus after the item name.
alpha(dsCase[itmsGuilt], check.keys = TRUE)
# psych::alpha(dsCase[itmsGuilt], check.keys = TRUE)

# In this case, precisely the wrong items are recoded.

# (ii) Another option is to recode the desired items
# manually, which is done through the keys option. The two
# instructions below are identical.
alpha(dsCase[itmsGuilt], 
      keys = c("Guilt03", "Guilt04", "Guilt05"))
alpha(dsCase[itmsGuilt], 
      keys = itmsGuilt[c(3,4,5)])


#------------------
# 4.4 Remove items (only when needed)
#------------------

# If items need to be removed, one can just redefine the 
# itmsGuilt vector and re-run the analysis. Suppose, for
# instance, that Guilt02 has to be removed, then the 
# following can be applied (currently commented out,
# because it is not wanted in this example)

# itmsGuilt <- c("Guilt01", "Guilt03", "Guilt04", "Guilt05")
# 
# alpha(dsCase[itmsGuilt], 
#       keys = c("Guilt03", "Guilt04", "Guilt05"))


#------------------
# 4.5 After scale analysis, determine participant scores
#------------------

# Respondent scores are available in the alpha-object.
# They can be available either as the mean or the sum 
# of the items, which can be set through the option
# cumulative. Afterward, the scores can be extracted 
# from the alpha-object with the `$` approach

# Parameter 'cumulative = TRUE' yields summated scores
rsltGuilt <- 
   alpha(dsCase[itmsGuilt], 
         keys = c("Guilt03", "Guilt04", "Guilt05"),
         cumulative = TRUE)
dsCase$sumGuilt <- rsltGuilt$scores

# Parameter 'cumulative = FALSE' yields average scores
rsltGuilt <- 
   alpha(dsCase[itmsGuilt], 
         keys = c("Guilt03", "Guilt04", "Guilt05"),
         cumulative = FALSE)
dsCase$avgGuilt <- rsltGuilt$scores

# The second approach (based on the average item scores)
# is mostly seen nowadays, and can be adopted. The former
# approach (based on the sum of item scores) may be 
# skipped

# The complete contents of the alpha object can be 
# inspected with the str function
str(rsltGuilt)

# Specific information elements can be retrieved by 
# rsltGuilt$'some element', as was illustrated 
# for the respondent scores, rsltGuilt$scores




#----------------------------------------------------------
# 5. Descriptive analysis and interval estimation
#----------------------------------------------------------


#------------------
# 5.1 Descriptive summaries
#------------------

# Descriptives
mean(dsCase$rateTrain, na.rm = TRUE)
median(dsCase$rateTrain, na.rm = TRUE)

sd(dsCase$rateTrain, na.rm = TRUE)
IQR(dsCase$rateTrain, na.rm = TRUE)

min(dsCase$rateTrain, na.rm = TRUE)
max(dsCase$rateTrain, na.rm = TRUE)

#------------------
# Make descriptives table
#------------------

# Tables with descriptive summaries are conveniently
# made with the describe function from the psych
# package. 

# Apply the describe function 
describe(dsCase)                # All stats, all columns
describe(dsCase, skew = FALSE)  # Suppress skewness

# In case of conflict, put psych:: in front:

# Apply the describe function to a subset of columns,
# which is convenient if the data frame contains much
# more columns/variables than needed for the research
sub <- c("rateTrain", "avgGuilt", "HrPWork")
describe(dsCase[sub], skew = FALSE)

# Moreover, it can be convenient to stote the table
# and make further selections
tmp <- describe(dsCase[sub], skew = FALSE)
tmp[c("mean", "sd")]


#------------------
# Make frequency table
#------------------

# Frequency tables are made with the table function,
# as has been illustrated before
tbl <- table(dsCase$cLiving)
tbl

cbind(Freq       = tbl, 
      CumFreq    = cumsum(tbl), 
      RelFreq    = 100*tbl/sum(tbl),
      CumRelFreq = 100*cumsum(tbl)/sum(tbl))

round(cbind(Freq       = tbl, 
            CumFreq    = cumsum(tbl), 
            RelFreq    = 100*tbl/sum(tbl),
            CumRelFreq = 100*cumsum(tbl)/sum(tbl)),
      3)



#----------------------------------------------------------
# 5.2 Determine confidence intervals for the mean step by step
#----------------------------------------------------------


# Calculate sample size, mean and standard deviation
n   <- sum(!is.na(dsCase$Age))
avg <- mean(dsCase$Age,na.rm=TRUE)
std <- sd(dsCase$Age,na.rm=TRUE)

# Error level, and critical value
alpha  <- 0.05
t_crit <- qt(1 - alpha/2, n-1)

# Lower and upper bounds
CI_low <- avg - t_crit*std/sqrt(n)
CI_upp <- avg + t_crit*std/sqrt(n)

# Ship results to console
cbind(Mean = avg, ConfLevel = 1 - alpha, tCrit = t_crit, 
      StDev = std, nObs = n, StdErr = std/sqrt(n), 
      CIlow = CI_low, CIupp = CI_upp)

# Rounding the numerical results to three decimals
round(cbind(Mean = avg, ConfLevel = 1 - alpha, tCrit = t_crit, 
            StDev = std, nObs = n, StdErr = std/sqrt(n), 
            CIlow = CI_low, CIupp = CI_upp), 3)


#----------------------------------------------------------
# Determine confidence intervals for the mean step by step
# (this time based on the descriptives table)
#----------------------------------------------------------

# Make table with descriptives
tbl <- describe(dsCase[c("Age", "avgGuilt", "HrStud")], 
                skew = FALSE, ranges = FALSE)
tbl

# Select relevant columns
tbl <- tbl[, c("n", "mean", "sd")] 
tbl

# Set the error level (once, same for all variables)
alpha  <- 0.05

# Determine critical value, which may be different for 
# different variables depending on the number of valid 
# observations. The criticval value is therefore stored
# in a column of tbl
tbl$t_crit <- qt(1 - alpha/2, tbl$n - 1)
tbl

# Lower and upper bounds. Again note that 'tbl$' is added 
# for all information that is taken from the table
tbl$CI_low <- tbl$mean - tbl$t_crit*tbl$sd/sqrt(tbl$n)
tbl$CI_upp <- tbl$mean + tbl$t_crit*tbl$sd/sqrt(tbl$n)
tbl



#----------------------------------------------------------
# Determine confidence intervals for the proportion step by step
#----------------------------------------------------------

# Find sample proportion of students living in villages
# (cLocate = 4). It starts with a frequency table, which
# summarises the counts of all outcomes
tbl <- table(dsCase$cLocate)
tbl

pct <- tbl/sum(tbl) # Proportions of all outcomes
pS  <- pct[4]       # Select the villages


# Determine valid sample size and standard error of the
# sample proportion
n      <- sum(!is.na(dsCase$cLocate))
stderr <- sqrt(pS*(1-pS)/n)

# Error level, and critical value
alpha  <- 0.05 
z_crit <- qnorm(1 - alpha/2)

# Lower and upper bounds
CI_low <- pS - z_crit*stderr
CI_upp <- pS + z_crit*stderr

cbind(Proportion = pS, ConfLevel = 1 - alpha, zCrit = z_crit, 
      StdErr = stderr, nObs = n, CIlow = CI_low, CIupp = CI_upp)

# If you do not like the many decimals, you may consider 
# using the round function
round(
   cbind(Proportion = pS, ConfLevel = 1 - alpha, zCrit = z_crit, 
            StdErr = stderr, nObs = n, CIlow = CI_low, CIupp = CI_upp),
   3)

#----------------------------------------------------------
# 5.3 Determine confidence intervals with functions
#----------------------------------------------------------

# Example t.test function to estimate the mean age. The 95%
# confidence level is set as the default
t.test(dsCase$Age)


# Example prop.test function 

# Determine frequencies and apply test
nVillage <- table(dsCase$cLocate == 4)
prop.test(nVillage)

# Reverse order (prop.test acts on the table with counts
# of FALSE and TRUE outcomes, and focusses on the first
# outcome. In the code before, the first code is FALSE 
# implying that results are obtained for not living in 
# a village. After reversing the order, by putting the 
# second outcome upfront, results for the TRUE outcomes 
# are obtained, which refers to living in a village)
prop.test(nVillage[2:1])




#----------------------------------------------------------
# 6. Export figures and tables
#----------------------------------------------------------

# The results of analysis are often used in other material,
# like presentations or papers. This means that the tables 
# and figures need to be exported

# For the ggplot figures this can be conveniently done with 
# the ggsave function. For the native hist, barplot, plot
# and other graphical functions this is done with the 
# pdf/png/etc and dev.off functions.

#-----------
# Export figures
#-----------

# Histogram, example of pdf format which is automatically 
# shipped to the results subfolder. The connection is
# opened with the pdf function and closed (!!!) with the 
# dev.off function (DO NOT FORGET TO CLOSE THE CONNECTION)
pdf(paste0(dirRslt, "ExampleHistogram.pdf"))
hist(dsCase$HrStud,col="Orange", main = "",
     ylim = c(0,225),
     xlab="Study time (in hours/week)", las=1)
dev.off()

# same, but png format
png(paste0(dirRslt, "ExampleHistogram.png"))
hist(dsCase$HrStud,col="Orange", main = "",
     ylim = c(0,225),
     xlab="Study time (in hours/week)", las=1)
dev.off()


# Illustrated for pdf, but works the same for png(),
# jpeg() and other. Important: connection opened with
# pdf(), png() or other, need to be closed with
# dev.off() 


#-----------
# Export tables
#-----------

# For convenience, collect names of the desired variables in
# a vector myVars
myVars <- c("avgGuilt", "Age", "rateTrain")

# Option 1: stargazer
# Note: first time usage requires that install.packages is
# run; in subsequent usage, the instruction can be 
# commented out

# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

stargazer(dsCase[myVars], 
          omit.summary.stat = c("p25", "p75"))

stargazer(dsCase[myVars], 
          type = "html", 
          out = paste0(dirRslt, "myDescriptives.doc"))


# Optie 2: describe/write.csv
tbl <- describe(dsCase[myVars], skew = FALSE, ranges = FALSE)
write.csv2(tbl, file = paste0(dirRslt, "myDescriptives.csv"))


# Optie 3: write table to clipboard
# In Windows, this can be done with the write.table 
# function, while specifying the "clipboard" option.
# In Mac OS this leads to unexpected behavior. It is 
# therefore to use the clipr package
tbl <- describe(dsCase[myVars], skew = FALSE, ranges = FALSE)

# Install and load the clipr package
# install.packages("clipr", dependencies = TRUE)
library(clipr)

write_clip(tbl)

# If you now open an excel spreadsheet, Word, or otherwise,
# and give ctrl/cmd-C, you will see the table.
