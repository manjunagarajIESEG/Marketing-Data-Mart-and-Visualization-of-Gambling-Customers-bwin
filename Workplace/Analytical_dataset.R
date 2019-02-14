#---------------------------------------------------------------------------------------#
# Install Packages
#---------------------------------------------------------------------------------------#
if(!require("sas7bdat")) install.packages("sas7bdat"); library("sas7bdat")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("readxl")) install.packages("readxl"); library("readxl")
#---------------------------------------------------------------------------------------#
# Import daily analytical data set , Dont forget to change the path
#---------------------------------------------------------------------------------------#

path = "C:\\Users\\mrudrappa\\Desktop\\R Group Project\\Open-source-programming-master\\Open-source-programming-master\\Group Assignment\\"

Analytical <- read.sas7bdat(file = (paste0(path,"AnalyticDataInternetGambling.sas7bdat")))

#---------------------------------------------------------------------------------------#
# Cleaning up analytical data set 
#---------------------------------------------------------------------------------------#

# converting all date columns to date format

Analytical$RegistrationDate <- as.Date(Analytical$RegistrationDate, origin="1960-01-01")

Analytical$FOFirstActiveDate <- as.Date(Analytical$FOFirstActiveDate, origin="1960-01-01")

Analytical$FOLastActiveDate <- as.Date(Analytical$FOLastActiveDate, origin="1960-01-01")

Analytical$LAFirstActiveDate <- as.Date(Analytical$LAFirstActiveDate, origin="1960-01-01")

Analytical$LALastActiveDate <- as.Date(Analytical$LALastActiveDate, origin="1960-01-01")

Analytical$FirstSportsActiveDate <- as.Date(Analytical$FirstSportsActiveDate, origin="1960-01-01")


# Converting nan values to 0

Analytical$LATotalStakes[is.na(Analytical$LATotalStakes)] <- 0

Analytical$LATotalWinnings[is.na(Analytical$LATotalWinnings)] <- 0

Analytical$LATotalBets[is.na(Analytical$LATotalBets)] <- 0

Analytical$LATotalDaysActive[is.na(Analytical$LATotalDaysActive)] <- 0

Analytical$FOTotalStakes[is.na(Analytical$FOTotalStakes)] <- 0

Analytical$FOTotalWinnings[is.na(Analytical$FOTotalWinnings)] <- 0

Analytical$FOTotalBets[is.na(Analytical$FOTotalBets)] <- 0

Analytical$FOTotalDaysActive[is.na(Analytical$FOTotalDaysActive)] <- 0


Analytical$COUNTRY <- NULL
Analytical$LANGUAGE <- NULL
Analytical$RegistrationDate <- NULL
Analytical$GENDER <- NULL


