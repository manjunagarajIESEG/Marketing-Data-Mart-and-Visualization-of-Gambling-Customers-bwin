#---------------------------------------------------------------------------------------#
# Install Packages
#---------------------------------------------------------------------------------------#
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("sas7bdat")) install.packages("sas7bdat"); library("sas7bdat")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

# filter the rows which are out of the period from feb 1 2005 to sept 30 2005

demographic <- demographic %>% filter(FirstPay >= as.Date("2005-02-01") & FirstPay <= as.Date("2005-09-30"))


# countries , language and appliction  with their IDs are imported 
path = "C:\\Users\\mrudrappa\\Desktop\\R Group Project\\Open-source-programming-master\\Open-source-programming-master\\Group Assignment\\"

# import countries
country <- read_excel(paste0(path,"country.xlsx"))

# import continents
continents <- read_excel(paste0(path,"continent.xlsx"))

# import languages
language <- read_excel(paste0(path,"language.xlsx"))

# import applications
application <- read_excel(paste0(path,"application.xlsx"))

# import demographic
demographic <- read.sas7bdat(file = (paste0(path,"RawDataIDemographics.sas7bdat")))


# merge demographic and country
demographic <- merge(x = demographic, y = country, by = "Country", all.x = TRUE)

# merge demographic and continent
demographic <- merge(x = demographic, y = continents, by = "CountryName", all.x = TRUE)


# merge demographic and application
demographic <- merge(x = demographic, y = application, by = "ApplicationID", all.x = TRUE)

# merge demographic and language
demographic <- merge(x = demographic, y = language, by = "Language", all.x = TRUE)

# delete existing country language and applation with ID
demographic$Language <- NULL
demographic$ApplicationID <- NULL
demographic$Country <- NULL

# Gender with 1 are Male and 0 are female
demographic$Gender <- ifelse(demographic$Gender == 0,"Female","Male" )


# convert factor to date format
demographic$RegDate <- as.Date(demographic$RegDate)

demographic$FirstPay <- ymd(demographic$FirstPay)

demographic$FirstAct <- ymd(demographic$FirstAct)

demographic$FirstSp <- ymd(demographic$FirstSp)

demographic$FirstCa <- ymd(demographic$FirstCa)

demographic$FirstGa <- ymd(demographic$FirstGa)

demographic$FirstPo <- ymd(demographic$FirstPo)











