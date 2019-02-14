#---------------------------------------------------------------------------------------#
# Install Packages
#---------------------------------------------------------------------------------------#
if(!require("sas7bdat")) install.packages("sas7bdat"); library("sas7bdat")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("readxl")) install.packages("readxl"); library("readxl")
#---------------------------------------------------------------------------------------#
# Import daily aggregation data set , Dont forget to change the path
#---------------------------------------------------------------------------------------#

path = "C:\\Users\\mrudrappa\\Desktop\\R Group Project\\Open-source-programming-master\\Open-source-programming-master\\Group Assignment\\"

RawDataIIUserDailyAggregation <- read.sas7bdat(file = (paste0(path, "RawDataIIUserDailyAggregation.sas7bdat")))
RawDataIIUserDailyAggregation$Date <- ymd(RawDataIIUserDailyAggregation$Date)

#---------------------------------------------------------------------------------------#
# Remove the transaction of the customers whose transact date is earlier than first pay 
# in date of demographic dataset
#---------------------------------------------------------------------------------------#

z <- merge(x = RawDataIIUserDailyAggregation , y = demographic[ , c("UserID", "FirstPay")], by = "UserID",all.x = TRUE)

RawDataIIUserDailyAggregation <- z %>% filter(Date > FirstPay & Date < as.Date("2005-09-30") & Date > as.Date("2005-02-01"))

rm(z)

# product descrption with their IDs are imported 
path = "C:\\Users\\mrudrappa\\Desktop\\R Group Project\\Open-source-programming-master\\Open-source-programming-master\\Group Assignment\\"

# import product
product <- read_excel(paste0(path,"product description.xlsx"))

# merge user daily aggregation and product description
RawDataIIUserDailyAggregation <- merge(x = RawDataIIUserDailyAggregation, y = product, by.x = "ProductID", by.y = "id", all.x = TRUE)
RawDataIIUserDailyAggregation$ProductID <- NULL

# stakes , winnings and bets with negatives are made 0 ,because the negative numbers were due to accounting correction
RawDataIIUserDailyAggregation$Bets <- ifelse(RawDataIIUserDailyAggregation$Bets < 0 , 0 ,RawDataIIUserDailyAggregation$Bets)
RawDataIIUserDailyAggregation$Stakes <- ifelse(RawDataIIUserDailyAggregation$Stakes < 0, 0,RawDataIIUserDailyAggregation$Stakes)
RawDataIIUserDailyAggregation$Winnings <- ifelse(RawDataIIUserDailyAggregation$Winnings < 0,0,RawDataIIUserDailyAggregation$Winnings)
#---------------------------------------------------------------------------------------#
# create one row per customer and derive new variables from user daily aggragation
#---------------------------------------------------------------------------------------#

user_daily_aggregation <- RawDataIIUserDailyAggregation %>% group_by(UserID) %>%
                          summarise(mean_stakes_sports = mean(Stakes),
                                    total_stakes_sports = sum(Stakes),
                                    max_stakes_sports = max(Stakes),
                                    min_stakes_sports = min(Stakes),
                                    mean_winnings_sports = mean(Winnings),
                                    total_winnings_sports = sum(Winnings),
                                    max_winnings_sports = max(Winnings),
                                    min_winnings_sports = min(Winnings),
                                    mean_bets_sports = mean(Bets),
                                    total_bets_sports = sum(Bets),
                                    max_bets_sports = max(Bets),
                                    min_bets_sports = min(Bets),
                                    max_date_sports = max(Date),
                                    min_date_sports = min(Date),
                                    total_days_sports = as.numeric(difftime(max_date_sports,min_date_sports, units = "days")),
                                    no_of_sports_product_played = n(),
                                    freq_sports_product_purchased = ifelse(total_days_sports == 0,0,no_of_sports_product_played / total_days_sports),
                                    max_sports_product_played = names(which.max(table(product_name))),
                                    freq_stakes_sports = ifelse(total_days_sports == 0,0,sum(Stakes > 0)/ total_days_sports),
                                    freq_winnings_sports = ifelse(total_days_sports == 0,0,sum(Winnings > 0)/ total_days_sports),
                                    freq_bets_sports = ifelse(total_days_sports == 0,0,sum(Bets > 0)/ total_days_sports),
                                    GGR_Sports = total_stakes_sports - total_winnings_sports)
                                    
                                    
                                    






