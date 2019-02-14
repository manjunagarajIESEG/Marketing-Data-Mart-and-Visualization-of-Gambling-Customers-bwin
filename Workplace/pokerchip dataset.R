
#---------------------------------------------------------------------------------------#
# Install Packages
#---------------------------------------------------------------------------------------#
if(!require("sas7bdat")) install.packages("sas7bdat"); library("sas7bdat")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
#---------------------------------------------------------------------------------------#
# Import poker data set , Dont forget to change the path
#---------------------------------------------------------------------------------------#

path = "C:\\Users\\mrudrappa\\Desktop\\R Group Project\\Open-source-programming-master\\Open-source-programming-master\\Group Assignment\\"

RawDataIIIPokerChipConversions <- read.sas7bdat(file = (paste0(path, "RawDataIIIPokerChipConversions.sas7bdat")))


# split date and time as seperate columns
RawDataIIIPokerChipConversions <- separate(RawDataIIIPokerChipConversions,TransDateTime, c("date", "time"),sep =" ")
RawDataIIIPokerChipConversions$date <- as.Date(RawDataIIIPokerChipConversions$date)


#---------------------------------------------------------------------------------------#
# Remove the transaction of the customers whose transact date is earlier than first pay 
# in date of demographic dataset also between the period  February 1, 2005 through September 30, 2005
#---------------------------------------------------------------------------------------#


v <- merge(x = RawDataIIIPokerChipConversions, y = demographic[ , c("UserID", "FirstPay")], by = "UserID",all.x = TRUE)

RawDataIIIPokerChipConversions <- v %>% filter(date >= FirstPay & date <= as.Date("2005-09-30") & date >= as.Date("2005-02-01"))

#---------------------------------------------------------------------------------------#
# create one row per customer and derive new variables from pokerchip conversions
#---------------------------------------------------------------------------------------#

#Check for any missing values in the data frame
apply(is.na(RawDataIIIPokerChipConversions),2,sum)


# convert date to date format and take only hours from time
RawDataIIIPokerChipConversions$date <- as.Date(RawDataIIIPokerChipConversions$date)
RawDataIIIPokerChipConversions$time <- as.numeric(substr(RawDataIIIPokerChipConversions$time,1,2))


# total transaction amount sold per customer
pokerchip  <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%  
              summarise(total_pokerchip_amount_sold = sum(TransAmount[TransType==124]))


# total transaction amount bought per customer
a <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%  
     summarise(total_pokerchip_amount_bought = sum(TransAmount[TransType==24]))

pokerchip[,3] <- a[,2]

# max transaction amount sold per customer
b <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
     summarise(max_pokerchip_amount_sold = max(TransAmount[TransType==124]))

pokerchip[,4] <- b[2]

# min transaction amount sold per customer
c <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
     summarise(min_pokerchip_amount_sold = min(TransAmount[TransType==124]))

pokerchip[,5] <- c[2]

# max transaction amount bought per customer
d <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
  summarise(max_pokerchip_amount_bought = max(TransAmount[TransType==24]))

pokerchip[,6] <- d[2]

pokerchip$max_pokerchip_amount_bought <- ifelse(pokerchip$max_pokerchip_amount_bought == -Inf , 0 ,pokerchip$max_pokerchip_amount_bought)

# min transaction amount bought per customer
e <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
  summarise(min_pokerchip_amount_bought = min(TransAmount[TransType==24]))

pokerchip[,7] <- e[2]
pokerchip$min_pokerchip_amount_bought <- ifelse(pokerchip$min_pokerchip_amount_bought == Inf , 0 ,pokerchip$min_pokerchip_amount_bought)

# Average transaction amount sold per customer
f <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
     summarise(Avg_pokerchip_amount_sold = mean(TransAmount[TransType==124]))

pokerchip[,8] <- f[2]

# Average transaction amount bought per customer
g <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>%
  summarise(Avg_pokerchip_amount_bought = mean(TransAmount[TransType==24]))


pokerchip[,9] <- g[2]

pokerchip$Avg_pokerchip_amount_bought[is.nan(pokerchip$Avg_pokerchip_amount_bought)] <- 0

# Number of times transactions sold  per customer
h <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% 
     summarise(n_pokerchip_transactions_sold = sum(TransType==124))

pokerchip[,10] <- h[2]

# Number of times transactions bought per customer
i <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% 
  summarise(n_pokerchip_transactions_bought = sum(TransType==24))

pokerchip[,11] <- i[2]


# max date of transaction
j <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% summarise(max_pokerchip_transact_date = max(date))
pokerchip[,12] <- j[2]

# min date of transaction
k <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% summarise(min_pokerchip_transact_date = min(date))
pokerchip[,13] <- k[2]

# total number of days of transactions
pokerchip$total_days_poker_transact<- as.numeric(difftime(pokerchip$max_pokerchip_transact_date,pokerchip$min_pokerchip_transact_date, units = "days"))


# frequency of transactions sold
pokerchip$frequency_pokerchip_sold <- ifelse(pokerchip$total_days_poker_transact == 0,0,
                    pokerchip$n_pokerchip_transactions_sold / pokerchip$total_days_poker_transact)


# frequency of transactions bought
pokerchip$frequency_pokerchip_bought <- ifelse(pokerchip$total_days_poker_transact == 0,0,
                    pokerchip$n_pokerchip_transactions_bought / pokerchip$total_days_poker_transact)

# Morning transactions
RawDataIIIPokerChipConversions <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% 
 mutate(morning_pokerchip_transactions = ifelse(time >= 6 & time < 12, 1, 0))

q <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% summarise(morning_pokerchip_transaction = sum(morning_pokerchip_transactions))
pokerchip[,17] <- q[2]

# Evening transactions
RawDataIIIPokerChipConversions <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% 
  mutate(evening_pokerchip_transactions = ifelse(time >= 12 & time < 17, 1, 0))

r <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% summarise(evening_pokerchip_transaction = sum(evening_pokerchip_transactions))

pokerchip[,18] <- r[2]


# night transactions
RawDataIIIPokerChipConversions <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% 
  mutate(night_pokerchip_transactions = ifelse(morning_pokerchip_transactions == 0 & evening_pokerchip_transactions == 0, 1, 0))

s <- RawDataIIIPokerChipConversions %>% group_by(UserID) %>% summarise(night_pokerchip_transaction = sum(night_pokerchip_transactions))

pokerchip[,19] <- s[2]

# Gross gaming revenue 
pokerchip$GGR_Poker <- pokerchip$total_pokerchip_amount_bought - pokerchip$total_pokerchip_amount_sold


# remove unwanted objects from the environment
rm(a,b,c,d,e,f,g,h,i,j,k,q,r,s,v)  



