#-------------------------------------------------------------------#
# Merge All 4 datasets to create one base table
# Demographic table is considered as Main Table
# Left join is performed between Demographic and remaining tables
#-------------------------------------------------------------------#

# Import continents


# merge demographic and user_daily_aggregation
merge1 <- merge(x = demographic, y = user_daily_aggregation, by = "UserID", all.x = TRUE)

# merge with analytical
merge2 <- merge(x = merge1, y = Analytical, by.x = "UserID" , by.y = "USERID", all.x = TRUE)

# merge with pokerchip
final_table <- merge(x = merge2, y = pokerchip, by = "UserID", all.x = TRUE)


# Replace all NAs in  numeric columns with 0s
x <- dplyr::select_if(final_table, is.numeric)
y <- dplyr::select_if(final_table, is.character)
z <- dplyr::select_if(final_table, is.Date)

x <- replace(x, is.na(x), 0)
y$Gender <- replace(y$Gender, is.na(y$Gender),"Male")

final_table <- cbind(x,y,z) 

# Last active date of the user
final_table[, "Last_Active_Date"] <- as.Date(apply(final_table[, c("max_date_sports", 
                                                                   "max_pokerchip_transact_date")], 
                                                   1, max, na.rm=TRUE),"%Y-%m-%d")
                                                   
# Last active date of the user
final_table[, "First_Active_Date"] <- as.Date(apply(final_table[, c("min_date_sports", 
                                                                   "min_pokerchip_transact_date")], 
                                                   1, max, na.rm=TRUE),"%Y-%m-%d")

# First Activity Lag
final_table[, "First_Activity_lag"] <- as.numeric(difftime(final_table$First_Active_Date,final_table$RegDate, units = "days"))

# length of relation ship of the user
final_table[,"Length_of_relation"] <- final_table$Last_Active_Date - final_table$FirstPay


#Loyalty
final_table[,"Loyalty"] <- ifelse(as.Date("2005-09-30") - final_table$Last_Active_Date < 40 ,"loyal","not loyal")

# total profits
final_table[,"Gross_Game_Revenue"] <- final_table$GGR_Sports + final_table$GGR_Poker

# overall active days
final_table[,"overall_active_days"] <- final_table$total_days_sports + final_table$total_days_poker_transact

# Life time value
final_table[,"Life_time_value"] <- (((final_table$total_stakes_sports + final_table$total_pokerchip_amount_bought) -  
                                   (final_table$total_winnings_sports + final_table$total_pokerchip_amount_sold))/ 
                            final_table$overall_active_days) * (final_table$Last_Active_Date - final_table$First_Active_Date + 1)
  
#Rename columns

colnames(final_table)[colnames(final_table)=="Application Description"] <- "Application_Description"
colnames(final_table)[colnames(final_table)=="Language Description"] <- "Language_Description"

# Remove unwanted variables 

final_table$mean_stakes_sports <- NULL
final_table$min_stakes_sports <- NULL
final_table$max_stakes_sports<- NULL
final_table$mean_winnings_sports <- NULL
final_table$max_winnings_sports <- NULL
final_table$min_winnings_sports<- NULL
final_table$mean_bets_sports   <- NULL
final_table$max_bets_sports <- NULL
final_table$min_bets_sports <- NULL
final_table$max_pokerchip_amount_sold <- NULL
final_table$max_pokerchip_amount_bought<- NULL
final_table$min_pokerchip_amount_bought <- NULL
final_table$min_pokerchip_amount_sold <- NULL
final_table$Avg_pokerchip_amount_bought <- NULL  
final_table$Avg_pokerchip_amount_sold <- NULL

# save final table

save(final_table,file="final_table.rdata")