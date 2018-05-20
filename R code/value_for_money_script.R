## Load Libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

### Create Data Frame ####
mobilemoney_value_data <- data.frame(
  mobilemoney_cashin = rep(c("MTN Momo", "Vodafone Cash", "Airtel Money", "Tigo Cash"), c(4, 4, 4, 4)),
  mobilemoney_cashout = rep(c("MTN Momo", "Vodafone Cash", "Airtel Money", "Tigo Cash"), 4),
  cashin_fee = rep(0, 16),
  transaction_fee = rep(0, 16),
  cashout_fee = rep(0, 16),
  Amountin = rep(0, 16), 
  Amountout = rep(0, 16))

# Change columns from factors to characters
mobilemoney_value_data$mobilemoney_cashin <- as.character(mobilemoney_value_data$mobilemoney_cashin)
mobilemoney_value_data$mobilemoney_cashout <- as.character(mobilemoney_value_data$mobilemoney_cashout)


## Create function to calculate value for mobile money transfers fees for an amount ####

momo_value_for_money <- function(Amountin = x){
  mobilemoney_value_data$Amountin <- Amountin
  
  ## Cashin Fee ####
  for(i in seq_along(mobilemoney_value_data$Amountin)){
    if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "MTN Momo"){ ####MTN Momo ####
      mobilemoney_value_data$cashin_fee[[i]] <- 0
    } else {
      if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "Vodafone Cash"){ ####Vodafone Cash ####
        mobilemoney_value_data$cashin_fee[[i]] <- 0
      } else {
        if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "Tigo Cash"){ ####Tigo Cash ####
          mobilemoney_value_data$cashin_fee[[i]] <- 0
        } else {
          if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "Airtel Money"){ ####Airtel Money ####
            mobilemoney_value_data$cashin_fee[[i]] <- 0
          }
        }
      }
  }
  }
  
  ## Loop to derive CashOut Fee ####
  for(i in seq_along(mobilemoney_value_data$Amountin)){
    if(mobilemoney_value_data$mobilemoney_cashout[[i]] == "MTN Momo"){ ####MTN Momo ####
      mobilemoney_value_data$cashout_fee[[i]] <- ifelse(between(Amountin, 1 , 50), .5,
                                           ifelse(between(Amountin, 51 , 1000), 0.01*(mobilemoney_value_data$Amountin - mobilemoney_value_data$cashin_fee - mobilemoney_value_data$transaction_fee)))
    } else {
      if(mobilemoney_value_data$mobilemoney_cashout[[i]] == "Vodafone Cash"){ ####Vodafone Cash ####
        mobilemoney_value_data$cashout_fee[[i]] <- ifelse(between(Amountin, 1 , 50), .5,
                                             ifelse(between(Amountin, 51 , 100), 1.50, 
                                                    ifelse(between(Amountin, 101, 250), 2.50,
                                                           ifelse(between(Amountin, 251, 500), 4, 
                                                                          ifelse(between(Amountin, 501, 1000), 6)))))
      } else {
        if(mobilemoney_value_data$mobilemoney_cashout[[i]] == "Tigo Cash"){ ####Tigo Cash ####
          mobilemoney_value_data$cashout_fee[[i]] <- ifelse(between(Amountin, 1 , 50), .8,
                                               ifelse(between(Amountin, 51 , 100), 1.50, 
                                                      ifelse(between(Amountin, 101, 250), 2.80,
                                                             ifelse(between(Amountin, 251, 500), 4, 
                                                                            ifelse(between(Amountin, 501, 1000), 6)))))
        } else {
          if(mobilemoney_value_data$mobilemoney_cashout[[i]] == "Airtel Money"){ ####Airtel Money ####
            mobilemoney_value_data$cashout_fee[[i]] <- ifelse(between(Amountin, 1 , 50), .8,
                                                 ifelse(between(Amountin, 51 , 100), 1.25, 
                                                        ifelse(between(Amountin, 101, 1000), 0.01*(mobilemoney_value_data$Amountin - mobilemoney_value_data$cashin_fee - mobilemoney_value_data$transaction_fee))))
          }
        }
      }
    }
  }



## Loop to derive Transaction Fee ####
for(i in seq_along(mobilemoney_value_data$Amountin)){
  if(mobilemoney_value_data$mobilemoney_cashin[[i]] == mobilemoney_value_data$mobilemoney_cashout[[i]]){
    mobilemoney_value_data$transaction_fee[[i]] <- 0.5
  } else {
    mobilemoney_value_data$transaction_fee[[i]] <- 0.75
    
  }
}

# Calculate Amount left after factoring in fees ####  
mobilemoney_value_data$Amountout <- mobilemoney_value_data$Amountin - mobilemoney_value_data$cashin_fee - mobilemoney_value_data$transaction_fee - mobilemoney_value_data$cashout_fee  

# Create labels for each mobile money combination
mobilemoney_value_data$label <- paste(mobilemoney_value_data$mobilemoney_cashin, "to", mobilemoney_value_data$mobilemoney_cashout)
  
return(mobilemoney_value_data)
  }


## Is my Momo Transfer Value for Money? Lets see

# GHC 10.00
value_for_money_data <- momo_value_for_money(Amountin = 10)


## Plot value for mobile money of 10 cedis results ####

value_for_money_data <- mutate(value_for_money_data, ID = case_when(value_for_money_data$label == "Vodafone Cash to Vodafone Cash" ~ "TRUE",
                                              value_for_money_data$label == "Airtel Money to Airtel Money" ~ "TRUE1", 
                                              value_for_money_data$label == "Tigo Cash to Tigo Cash" ~ "TRUE2", 
                                              value_for_money_data$label == "MTN Momo to MTN Momo" ~ "TRUE3",
                                              value_for_money_data$label != c("Vodafone Cash to Vodafone Cash", "Airtel Money to Airtel Money", "Tigo Cash to Tigo Cash", "MTN Momo to MTN Momo") ~ "FALSE"))

ggplot(value_for_money_data, aes(x = reorder(label, Amountout))) + 
  geom_bar(aes(weight = Amountout, fill = ID)) + 
  coord_flip() + 
  geom_text(aes(y = Amountout, label = Amountout, hjust = 1.5), size = 10) +
  labs(title = "What is the value for Mobile Money way of transfering GHc10 \n across Networks with the introduction of Interopability?",
          subtitle = "How much will the receiver actually get when (s)he makes a withdrawal?", 
       caption = "By: David Quartey (@DaveQuartey)          \n Source: BoG, MTN, Tigo, Vodafone, Airtel\n Date: 14/05/2018                                      \n") +
  ylab("Amount Out (GHc)") + 
  xlab("Mobile Money Service") + 
  theme(title = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 15),
        legend.position = "none") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
  scale_fill_manual(values = c("TRUE" = "red", 
                                 "TRUE1" = "firebrick1", 
                                 "TRUE2" = "blue", 
                                 "TRUE3" = "gold",
                               "FALSE" = "gray"))



## GHc1, 000.00

value_for_money_data <- momo_value_for_money(Amountin = 1000)


## Plot value for mobile money of 1000 cedis results ####

value_for_money_data <- mutate(value_for_money_data, ID = case_when(value_for_money_data$label == "Vodafone Cash to Vodafone Cash" ~ "TRUE",
                                                                    value_for_money_data$label == "Airtel Money to Airtel Money" ~ "TRUE1", 
                                                                    value_for_money_data$label == "Tigo Cash to Tigo Cash" ~ "TRUE2", 
                                                                    value_for_money_data$label == "MTN Momo to MTN Momo" ~ "TRUE3",
                                                                    value_for_money_data$label != c("Vodafone Cash to Vodafone Cash", "Airtel Money to Airtel Money", "Tigo Cash to Tigo Cash", "MTN Momo to MTN Momo") ~ "FALSE"))

ggplot(value_for_money_data, aes(x =reorder(label, Amountout))) + 
  geom_bar(aes(weight = Amountout, fill = ID)) + 
  coord_flip() + 
  geom_text(aes(y = Amountout, label = Amountout, hjust = 1.5), size = 10) +
  labs(title = "What is the value for Mobile Money way of transfering GHc1, 000 \n across Networks with the introduction of Interopability?",
       subtitle = "How much will the receiver actually get when (s)he makes a withdrawal?", 
       caption = "By: David Quartey (@DaveQuartey)          \n Source: BoG, MTN, Tigo, Vodafone, Airtel\n Date: 14/05/2018                                      \n") +
  ylab("Amount Out (GHc)") + 
  xlab("Mobile Money Service") + 
  theme(title = element_text(size = 15), 
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15),
        legend.position = "none") +
  scale_fill_manual(values = c("TRUE" = "red", 
                               "TRUE1" = "firebrick1", 
                               "TRUE2" = "blue", 
                               "TRUE3" = "gold",
                               "FALSE" = "gray"))