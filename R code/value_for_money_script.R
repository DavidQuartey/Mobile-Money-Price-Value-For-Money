## Load Libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

## Create function to calculate value for mobile money transfers fees for an amount ####

momo_value_for_money <- function(Amountin = x){
  
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


  ## Loop to derive Transaction Fee (AirtelTigo) ####
 # for(i in seq_along(mobilemoney_value_data$Amountin)){
  #  if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "Tigo Cash" & mobilemoney_value_data$mobilemoney_cashout[[i]] == "Airtel Money" | mobilemoney_value_data$mobilemoney_cashin[[i]] == "Airtel Money" & mobilemoney_value_data$mobilemoney_cashout[[i]] == "Tigo Cash" | mobilemoney_value_data$mobilemoney_cashin[[i]] == mobilemoney_value_data$mobilemoney_cashout[[i]]){
   #   mobilemoney_value_data$transaction_fee[[i]] <- 0.5
    #} else {
     # mobilemoney_value_data$transaction_fee[[i]] <- 0.75
      
#    }
 # }
  
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
  


## Plot value for mobile money results ####

mobilemoney_value_data <- mutate(mobilemoney_value_data, ID = case_when(mobilemoney_value_data$label == "Vodafone Cash to Vodafone Cash" ~ "TRUE",
                                                                    mobilemoney_value_data$label == "Airtel Money to Airtel Money" ~ "TRUE1", 
                                                                    mobilemoney_value_data$label == "Tigo Cash to Tigo Cash" ~ "TRUE2", 
                                                                    mobilemoney_value_data$label == "MTN Momo to MTN Momo" ~ "TRUE3",
                                                                    mobilemoney_value_data$label != c("Vodafone Cash to Vodafone Cash", "Airtel Money to Airtel Money", "Tigo Cash to Tigo Cash", "MTN Momo to MTN Momo") ~ "FALSE"))


title <- paste("What is the value for Mobile Money way of transfering", paste0("GHc", Amountin), "\n across Networks with the introduction of Interopability?")
caption <- paste("By: David Quartey (@DaveQuartey)          \n Source: BoG, MTN, Tigo, Vodafone, Airtel\n Date:", format(as.Date(Sys.Date()), "%d/%m/%Y"),"                                      \n")
subtitle <- "How much will the receiver actually get when (s)he makes a withdrawal?"

#mobilemoney_value_data$Amountout <- mobilemoney_value_data$Amountin - mobilemoney_value_data$Amountout ## if you want to plot fees paid

plot <- ggplot(mobilemoney_value_data, aes(x = reorder(label, Amountout))) + 
  geom_bar(aes(weight = Amountout, fill = ID)) + 
  coord_flip() + 
  geom_text(aes(y = Amountout, label = Amountout, hjust = 1.5), size = 10) +
  labs(title = title,
       subtitle = subtitle, 
       caption = caption) +
  ylab("Amount received after withdrawing (GHc)") + 
  xlab("Mobile Money Service") + 
  theme(title = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 15),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_blank()) +
  scale_fill_manual(values = c("TRUE" = "red", 
                               "TRUE1" = "firebrick1", 
                               "TRUE2" = "blue", 
                               "TRUE3" = "gold",
                               "FALSE" = "gray"))
### Return plot
return(plot)



}


#### Plot value for mobile money of 10 cedis results ####
momo_value_for_money(Amountin = 10)

## Plot value for mobile money of 1000 cedis results ####
momo_value_for_money(Amountin = 1000)




## Create function to calculate value for mobile money transfers fees for an amount ####

momo_value_for_money_fees_paid <- function(Amountin = x){
  
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
  
  
  ## Loop to derive Transaction Fee (AirtelTigo) ####
  # for(i in seq_along(mobilemoney_value_data$Amountin)){
  #  if(mobilemoney_value_data$mobilemoney_cashin[[i]] == "Tigo Cash" & mobilemoney_value_data$mobilemoney_cashout[[i]] == "Airtel Money" | mobilemoney_value_data$mobilemoney_cashin[[i]] == "Airtel Money" & mobilemoney_value_data$mobilemoney_cashout[[i]] == "Tigo Cash" | mobilemoney_value_data$mobilemoney_cashin[[i]] == mobilemoney_value_data$mobilemoney_cashout[[i]]){
  #   mobilemoney_value_data$transaction_fee[[i]] <- 0.5
  #} else {
  # mobilemoney_value_data$transaction_fee[[i]] <- 0.75
  
  #    }
  # }
  
  ## Loop to derive Transaction Fee simple example ####
  for(i in seq_along(mobilemoney_value_data$Amountin)){
    if(mobilemoney_value_data$mobilemoney_cashin[[i]] == mobilemoney_value_data$mobilemoney_cashout[[i]]){
      mobilemoney_value_data$transaction_fee[[i]] <- 0.5
    } else {
      mobilemoney_value_data$transaction_fee[[i]] <- 0.75
      
    }
  }
  
  ## Loop to derive Transaction Fee ####
  for(i in seq_along(mobilemoney_value_data$Amountin)){
    if(mobilemoney_value_data$mobilemoney_cashin[[i]] == mobilemoney_value_data$mobilemoney_cashout[[i]] & mobilemoney_value_data$mobilemoney_cashin[[i]] == "Vodafone Cash"){
      if(mobilemoney_value_data$AMountin <=50){
      mobilemoney_value_data$transaction_fee[[i]] <- 0.5
      }
    } else {
     if(mobilemoney_value_data$transaction_fee[[i]] == )
      
    }
  }
  
  # Calculate Amount left after factoring in fees ####  
  mobilemoney_value_data$Amountout <- mobilemoney_value_data$Amountin - mobilemoney_value_data$cashin_fee - mobilemoney_value_data$transaction_fee - mobilemoney_value_data$cashout_fee  
  
  # Calculate Amount paid in fees ####
  mobilemoney_value_data$Amountout <- mobilemoney_value_data$Amountin - mobilemoney_value_data$Amountout
  
  # Create labels for each mobile money combination
  mobilemoney_value_data$label <- paste(mobilemoney_value_data$mobilemoney_cashin, "to", mobilemoney_value_data$mobilemoney_cashout)
  
  
  
  ## Plot value for mobile money results ####
  
  mobilemoney_value_data <- mutate(mobilemoney_value_data, ID = case_when(mobilemoney_value_data$label == "Vodafone Cash to Vodafone Cash" ~ "TRUE",
                                                                          mobilemoney_value_data$label == "Airtel Money to Airtel Money" ~ "TRUE1", 
                                                                          mobilemoney_value_data$label == "Tigo Cash to Tigo Cash" ~ "TRUE2", 
                                                                          mobilemoney_value_data$label == "MTN Momo to MTN Momo" ~ "TRUE3",
                                                                          mobilemoney_value_data$label != c("Vodafone Cash to Vodafone Cash", "Airtel Money to Airtel Money", "Tigo Cash to Tigo Cash", "MTN Momo to MTN Momo") ~ "FALSE"))
  
  
  title <- paste("What is the value for Mobile Money way of transfering", paste0("GHc", Amountin), "\n across Networks with the introduction of Interopability?")
  caption <- paste("By: David Quartey (@DaveQuartey)          \n Source: BoG, MTN, Tigo, Vodafone, Airtel\n Date:","25/05/2018","                                      \n")
  subtitle <- "How much will be paid in fees in total?"
  
  #mobilemoney_value_data$Amountout <- mobilemoney_value_data$Amountin - mobilemoney_value_data$Amountout ## if you want to plot fees paid
  
  plot <- ggplot(mobilemoney_value_data, aes(x = reorder(label, -Amountout))) + 
    geom_bar(aes(weight = Amountout, fill = ID)) + 
    coord_flip() + 
    geom_text(aes(y = Amountout, label = Amountout, hjust = 1.5), size = 10) +
    labs(title = title,
         subtitle = subtitle, 
         caption = caption) +
    ylab("Amount paid after withdrawing (GHc)") + 
    xlab("Mobile Money Service") + 
    theme(title = element_text(size = 15), 
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15), 
          axis.text = element_text(size = 15),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.text.x = element_blank()) +
    scale_fill_manual(values = c("TRUE" = "red", 
                                 "TRUE1" = "firebrick1", 
                                 "TRUE2" = "blue", 
                                 "TRUE3" = "gold",
                                 "FALSE" = "gray"))
  ### Return plot
  return(plot)
  
  
  
}

#### Plot value for mobile money of 1000 cedis results ####
momo_value_for_money_fees_paid(1000)






#------------------------------------------------- Alternative Approach -------------------------------------------------#
# Load Libraries
library(purrr)
library(ggplot2)
library(dplyr)



momo_value_for_money <- function(network_from, network_to , initial_amount){ # Accept inputs about 
  
# Aims of function
  # columns for network combination - Done
  # columns for amount obtained after withdrawing - Done
    # Where I left off: Check new tariffs for same network transactions - Done
    # Check MTN Tariffs - Done
  # Change 0 - 50 to 1 - 50 - Done
  # Obtain airteltigo to airteltigo rates to do airteltigo section in transaction (withdrawal section that of MTN so have to do too) - Done
  # Check MTN to another network rates - Done
  # Update Vodafobe to Vodafone charges with new info found on website - saved in mobile money folder - Done
  # Create table of price ranges and interoperability charges for each telco - Add new Vodafone charges & add range accross all to accomodate all
  # Add < 50 which can handle less than 1 initial amounts
  # Change Vodafone and Airtel colors in graphs
  
  
  #### TRANSFER STARTS FROM HERE ####
  
    
    if(network_from == "Vodafone Cash"){ #### From Vodafone Cash ####
    if(dplyr::between(initial_amount, 1, 50)){
      if(network_from == network_to){
        
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.5, effect = "flat")
        
         
        
      } else if(network_from != network_to){
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.5, effect = "flat")
         
      }
      
      
    }
      
      if(dplyr::between(initial_amount, 50.1, 75)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.75, effect = "flat")
          
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
          
          
        }
        
      }
      
    if(dplyr::between(initial_amount, 75.1, 100)){
      if(network_from == network_to){
        
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "flat")
         
        
        
      } else if(network_from != network_to){
        
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
         
        
      }
        
    } 
    
    if(dplyr::between(initial_amount, 100.1, 250)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
    }
      
    if(dplyr::between(initial_amount, 250.1, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
    if(dplyr::between(initial_amount, 500.1, 1000)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.50, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
    }
  
    if(network_from == "AirtelTigo Money"){ #### From AirtelTigo ####
      if(dplyr::between(initial_amount, 1, 50)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.5, effect = "flat")
           
          
        } else if(network_from != network_to){
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.75, effect = "flat")
           
        }
        
        
      }
      
      if(dplyr::between(initial_amount, 50.1, 100)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      } 
      
      if(dplyr::between(initial_amount, 100.1, 250)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 250.1, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 500.1, 1000)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.50, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
    
    
    }
  
    if(network_from == "MTN Momo"){ #### From MTN Momo ####
      if(dplyr::between(initial_amount, 1, 50)){
        
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.5, effect = "flat")
           
          
        } else if(network_from != network_to){
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.75, effect = "flat")
           
        }
        
        
      }
      
      if(dplyr::between(initial_amount, 50.1, 100)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      } 
      
      if(dplyr::between(initial_amount, 100.1, 250)){
        
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 250.1, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 500.1, 1000)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
    
    
    }
  
  
  
  
  
#### WITHDRAWAL STARTS FROM HERE ####
  
  if(network_to == "Vodafone Cash"){ #### To Vodafone
    if(dplyr::between(remaining_amount, 1, 50)){
      
        withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 0.5, effect = "flat")
        return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
    
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.1, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1.5, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    } 
    
    if(dplyr::between(remaining_amount, 100.1, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 2.5, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 250.1, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 4.0, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 500.1, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 6.0, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    
  }
  
  
  
  if(network_to == "AirtelTigo Money"){ #### To AirtelTigo
    if(dplyr::between(remaining_amount, 1, 50)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 0.5, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.1, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    } 
    
    if(dplyr::between(remaining_amount, 100.1, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1.50, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 250.1, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 2.00, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 500.1, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 2.50, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    
  }
  
  
  if(network_to == "MTN Momo"){ #### To MTN Momo
    if(dplyr::between(remaining_amount, 1, 50)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 0.5, effect = "flat")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.1, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    } 
    
    if(dplyr::between(remaining_amount, 100.1, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 250.1, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
    }
    
    if(dplyr::between(remaining_amount, 500.1, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
      return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      
      
    }
    
    
  }
  

    
    
}


momo_transfer <- function(initial_amount, transaction_fee , effect = c("flat", "percentage")){
    
    if(effect == "percentage"){
    
    transaction_fee <- transaction_fee/100
    
    amount_after_transaction <- initial_amount - (initial_amount * transaction_fee)
    
    return(amount_after_transaction)
    
    
    } else if(effect == "flat"){
      
      transaction_fee <- transaction_fee
      
      amount_after_transaction <- initial_amount - transaction_fee
      
      return(amount_after_transaction)
      
    }
    
  
}


momo_withdrawal <- function(remaining_amount, withdrawal_fee , effect = c("flat", "percentage")){
  
  if(effect == "percentage"){
    
    withdrawal_fee <- withdrawal_fee/100
    
    amount_after_withdrawal <- remaining_amount - (remaining_amount * withdrawal_fee)
    
    return(amount_after_withdrawal)
    
    
  } else if(effect == "flat"){
    
    withdrawal_fee <- withdrawal_fee
    
    amount_after_withdrawal <- remaining_amount - withdrawal_fee
    
    return(amount_after_withdrawal)
    
  }
  
  
}


#### Testing functions with purrr ####


library(ggplot2)
library(magrittr)
library(ggrepel)


initial_amount <- 10


across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
  data.frame(stringsAsFactors = F) %>% 
  dplyr::rename(network_from = X1, network_to = X2)

same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)

all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations) %>% 
  data.frame(initial_amount = rep(initial_amount, nrow(.))) %>% 
  purrr::pmap_df(momo_value_for_money)
  
# Plot for amount paid in fees
 all_network_permutations %>% 
  ggplot(aes(x = forcats::fct_reorder(Mobile_Money_Service, withdrawn_amount))) + 
    geom_bar(aes(weight = initial_amount - withdrawn_amount)) + 
   geom_text(aes(y = (initial_amount - withdrawn_amount), label = (initial_amount - withdrawn_amount)), size = 10, hjust = 1.5) +
   coord_flip() + 
   theme(legend.position = "none") +
   scale_color_manual(
     values = c("Vodafone Cash to Vodafone Cash" = "firebrick4",
                "Vodafone Cash to MTN Momo" = "dimgrey",
                "Vodafone Cash to AirtelTigo Money" = "dimgrey",
                "MTN Momo to Vodafone Cash" = "dimgrey",
                "MTN Momo to MTN Momo" = "goldenrod1",
                "MTN Momo to AirtelTigo Money" = "dimgrey",
                "AirtelTigo Money to Vodafone Cash" = "dimgrey",
                "AirtelTigo Money to MTN Momo" = "dimgrey",
                "AirtelTigo Money to AirtelTigo Money" = "red")
   ) + 
   theme_minimal() + 
   labs(x = "Mobile Money Service", y = "Transfer Amount (GHc)")

 # cum for amount withdrawn 
  all_network_permutations %>% 
    split(.$Mobile_Money_Service) %>% 
    purrr::map(.f = ~purrr::map(.x, ~rep(.x, 10))) %>% 
    purrr::map(dplyr::as_data_frame) %>%
    dplyr::bind_rows() %>% dplyr::group_by(Mobile_Money_Service) %>% 
    dplyr::mutate(cum_withdrawn_amount = cumsum(withdrawn_amount)) %>% 
    dplyr::mutate(n = 1:10) %>% 
    ggplot(aes(n, cum_withdrawn_amount)) + geom_line(aes(color = Mobile_Money_Service))
  

# cum for amount actually paid
  
  all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations) %>% 
    data.frame(initial_amount = rep(initial_amount, nrow(.))) %>% 
    purrr::pmap_df(momo_value_for_money)
  
  all_network_permutations %>% 
    dplyr::mutate(withdrawn_amount =initial_amount - withdrawn_amount) %>% 
    split(.$Mobile_Money_Service) %>% 
    purrr::map(.f = ~purrr::map(.x, ~rep(.x, 12))) %>% 
    purrr::map(dplyr::as_data_frame) %>%
    dplyr::bind_rows() %>% 
    dplyr::group_by(Mobile_Money_Service) %>% 
    dplyr::mutate(cum_withdrawn_amount = cumsum(withdrawn_amount), n = 1:12) %>% 
    ggplot(aes(n, cum_withdrawn_amount)) + 
    geom_line(aes(color = Mobile_Money_Service), size = 1.5, alpha = 0.8) +
    scale_color_manual(
      values = c("Vodafone Cash to Vodafone Cash" = "firebrick4",
                 "Vodafone Cash to MTN Momo" = "dimgrey",
                 "Vodafone Cash to AirtelTigo Money" = "dimgrey",
                 "MTN Momo to Vodafone Cash" = "dimgrey",
                 "MTN Momo to MTN Momo" = "goldenrod1",
                 "MTN Momo to AirtelTigo Money" = "dimgrey",
                 "AirtelTigo Money to Vodafone Cash" = "dimgrey",
                 "AirtelTigo Money to MTN Momo" = "dimgrey",
                 "AirtelTigo Money to AirtelTigo Money" = "red")
      ) + 
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis( ~.,breaks = cum_fees_ends)) +
    ggtitle(label = glue::glue("How much is paid in fees across each Mobile Money Service pair, if we sent {initial_amount}")) +
    labs(x = "Number of months", y = "Cummulative Mobile Money Fees") +
    annotate(geom = "text", x = 14, y = 25, label = "AirtelTigo Money to AirtelTigo Money", color = "red", size = 3.5)
  
  
  
  
all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations) %>% 
    data.frame(initial_amount = rep(initial_amount, nrow(.))) %>% 
    purrr::pmap_df(momo_value_for_money)
  
 cum_fees_ends <-  all_network_permutations %>% 
    dplyr::mutate(fees_paid =initial_amount - withdrawn_amount) %>% 
    split(.$Mobile_Money_Service) %>% 
    purrr::map(.f = ~purrr::map(.x, ~rep(.x, 12))) %>% 
    purrr::map(dplyr::as_data_frame) %>%
    dplyr::bind_rows() %>% 
    dplyr::group_by(Mobile_Money_Service) %>% 
    dplyr::mutate(cum_fees_paid = cumsum(fees_paid), n = 1:12) %>%
    dplyr::group_by(Mobile_Money_Service) %>% 
    dplyr::top_n(n = 1) %>% 
    dplyr::pull(cum_fees_paid)
  

 ## Mobile Money Pricing Structure in Percentage Terms

 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 number_of_times <- 1000
 same_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = ((initial_amount - withdrawn_amount)/initial_amount) * 100) %>% # Redo this as it starts from 2, must be on
   ggplot(aes(initial_amount, percentage)) + geom_line(aes(color = Mobile_Money_Service)) + scale_y_continuous(limits = c(0, 5))
   
 
 # Mobile Money Structure for all possible money transfer combinnations ( Average Price )
 across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
   data.frame(stringsAsFactors = F) %>% 
   dplyr::rename(network_from = X1, network_to = X2)
 
 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 
 all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations)
   
 
 number_of_times <- 1000
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::ungroup() %>% 
   dplyr::group_by(initial_amount) %>%  
   dplyr::mutate(highest_percentage = max(percentage), lowest_percentage = min(percentage), mean_percentage = mean(percentage)) %>% 
   dplyr::ungroup() %>% 
   ggplot(aes(initial_amount, percentage)) + 
   geom_line(aes(color = Mobile_Money_Service), size = 2) + 
   geom_line(aes(x = initial_amount, y = highest_percentage), color = "black", size = 2) +
   geom_line(aes(x = initial_amount, y = lowest_percentage), color = "black", size = 2) +
   geom_line(aes(x = initial_amount, y = mean_percentage), color = "red", size = 2) +
   geom_vline(xintercept = 375, color = "black", linetype = 1, size = 1.5) +
   geom_point(aes(x = 375, y = upper_fee_boundary), color = "black", size = 4)  +
   geom_point(aes(x = 375, y = lower_fee_boundary), color = "black", size = 4)  +
   geom_curve(aes(xend = 385, yend = upper_fee_boundary + 0.001, y = 0.0325, x = 625), arrow = arrow(ends = "last", length = unit(0.03, "npc"), type = "closed"), size = 1) +
   #scale_y_continuous(limits = c(0, .5)) + 
   #scale_color_viridis_d() + 
   #geom_smooth(method = "loess", se = FALSE, color = "red", size = 2) +
   scale_color_manual(
     values = c("Vodafone Cash to Vodafone Cash" = "dimgrey",
                "Vodafone Cash to MTN Momo" = "dimgrey",
                "Vodafone Cash to AirtelTigo Money" = "dimgrey",
                "MTN Momo to Vodafone Cash" = "dimgrey",
                "MTN Momo to MTN Momo" = "dimgrey",
                "MTN Momo to AirtelTigo Money" = "dimgrey",
                "AirtelTigo Money to Vodafone Cash" = "dimgrey",
                "AirtelTigo Money to MTN Momo" = "dimgrey",
                "AirtelTigo Money to AirtelTigo Money" = "dimgrey")
   ) + 
   annotate(geom = "text", x = 505, y = .022, label = "Average Price", color = "red") +
   theme_minimal() + 
   theme(legend.position = "none") +
   labs(y = "Percentage Paid in Fees", x = "Transfer Amount (GHc)", title = "Mobile Money Fee Structure") +
    scale_y_continuous(labels = function(x){ paste0(round(x * 100, 0), "%")}, limits = c(0, 0.05))
 
 
 
upper_fee_boundary <-  all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::ungroup() %>% 
   dplyr::group_by(initial_amount) %>%  
   dplyr::mutate(highest_percentage = max(percentage), lowest_percentage = min(percentage), mean_percentage = mean(percentage)) %>% 
   dplyr::filter(initial_amount == 375) %>% 
   dplyr::pull(highest_percentage) %>% unique()

lower_fee_boundary <-  all_network_permutations %>% 
  split(.$network_from) %>% 
  purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
  purrr::map(dplyr::as_data_frame) %>% 
  dplyr::bind_rows() %>% 
  dplyr::group_by(network_from, network_to) %>% 
  dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
  purrr::pmap_df(momo_value_for_money) %>% 
  dplyr::group_by(Mobile_Money_Service) %>% 
  dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
  dplyr::ungroup() %>% 
  dplyr::group_by(initial_amount) %>%  
  dplyr::mutate(highest_percentage = max(percentage), lowest_percentage = min(percentage), mean_percentage = mean(percentage)) %>% 
  dplyr::filter(initial_amount == 375) %>% 
  dplyr::pull(lowest_percentage) %>% unique()

 
 
 # Mobile Money Structure for all possible money transfer combinations ( All Combinations )
 across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
   data.frame(stringsAsFactors = F) %>% 
   dplyr::rename(network_from = X1, network_to = X2)
 
 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 
 all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations)
 
 
 number_of_times <- 1000
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount, x = 1010 - .0018, 
                 y = dplyr::case_when(Mobile_Money_Service == "Vodafone Cash to Vodafone Cash" ~ 0.00850 - .0018,
                                      Mobile_Money_Service == "Vodafone Cash to MTN Momo" ~ 0.0250 - .0018,
                                      Mobile_Money_Service == "Vodafone Cash to AirtelTigo Money" ~ 0.0175 - .0018,
                                      Mobile_Money_Service == "MTN Momo to Vodafone Cash" ~ 0.0210 - .0018,
                                      Mobile_Money_Service == "MTN Momo to MTN Momo" ~ 0.0199 - .0018,
                                      Mobile_Money_Service == "MTN Momo to AirtelTigo Money" ~ 0.0175 - .0018,
                                      Mobile_Money_Service == "AirtelTigo Money to Vodafone Cash" ~ 0.0210 - .0018,
                                      Mobile_Money_Service == "AirtelTigo Money to MTN Momo" ~ 0.0248 - .0018,
                                      Mobile_Money_Service == "AirtelTigo Money to AirtelTigo Money" ~ 0.00500 - .0018), 
                 breaks = dplyr::case_when(Mobile_Money_Service == "Vodafone Cash to Vodafone Cash" ~ paste0(round(0.00850 * 100, 1), "%"), 
                                           Mobile_Money_Service == "MTN Momo to MTN Momo" ~ paste0(round(0.0199 * 100, 1), "%"),
                                           Mobile_Money_Service == "Vodafone Cash to AirtelTigo Money" ~ paste0(round(0.0175 * 100, 1), "%"),
                                           Mobile_Money_Service == "MTN Momo to Vodafone Cash" ~ paste0(round(0.0210 * 100, 1), "%"),
                                           Mobile_Money_Service == "Vodafone Cash to MTN Momo" ~ paste0(round(0.025 * 100, 1), "%"),
                                           Mobile_Money_Service == "MTN Momo to AirtelTigo Money" ~ paste0(round(0.0175 * 100, 1), "%"),
                                           Mobile_Money_Service == "AirtelTigo Money to Vodafone Cash" ~ paste0(round(0.0210 * 100, 1), "%"),
                                           Mobile_Money_Service == "AirtelTigo Money to MTN Momo" ~ paste0(round(0.0248 * 100, 1), "%"),
                                           Mobile_Money_Service == "AirtelTigo Money to AirtelTigo Money" ~ paste0(round(0.00500 * 100, 1), "%"))) %>% # Redo this as it starts from 2, must be on
   ggplot(aes(initial_amount, percentage)) + 
   geom_line(aes(color = Mobile_Money_Service), size = 2) + 
   scale_y_continuous(limits = c(0, 5)) + 
   #scale_color_viridis_d()+
   labs(y = "Percentage Paid in Fees", x = "Transfer Amount (GHc)", title = "Mobile Money Fee Structure") +
   scale_y_continuous(labels = function(x){ paste0(round(x * 100, 0), "%")}, limits = c(0, 0.05)) +
   scale_x_continuous(expand = c(0,0)) + 
   theme_minimal() + 
  facet_wrap(.~Mobile_Money_Service, scales = "free") + 
   scale_color_manual(
     values = c("Vodafone Cash to Vodafone Cash" = "firebrick4",
                "Vodafone Cash to MTN Momo" = "dimgrey",
                "Vodafone Cash to AirtelTigo Money" = "dimgrey",
                "MTN Momo to Vodafone Cash" = "dimgrey",
                "MTN Momo to MTN Momo" = "goldenrod1",
                "MTN Momo to AirtelTigo Money" = "dimgrey",
                "AirtelTigo Money to Vodafone Cash" = "dimgrey",
                "AirtelTigo Money to MTN Momo" = "dimgrey",
                "AirtelTigo Money to AirtelTigo Money" = "red")
   ) + 
   theme(legend.position = "none") + 
   ggrepel::geom_text_repel(data = qq, 
                            aes(x = x, y = y, label = breaks), 
                            color = c("red", "dimgrey", "dimgrey", "dimgrey", "goldenrod1", "dimgrey", "dimgrey", "dimgrey", "firebrick4"))
 
 
momo_fee_structure_ends <- all_network_permutations %>% 
  split(.$network_from) %>% 
  purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
  purrr::map(dplyr::as_data_frame) %>% 
  dplyr::bind_rows() %>% 
  dplyr::group_by(network_from, network_to) %>% 
  dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
  purrr::pmap_df(momo_value_for_money) %>% 
  dplyr::group_by(Mobile_Money_Service) %>% 
  dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
  dplyr::top_n(percentage,n = -1) %>% 
  dplyr::pull(percentage)
 
 # Highest and Lowest possible percentage at each cedi
 
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::ungroup() %>% 
   dplyr::group_by(initial_amount) %>%  
   dplyr::mutate(highest_percentage = max(percentage), lowest_percentage = min(percentage)) %>% 
   dplyr::ungroup() %>% 
   ggplot(aes(initial_amount, highest_percentage)) + 
   geom_line(size = 2) +
   geom_line(aes(initial_amount, lowest_percentage), size = 2) + 
   scale_y_continuous(limits = c(0, 5)) + 
   scale_color_viridis_c()+
   labs(y = "Percentage Paid in Fees", x = "Transfer Amount (GHc)", title = "Mobile Money Fee Structure") +
   scale_y_continuous(labels = function(x){ paste0(round(x * 100, 0), "%")}, limits = c(0, 0.05)) +
   scale_x_continuous(expand = c(0,0)) + 
   theme_minimal() + 
   theme(legend.position = "none")
 
 
 # Highest and Lowest possible percentage at each cedi (and how it deviates froms 0)
 
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::ungroup() %>% 
   dplyr::group_by(initial_amount) %>%  
   dplyr::mutate(highest_percentage = max(percentage), lowest_percentage = min(percentage), gap = highest_percentage - lowest_percentage) %>% 
   dplyr::ungroup() %>% 
   ggplot(aes(initial_amount, gap)) + 
   geom_line(size = 2) +
   scale_y_continuous(limits = c(0, 5)) + 
   scale_color_viridis_c()+
   labs(y = "Percentage Paid in Fees", x = "Transfer Amount (GHc)", title = "Mobile Money Fee Structure") +
   scale_y_continuous(labels = function(x){ paste0(round(x * 100, 0), "%")}, limits = c(0, 0.05)) +
   scale_x_continuous(expand = c(0,0)) + 
   theme_minimal() + 
   theme(legend.position = "none")
 
 
 
 
 
 
 #### SD at each initial amount ####
 
 across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
   data.frame(stringsAsFactors = F) %>% 
   dplyr::rename(network_from = X1, network_to = X2)
 
 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 
 all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations)
 
 
 number_of_times <- 1000
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::group_by(initial_amount) %>% 
   dplyr::mutate(sd_percentage = sd(percentage)) %>% 
   dplyr::ungroup() %>% 
   ggplot(aes(initial_amount, sd_percentage)) + geom_line()
 
 
 #### Linear relationship between SD at each initial amount ####
 
 across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
   data.frame(stringsAsFactors = F) %>% 
   dplyr::rename(network_from = X1, network_to = X2)
 
 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 
 all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations)
 
 
 number_of_times <- 1000
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = (initial_amount - withdrawn_amount)/initial_amount) %>% # Redo this as it starts from 2, must be on
   dplyr::group_by(initial_amount) %>% 
   dplyr::mutate(sd_percentage = sd(percentage)) %>% 
   dplyr::ungroup() %>% 
   lm(sd_percentage ~ initial_amount, data = .) %>% summary()
 
 
 #### mean and median at each initial amount ####
 
 across_network_permutations <- gtools::permutations(n = 3, r = 2, v = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money")) %>% 
   data.frame(stringsAsFactors = F) %>% 
   dplyr::rename(network_from = X1, network_to = X2)
 
 same_network_permutations <- data.frame(network_from = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), network_to = c("Vodafone Cash", "MTN Momo", "AirtelTigo Money"), stringsAsFactors = FALSE)
 
 all_network_permutations <- dplyr::bind_rows(across_network_permutations, same_network_permutations)
 
 
 number_of_times <- 1000
 all_network_permutations %>% 
   split(.$network_from) %>% 
   purrr::map(~purrr::map(.x,.f = ~rep(.x, number_of_times))) %>% # number of times to repeat each column
   purrr::map(dplyr::as_data_frame) %>% 
   dplyr::bind_rows() %>% 
   dplyr::group_by(network_from, network_to) %>% 
   dplyr::mutate(initial_amount = 1:number_of_times) %>% # Fill each column from 1 to the maximum number
   purrr::pmap_df(momo_value_for_money) %>% 
   dplyr::group_by(Mobile_Money_Service) %>% 
   dplyr::mutate(initial_amount = 2:number_of_times, percentage = ((initial_amount - withdrawn_amount)/initial_amount) * 100) %>% # Redo this as it starts from 2, must be on
   dplyr::group_by(initial_amount) %>% 
   dplyr::mutate(mean_percentage = mean(percentage) , median_percentage = median(percentage)) %>% 
   dplyr::ungroup() %>% 
   ggplot(aes(initial_amount, mean_percentage)) + geom_line(color = "red") + geom_line(aes(initial_amount, median_percentage)) + ylim(c(0, 5))
 
 