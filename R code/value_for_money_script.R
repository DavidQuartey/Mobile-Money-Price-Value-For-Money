
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
      
      if(dplyr::between(initial_amount, 50.000000000000000000001, 75)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 0.75, effect = "flat")
          
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
          
          
        }
        
      }
      
    if(dplyr::between(initial_amount, 75.000000000000000000001, 100)){
      if(network_from == network_to){
        
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "flat")
         
        
        
      } else if(network_from != network_to){
        
        remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
         
        
      }
        
    } 
    
    if(dplyr::between(initial_amount, 100.000000000000000000001, 250)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
    }
      
    if(dplyr::between(initial_amount, 250.000000000000000000001, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
    if(dplyr::between(initial_amount, 500.000000000000000000001, 1000)){
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
      
      if(dplyr::between(initial_amount, 50.000000000000000000001, 100)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      } 
      
      if(dplyr::between(initial_amount, 100.000000000000000000001, 250)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 250.000000000000000000001, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 2.0, effect = "flat")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 500.000000000000000000001, 1000)){
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
      
      if(dplyr::between(initial_amount, 50.000000000000000000001, 100)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      } 
      
      if(dplyr::between(initial_amount, 100.000000000000000000001, 250)){
        
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 250.000000000000000000001, 500)){
        if(network_from == network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1, effect = "percentage")
           
          
          
        } else if(network_from != network_to){
          
          remaining_amount <- momo_transfer(initial_amount = initial_amount, transaction_fee = 1.5, effect = "percentage")
           
          
        }
        
      }
      
      if(dplyr::between(initial_amount, 500.000000000000000000001, 1000)){
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
        #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
        return(withdrawn_amount)    
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.000000000000000000001, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1.5, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    } 
    
    if(dplyr::between(remaining_amount, 100.000000000000000000001, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 2.5, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 250.000000000000000000001, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 4.0, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 500.000000000000000000001, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 6.0, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    
  }
  
  
  
  if(network_to == "AirtelTigo Money"){ #### To AirtelTigo
    if(dplyr::between(remaining_amount, 1, 50)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 0.80, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.000000000000000000001, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1.50, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    } 
    
    if(dplyr::between(remaining_amount, 100.000000000000000000001, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 2.80, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 250.000000000000000000001, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 4.00, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 500.000000000000000000001, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 6.00, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    
  }
  
  
  if(network_to == "MTN Momo"){ #### To MTN Momo
    if(dplyr::between(remaining_amount, 1, 50)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 0.5, effect = "flat")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
      
    }
    
    
    if(dplyr::between(remaining_amount, 50.000000000000000000001, 100)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    } 
    
    if(dplyr::between(remaining_amount, 100.000000000000000000001, 250)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 250.000000000000000000001, 500)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)      
    }
    
    if(dplyr::between(remaining_amount, 500.000000000000000000001, 1000)){
      
      withdrawn_amount <- momo_withdrawal(remaining_amount = remaining_amount, withdrawal_fee = 1, effect = "percentage")
      #return(data.frame(Mobile_Money_Service = paste(network_from, "to", network_to), withdrawn_amount = withdrawn_amount, stringsAsFactors = FALSE))
      return(withdrawn_amount)
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
 
