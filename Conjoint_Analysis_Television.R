
#Conjoint function with preference vector and design matrix as input
conjoint = function(pref,design_matrix){
  
  # Build linear regression model for preferences vs design matrix
  lm_out <- lm(pref ~ design_matrix)        ##
  
  #Vector to store results on partworth estimation
  partworth_estimates <- data.frame("attribute_levels" = c("screen_52","screen_65","3D_tech","sony_brand","high_price"),"pw_est" = lm_out$coefficients[2:6])  ###Store partworth estimates in data frame                      
  
  #Create vector for different attributes
  screen_values <- c(lm_out$coefficients[2],lm_out$coefficients[3],0)
  technology_values <- c(lm_out$coefficients[4],0)
  brand_values <- c(lm_out$coefficients[5],0)
  price_values <- c(lm_out$coefficients[6],0)
  
  #Compute ranges for different attibutes
  screen_range <- max(screen_values) - min(screen_values)
  technology_range <- max(technology_values) - min(technology_values) 
  brand_range <- max(brand_values) - min(brand_values)
  price_range <- max(price_values) - min(price_values) 

    
  attribute_df <- data.frame(attribute = c("Screen","Technology","Brand","Price"),range = c(screen_range,technology_range,brand_range,price_range)) ###Create a dataframe storing attribute information
  attribute_df$importance <- attribute_df$range*100/sum(attribute_df$range) ###Computing importance for attributes
  
  
  price_per_util <- (2500-2000)/attribute_df['4','range'] ###Computing price per utility
  
  partworth_estimates$wtp <- partworth_estimates$pw_est*price_per_util ###Computing willingness to pay 
  
  calling_df <- data.frame("rank"=c(1,2,3,4,5,6,7,8,9,10,11),
                           "price"=c(1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500)) ###Creating a datframe with different prices
  
  comp_1_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"), 
                              "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,1,0,1,1,2500))    ###Creating competitor brand1 design
  comp_1_utility <- (sum(comp_1_design$pw_est[1:5]*comp_1_design$weigths[1:5])) + (comp_1_design$pw_est[6]*(comp_1_design$weigths[6]-2000)/(2500-2000))  ###Computing utility for competitor brand 1 
  comp_1_attractiveness <- exp(comp_1_utility) ###Computing attractiveness for competitor brand 1 
  
  comp_2_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"), 
                              "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,0,1,1,0,2000)) ###Creating competitor brand2 design
  comp_2_utility <- (sum(comp_2_design$pw_est[1:5]*comp_2_design$weigths[1:5])) + (comp_2_design$pw_est[6]*(comp_2_design$weigths[6]-2000)/(2500-2000)) ###Computing utility for competitor brand 2
  comp_2_attractiveness <- exp(comp_2_utility) ###Computing attractiveness for competitor brand 2
  
  compiled_df <- data.frame("price"=numeric(),"my_design_margin"=numeric(),"my_design_market_share"=numeric(),"my_design_profit_per_unit"=numeric()) ###Initializing empty datframe for my design 
  
  library(dplyr)
  list_loop <- c(1,2,3,4,5,6,7,8,9,10,11)  
  for (i in list_loop){                                               ###Running loop for different prices                 
    price = calling_df %>% filter(rank==i) %>% select('price')
    my_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"),   ###Creating my design dataframe
                            "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,0,1,0,0,price$price))
    my_design_utility <- (sum(my_design$pw_est[1:5]*my_design$weigths[1:5])) + (my_design$pw_est[6]*(my_design$weigths[6]-2000)/(2500-2000)) ###Computing utility for my design
    costs <- matrix(c(1000,500,1000,250,250)) ###Computing costs 
    net_costs <- sum(matrix(my_design$weigths[1:5])*costs)  ###Computing net costs
    my_design_attractiveness <- exp(my_design_utility) ###Computing attractiveness for my design
    
    market_share_df  <- data.frame("company" = c("my_design","comp_1","comp_2"),"utility" = c(my_design_utility,comp_1_utility,comp_2_utility)    ###Creating a datframe to compute market share
                                   ,"attractiveness" = c(my_design_attractiveness,comp_1_attractiveness,comp_2_attractiveness))
    market_share_df$market_share <- market_share_df$attractiveness*100/sum(market_share_df$attractiveness)     ###Computing market share 
    my_design_market_share <- market_share_df$market_share[1] ###Storing my design market share
    my_design_margin <- price$price - net_costs  ###Computing my design margin 
    my_design_profit_per_unit <- my_design_market_share*my_design_margin/100  ###Computing my design profit per unit
    summary_df <- data.frame("price"=c(price$price),"my_design_margin"=c(my_design_margin),"my_design_market_share"=c(my_design_market_share),"my_design_profit_per_unit"=c(my_design_profit_per_unit))  ###Storing market share, margin and profit for given price in dataframe
    compiled_df <- rbind(compiled_df,summary_df)  ###Appending data to initialized dataframe
  }
  
  
  best_design = compiled_df[which.max(compiled_df$my_design_profit_per_unit),]   ###Selecting design with highest profit per unit
  
  optimal_price = as.numeric(best_design$price)    ###Selecting optimal price 
  market_share_optimal_price = as.numeric(best_design$my_design_market_share) ###Selecting market share at optimal price
  max_profit_optimal_price = as.numeric(best_design$my_design_profit_per_unit) ###Selecting profit per unit at optimal price
  
  
  partworth_estimates$pw_out <- paste(partworth_estimates$attribute_levels,round(partworth_estimates$pw_est,2),sep = ' = ') ###Storing partworth estimates with descriptions
  partworth_estimates$wtp_out <- paste(partworth_estimates$attribute_levels,round(partworth_estimates$wtp,2),sep = ' = ')   ###Storing willingness to pay with descriptions
  
  partworth_estimates_answer <- c(partworth_estimates$pw_out)   ###Storing partworth estimates as vector
  intercept <- paste("intercept = ",round(lm_out$coefficients[1],2),sep = '')       ###Getting intercept partworth estimates
  
  partworth_estimates_answer <- paste(intercept,partworth_estimates_answer[1],partworth_estimates_answer[2],    ###Getting all partworth estimates in one vector
                                      partworth_estimates_answer[3],partworth_estimates_answer[4]
                                      ,partworth_estimates_answer[5],sep = ', ')
  
  attribute_df$ai_out <- paste(attribute_df$attribute,round(attribute_df$importance,2),sep = ' = ') ###Storing attribute importance with descriptions
  
  attribute_importance_answer <- c(attribute_df$ai_out) ###Storing attribute importance as vector
  
  attribute_importance_answer <- paste(attribute_importance_answer[1],attribute_importance_answer[2], ###Getting all attribute importance in one vector
                                       attribute_importance_answer[3],attribute_importance_answer[4]
                                       ,sep = ', ')
  
  willingness_to_pay_answer <- c(partworth_estimates$wtp_out)   ###Storing willingness to pay as vector
  
  willingness_to_pay_answer <- paste(willingness_to_pay_answer[1],willingness_to_pay_answer[2],       ###Getting all willingness to pay in one vector
                                     willingness_to_pay_answer[3],willingness_to_pay_answer[4]
                                     ,willingness_to_pay_answer[5],sep = ', ')
  
  #final_output <- data.frame("Outputs" = c("Partworth Estimates","Attribute Importance", "Willingness To Pay", "Optimal Price","Market Share At Optimal Price","Maximum Profit At Optimal Price"),   ###Getting final output dataframe ready
                            # "Elements" = c(partworth_estimates_answer,attribute_importance_answer,willingness_to_pay_answer,optimal_price,market_share_optimal_price,max_profit_optimal_price))
  #return(View(final_output))
  
  final_output_list <- list(c(partworth_estimates_answer),c(attribute_importance_answer),c(willingness_to_pay_answer),c(optimal_price),c(market_share_optimal_price),c(max_profit_optimal_price))      ###Getting final output list ready
  names(final_output_list) <- c("Partworth Estimates","Attribute Importance(%)", "Willingness To Pay($)", "Optimal Price($)","Market Share At Optimal Price(%)","Maximum Profit At Optimal Price($)")
  return(final_output_list)
}

dm <- data.frame("screen_52_inch" = c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0),   ###Initializing design matrix dataframe
                 "screen_65_inch" = c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0),
                 "3D_flag" = c(0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1),
                 "Sony_flag" = c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0),
                 "Price_high_flag" = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1))

dm <- as.matrix(dm) ###Converting design to matrix

preferences <- c(16,23,6,19,24,3,12,22,2,11,21,5,7,14,17,8,13,20,1,10,15,9,4,18) ###Inputting preferences

conjoint(preferences,dm) ###Running conjoint function

