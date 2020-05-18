
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

  #Create a dataframe storing attribute information  
  attribute_df <- data.frame(attribute = c("Screen","Technology","Brand","Price"),range = c(screen_range,technology_range,brand_range,price_range)) 
  #Computing importance for attributes
  attribute_df$importance <- attribute_df$range*100/sum(attribute_df$range) 
  
  #price per utility
  price_per_util <- (2500-2000)/attribute_df['4','range']
  #willingness to pay 
  partworth_estimates$wtp <- partworth_estimates$pw_est*price_per_util
  
  #datframe with different prices
  calling_df <- data.frame("rank"=c(1,2,3,4,5,6,7,8,9,10,11),
                           "price"=c(1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500))
  
  #Creating competitor brand1 design
  comp_1_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"), 
                              "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,1,0,1,1,2500))   
  #Computing attractiveness for competitor brand 1 
  comp_1_utility <- (sum(comp_1_design$pw_est[1:5]*comp_1_design$weigths[1:5])) + (comp_1_design$pw_est[6]*(comp_1_design$weigths[6]-2000)/(2500-2000))  ###Computing utility for competitor brand 1 
  comp_1_attractiveness <- exp(comp_1_utility)
  
  comp_2_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"), 
                              "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,0,1,1,0,2000)) ###Creating competitor brand2 design
  comp_2_utility <- (sum(comp_2_design$pw_est[1:5]*comp_2_design$weigths[1:5])) + (comp_2_design$pw_est[6]*(comp_2_design$weigths[6]-2000)/(2500-2000)) ###Computing utility for competitor brand 2
  comp_2_attractiveness <- exp(comp_2_utility) ###Computing attractiveness for competitor brand 2
  
  compiled_df <- data.frame("price"=numeric(),"my_design_margin"=numeric(),"my_design_market_share"=numeric(),"my_design_profit_per_unit"=numeric()) ###Initializing empty datframe for my design 
  
  library(dplyr)
  list_loop <- c(1,2,3,4,5,6,7,8,9,10,11)  
  #for different prices
  for (i in list_loop){                                                          
    price = calling_df %>% filter(rank==i) %>% select('price')
    my_design <- data.frame("attribute_levels" = c("intercept","screen_52","screen_65","3D_tech","sony_brand","high_price"),
                            "pw_est" = lm_out$coefficients[1:6],"weigths" = c(1,0,1,0,0,price$price))
    my_design_utility <- (sum(my_design$pw_est[1:5]*my_design$weigths[1:5])) + (my_design$pw_est[6]*(my_design$weigths[6]-2000)/(2500-2000))
    
    #costs 
    costs <- matrix(c(1000,500,1000,250,250))
    net_costs <- sum(matrix(my_design$weigths[1:5])*costs)
    #attractiveness for my design
    my_design_attractiveness <- exp(my_design_utility) 
    
    #market share
    market_share_df  <- data.frame("company" = c("my_design","comp_1","comp_2"),"utility" = c(my_design_utility,comp_1_utility,comp_2_utility)    ###Creating a datframe to compute market share
                                   ,"attractiveness" = c(my_design_attractiveness,comp_1_attractiveness,comp_2_attractiveness))
    market_share_df$market_share <- market_share_df$attractiveness*100/sum(market_share_df$attractiveness)
    my_design_market_share <- market_share_df$market_share[1]
    
    #design margin 
    my_design_margin <- price$price - net_costs  ##
    
    #profit per unit
    my_design_profit_per_unit <- my_design_market_share*my_design_margin/100  
    #final results' dataframe
    summary_df <- data.frame("price"=c(price$price),"my_design_margin"=c(my_design_margin),"my_design_market_share"=c(my_design_market_share),"my_design_profit_per_unit"=c(my_design_profit_per_unit))  ###Storing market share, margin and profit for given price in dataframe
    compiled_df <- rbind(compiled_df,summary_df) 
  }
  
  #best/optimal factor
  best_design = compiled_df[which.max(compiled_df$my_design_profit_per_unit),] 
  optimal_price = as.numeric(best_design$price)
  market_share_optimal_price = as.numeric(best_design$my_design_market_share)
  max_profit_optimal_price = as.numeric(best_design$my_design_profit_per_unit)
  
  
  partworth_estimates$pw_out <- paste(partworth_estimates$attribute_levels,round(partworth_estimates$pw_est,2),sep = ' = ')
  partworth_estimates$wtp_out <- paste(partworth_estimates$attribute_levels,round(partworth_estimates$wtp,2),sep = ' = ')
  
  partworth_estimates_answer <- c(partworth_estimates$pw_out) 
  intercept <- paste("intercept = ",round(lm_out$coefficients[1],2),sep = '') 
  
  partworth_estimates_answer <- paste(intercept,partworth_estimates_answer[1],partworth_estimates_answer[2], 
                                      partworth_estimates_answer[3],partworth_estimates_answer[4]
                                      ,partworth_estimates_answer[5],sep = ', ')
  
  attribute_df$ai_out <- paste(attribute_df$attribute,round(attribute_df$importance,2),sep = ' = ') 
  
  attribute_importance_answer <- c(attribute_df$ai_out)
  
  attribute_importance_answer <- paste(attribute_importance_answer[1],attribute_importance_answer[2], 
                                       attribute_importance_answer[3],attribute_importance_answer[4]
                                       ,sep = ', ')
  
  willingness_to_pay_answer <- c(partworth_estimates$wtp_out)  
  
  willingness_to_pay_answer <- paste(willingness_to_pay_answer[1],willingness_to_pay_answer[2],      
                                     willingness_to_pay_answer[3],willingness_to_pay_answer[4]
                                     ,willingness_to_pay_answer[5],sep = ', ')  
  final_output_list <- list(c(partworth_estimates_answer),c(attribute_importance_answer),c(willingness_to_pay_answer),c(optimal_price),c(market_share_optimal_price),c(max_profit_optimal_price))      ###Getting final output list ready
  names(final_output_list) <- c("Partworth Estimates","Attribute Importance(%)", "Willingness To Pay($)", "Optimal Price($)","Market Share At Optimal Price(%)","Maximum Profit At Optimal Price($)")
  return(final_output_list)
}

dm <- data.frame("screen_52_inch" = c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0),   
                 "screen_65_inch" = c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0),
                 "3D_flag" = c(0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1),
                 "Sony_flag" = c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0),
                 "Price_high_flag" = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1))

dm <- as.matrix(dm)
preferences <- c(16,23,6,19,24,3,12,22,2,11,21,5,7,14,17,8,13,20,1,10,15,9,4,18)

conjoint(preferences,dm) 

