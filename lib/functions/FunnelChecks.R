FunnelChecks <- function(dataset) {
  
  # function that does the checks for Chapter Funnel
  # 
  # Parameters used:
  # dataset = the provided dataset
  # franciseCodes = vector containg franchises
  # 
  # returns the same dataset with the added checks 
  
  ### request 1- 4 all in 1 loop 
  # Overall_Brand_Awareness → Headset_brand_consideration
  # If 3.2_Brand# == 2, then 4.A.1_Brand# =/= 1
  
  # Headset_brand_consideration → headset_brand_preference
  # If 4.A.1_Brand# == 2, then 4.A.2 =/= Brand#
  
  # Overall_Brand_Awareness → Headset_brand_ownership
  # If 3.2_Brand# == 2, then 4.A.3_Brand# =/= 1
  
  # Headset_brand_ownership → Headset_brand_loyalty
  # If 4.A.3_Brand# == 0, then 4.A.4_Brand# =/= 1
  
  # vector of franchise brands 
  # valueVector <- c(1:35)
  valueVector <- c(1:5)
  
  # loop for all brands 
  for (value in valueVector){
    
    # create the command to add column for request 1 
    cdmColumnCreation1 <- paste0('dataset$reviewFunnel_1_',
                                 value,
                                 '[dataset$Q3_2_',
                                 value,
                                 ' == 2 & dataset$Q4A1_',
                                 value,
                                 ' != 1] <- "review-in"')
    
    # create the command to add column for request 2
    cdmColumnCreation2 <- paste0('dataset$reviewFunnel_2_',
                                 value,
                                 '[dataset$Q4A1_',
                                 value,
                                 ' == 2 & dataset$Q4A2 != ',
                                 value,
                                 '] <- "review-in"'
                                 )

    # create the command to add column for request 3
    cdmColumnCreation3 <- paste0('dataset$reviewFunnel_3_',
                                 value,
                                 '[dataset$Q3_2_',
                                 value,
                                 ' == 2 & dataset$Q4A3A',
                                 value,
                                 ' != 1] <- "review-in"')

    # create the command to add column for request 4
    cdmColumnCreation4 <- paste0('dataset$reviewFunnel_4_',
                                 value,
                                 '[dataset$Q4A3A',
                                 value,
                                 ' == 0 & dataset$Q4A4_',
                                 value,
                                 '!= 1] <- "review-in"')
    
    # run command lines
    eval(parse(text = cdmColumnCreation1))
    eval(parse(text = cdmColumnCreation2))
    eval(parse(text = cdmColumnCreation3))
    eval(parse(text = cdmColumnCreation4))
  
  }
  
  ### request 5
  # Routing / NA check: testing if the right values are missing
  
  ###	If Q3_2_None == 1, then ALL Q3_2_Brand# == 0
  cmdAddBrands <- 'dataset$reviewFunnel_5_a[dataset$Q3_2_None == 1'
  
  # loop for adding all the brands
  for (value in valueVector) {
    
    # expand command to all
    cmdAddBrands <- paste0(cmdAddBrands,
                           ' & dataset$Q3_2_',
                           value, 
                           ' == 0')
  }
    
  # add final touch 
  cmdAddBrands <- paste0(cmdAddBrands,
                         '] <- "review-in"')
  
  # run command lines
  eval(parse(text = cmdAddBrands))
  
  ###	If Q3_2_Brand# == 2, then Q4_A_1_Brand# == N/A 
  
  # loop for all brands 
  for (value in valueVector){
    
    # create the command to add column for request 1 
    cdmColumnCreation <- paste0('dataset$reviewFunnel_5_b_',
                                 value,
                                 '[dataset$Q3_2_',
                                 value,
                                 ' == 2 & is.na(dataset$Q4A1_',
                                 value,
                                 ')] <- "review-in"')
    
    # run command lines
    eval(parse(text = cdmColumnCreation))
    }
  
  
  ###	If Q3_1_Peripheral# == 3 | 4, then Q4A_3_Brand# == N/A
  # loop for all brands 
  for (value in valueVector){
    
    # create the command to add column for request 1 
    cdmColumnCreation <- paste0('dataset$reviewFunnel_5_b_',
                                value,
                                '[(dataset$Q3_1_',
                                value,
                                ' == 3 | dataset$Q3_1_',
                                value,
                                ' == 4) & is.na(dataset$Q4A_3_',
                                value,
                                ')] <- "review-in"')
    
    # run command lines
    eval(parse(text = cdmColumnCreation))
  }
  
  ###	If Q3_2_Brand# == 2 for ALL peripheral_range, then Q4_A_4 == N/A
  
  # TBD
  
  
  ###	If Q4_A_3 == N/A, then Q4_A_4 == N/A
  dataset$reviewFunnel_5_e[is.na(dataset$Q4_A_3) & 
                             is.na(dataset$Q4_A_4)] <- "review-in"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(dataset)
}