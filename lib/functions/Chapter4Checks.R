Chapter4Checks <- function(dataset) {
  
  # function that does the checks for Chapter 4
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  ###  request 1 - 
  # Q4Z1 Drivers for peripherals 
  valueVector <- c(1:18)
  
  # loop for all values
  for (value in valueVector){
    
    # create the command to add column 
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset, 
                                {reviewChapter4_1_',
                                value,
                                ' = memisc::cases(
                                "review-in-1" = (Q4Z1A',
                                value,
                                ' == 1 | Q4Z1A',
                                value,
                                ' == 0) & (Q3_1_1 != 4 | 
                                               Q3_1_2 != 4 | 
                                               Q3_1_3 != 4 | 
                                               Q3_1_4 != 4 | 
                                               Q3_1_5 != 4),
                                "review-in-4" = is.na(Q4Z1A',
                                value,
                                ' ) & (Q3_1_1 != 4 | 
                                             Q3_1_2 != 4 | 
                                             Q3_1_3 != 4 | 
                                             Q3_1_4 != 4 | 
                                             Q3_1_5 != 4))}))')
    
    # run command line 
    eval(parse(text = cdmColumnCreation))
    
  }
  
  ### request 2 - 
  # Q4Z2 Source 
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter4_2 = memisc::cases(
      
      "review-in-1" = Q4Z2 %in% c(1:4) & (Q3_1_1 != 4 | 
                                            Q3_1_2 != 4 | 
                                            Q3_1_3 != 4 | 
                                            Q3_1_4 != 4 | 
                                            Q3_1_5 != 4),
      "review-in-2" = is.na(Q4Z2) & (Q3_1_1 != 4 | 
                                       Q3_1_2 != 4 | 
                                       Q3_1_3 != 4 | 
                                       Q3_1_4 != 4 | 
                                       Q3_1_5 != 4))})
  )
  
  ### request 3 
  # Q4Z3 Online retailer - COUNTRY SPECIFIC – 9 times
  
  # pattern to online retailer columns 
  oRPattern <- "Q4Z3A"
  
  # find the columns that correspond to social media 
  oRColumns <- dataset %>% 
    dplyr:: select(starts_with(oRPattern)) %>% 
    colnames()
  
  # find the amount of social media columns 
  numberORColumns <- length(oRColumns)
  
  # loop for all social media columns 
  for (counter in 1:numberORColumns) {
    
    # create the command to add column
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset,
                                {reviewChapter4_3_',
                                counter,
                                '= memisc::cases(
                                "review-in-1" = LAND == 1 & Q4Z2 == 2 & (',
                                oRColumns[counter],
                                ' == 1 | ',
                                oRColumns[counter],
                                '== 0),
                                "review-in-2" = LAND != 1 & Q4Z2 != 2 & is.na(',
                                oRColumns[counter],
                                '))}))')

    # # run command line
    eval(parse(text = cdmColumnCreation))
    
  }
  
  
  ### request 4
  # Q4Z4 Physical store - COUNTRY SPECIFIC – 9 times
  
  # pattern to online retailer columns 
  pSPattern <- "Q4Z4A"
  
  # find the columns that correspond to social media 
  pSColumns <- dataset %>% 
    dplyr:: select(starts_with(pSPattern)) %>% 
    colnames()
  
  # find the amount of social media columns 
  numberPSColumns <- length(pSColumns)
  
  # loop for all social media columns 
  for (counter in 1:numberPSColumns) {
    
    # create the command to add column
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset,
                                {reviewChapter4_3_',
                                counter,
                                '= memisc::cases(
                                "review-in-1" = LAND == 1 & Q4Z2 == 3 & (',
                                pSColumns[counter],
                                ' == 1 | ',
                                pSColumns[counter],
                                '== 0),
                                "review-in-2" = LAND != 1 & Q4Z2 != 3 & is.na(',
                                pSColumns[counter],
                                '))}))')
    
    # # run command line
    eval(parse(text = cdmColumnCreation))
  }
  
  
  
  return(dataset)
}