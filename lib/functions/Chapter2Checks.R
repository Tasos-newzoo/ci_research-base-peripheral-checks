Chapter2Checks <- function(dataset) {
  
  # function that does the checks for Chapter 1
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  ###  request 1 - 
  # Q2_1_1 & Q2_1_2 & Q2_1_3 Play frequency
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_1_a = memisc::cases(
      
      "review-in-1" = (Q2_1_1 == 1 | 
                          Q2_1_1 == 0) & Q1_10 == 1,
      "review-in-2" = is.na(Q2_1_1) & Q1_10 == 2)})
    
  )
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_1_b = memisc::cases(
      
      "review-in-1" = (Q2_1_2 == 1 | 
                          Q2_1_2 == 0) & Q1_10 == 1,
      "review-in-2" = is.na(Q2_1_2) & Q1_10 == 2)})
    
  )
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_1_c = memisc::cases(
      
      "review-in-1" = (Q2_1_3 == 1 | 
                          Q2_1_3 == 0) & Q1_10 == 1,
      "review-in-2" = is.na(Q2_1_3) & Q1_10 == 2)})
    
  )
  
  
  ### request 2 - 
  # Q2_1AA1 Type of PC gamers played – Browser & Q2_1AA1 
  # Type of PC gamers played – Downloaded
  
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_2_a = memisc::cases(
      
      "review-in-1" = (Q2_1AA1 == 1 | 
                          Q2_1AA1 == 0) & Q2_1_3 != 1,
      "review-in-2" = is.na(Q2_1AA1) & Q2_1_3 == 1)})
    
  )
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_2_b = memisc::cases(
      
      "review-in-1" = (Q2_1AA2 == 1 | 
                          Q2_1AA2 == 0) & Q2_1_3 != 1,
      "review-in-2" = is.na(Q2_1AA2) & Q2_1_3 == 1)})
    
  )
  
  ### request 3 - 
  # Q2_2_1 & Q2_2_2 & Q2_2_3 Time spent
  
  
  valueVector <- c(1:7)
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_3_a = memisc::cases(
      
      "review-in-1" = (Q2_2_1 %in% valueVector) & Q2_1_1 != 1,
      "review-in-2" = is.na(Q2_2_1) & Q2_1_1 == 1)})
    
  )
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_3_b = memisc::cases(
      
      "review-in-1" = (Q2_2_2 %in% valueVector) & Q2_1_2 != 1,
      "review-in-2" = is.na(Q2_2_2) & Q2_1_2 == 1)})
    
  )
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter2_3_c = memisc::cases(
      
      "review-in-1" = (Q2_2_3 %in% valueVector) & Q2_1_3 != 1,
      "review-in-2" = is.na(Q2_2_3) & Q2_1_3 == 1)})
    
  )
  
  ### request 4 - 
  # Q2_5 Social media used – COUNTRY SPECIFIC – 9 times
  
  # pattern to social media columns 
  sMPattern <- "Q2_5A"
  
  # find the columns that correspond to social media 
  sMColumns <- dataset %>% 
    dplyr:: select(starts_with(sMPattern)) %>% 
    colnames()
  
  # find the amount of social media columns 
  numberSMColumns <- length(sMColumns)
  
  # loop for all social media columns 
  for (counter in 1:numberSMColumns) {
    
    # create the command to add column
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset, 
                                {reviewChapter2_4_',
                                counter,
                                '= memisc::cases(
                                "review-in-1" = LAND == 1 & (',
                                sMColumns[counter],
                                ' == 1 | ',
                                sMColumns[counter],
                                '== 0),
                                "review-in-2" = LAND != 1 & is.na(',
                                sMColumns[counter],
                                '))}))')
    
    # # run command line
    eval(parse(text = cdmColumnCreation))
    
  }
  
  return(dataset)
}