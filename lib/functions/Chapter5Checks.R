Chapter5Checks <- function(dataset) {
  
  # function that does the checks for Chapter 5
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  ##############  request 1 - 
  # Q5_2 Console ownership
  numericVector <- c(1:10)
  
  
  for (counter in numericVector){
    
    # create the command to add column 
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset, 
                                {reviewChapter5_1_',
                                counter,
                                '=memisc::cases(
                                "review1-in-1" = (Q5_2A',
                                counter,
                                ' == 1 | Q5_2A',
                                counter,
                                ' == 0) & Q2_1_2 != 1, 
                                "review1-in-2" = is.na(Q5_2A',
                                counter,
                                ') & Q2_1_2 == 1,
                                "review1-in-3" = (Q5_2A',
                                counter,
                                ' == 0) & Q5_2A11 == 1)}))')
    
    # run command line 
    eval(parse(text = cdmColumnCreation))
    
  }
  
  ############## request 2 - 
  # Q5_3 PC usage
  
  valueVector <- c(1:4)
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter5_2 = memisc::cases(
      
      "review1-in-1" = (Q5_3 %in% valueVector) & Q2_1_3 != 1,
      "review1-in-2" = is.na(Q5_3) & Q2_1_3 != 1)})
    
  )
  
  ############## request 3 - 
  # Q5_8 Steering wheel budget
  
  
  valueVector <- c(1:6)
  valueVector2 <- c(1:3)
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter5_3 = memisc::cases(
      
      "review1-in-1" = (Q5_8 %in% valueVector) & (Q5_4_7 %in% valueVector2),
      "review1-in-2" = is.na(Q5_8) & Q5_4_7 == 4)})
    
  )
  
  return(dataset)
}