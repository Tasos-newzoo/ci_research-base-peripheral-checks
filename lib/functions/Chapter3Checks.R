Chapter3Checks <- function(dataset) {
  
  # function that does the checks for Chapter 3
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  
  #  ##############  request 1 - 
  # Q3_3 Brand attributes. We ask this for 10 brands, where the blue number 
  # correspond to the brand awareness numbers in red 
  # e.g. for Astro: Q3_3_1_1 through Q3_3_1_17 = Astro brand attributes. 
  # People need to be aware of Astro (Q3_2_1 == 1)
  # while for Logitech G it will be Q3_3_9_1 through Q3_3_9_17 and Q3_2_9 
  
  brandVector <- c(2,6,11,13,15,20,23,25,28,32) #c(1:10)
  questionVector <- c(1:17)
  
  # loop for brands 
  for (brand in brandVector) {
    
    # loop for questions 
    for (question in questionVector){
      
      # create the command to add column 
      cdmColumnCreation <- paste0('suppressMessages(
                                  dataset <- memisc::within(dataset, 
                                  {reviewChapter3_1_',
                                  brand,
                                  '_',
                                  question,
                                  ' = memisc::cases(
                                  "review-in-1" = Q3_2_',
                                  brand,
                                  '== 1 & (Q3_3_',
                                  brand,
                                  '_',
                                  question,
                                  ' == 1 | Q3_3_',
                                  brand,
                                  '_',
                                  question,
                                  '== 0), "review-in-2" = Q3_2_',
                                  brand,
                                  '== 2 & is.na(Q3_3_',
                                  brand,
                                  '_',
                                  question,
                                  '== 1))}))')
      
      # run command line 
      eval(parse(text = cdmColumnCreation))
    }
  }
  
  ### request 2 - 
  # Q3_4 How did you get to know about SteelSeries?
  
  valueVector <- c(1:10)
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter3_2 = memisc::cases(
      
      "review1-in-1" = (Q3_4 %in% valueVector) & Q3_2_28 == 1,
      "review1-in-2" = is.na(Q3_4) & Q3_2_28 == 2)})
    
  )
  
  ### request 3 - 
  # Q3_5 SteelSeries programs
  # NEW CONDITIONS 
  # Check if Q3_5A1 to Q3_5A6 != NA , if Q3_4 == 3
  # if Q3_4 != 3 OR == NA, if Q3_5A1 to Q3_5A6 == NA
  
  valueVector <- c(1:6)
  
  
  for (value in valueVector) {
    
    # create the command to add column 
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset, 
                                {reviewChapter3_3_',
                                value,
                                '= memisc::cases(
                                "review-in-1" = is.na(Q3_5A',
                                value,
                                ') & Q3_4 == 3,
                                "review-in-2" = is.na(Q3_5A',
                                value,
                                ') & (Q3_4 != 3 | is.na(Q3_4)))}))'
                                )
    
    # run command line 
    eval(parse(text = cdmColumnCreation))
  }
  return(dataset)
}