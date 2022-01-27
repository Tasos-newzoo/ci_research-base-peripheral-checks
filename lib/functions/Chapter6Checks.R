Chapter6Checks <- function(dataset, franciseCodes) {
  
  # function that does the checks for Chapter 6
  # 
  # Parameters used:
  # dataset = the provided dataset
  # franciseCodes = vector containg franchises
  # 
  # returns the same dataset with the added checks 
  
  ###  request 1 - 
  # Q6_3 Esports frequency
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter6_1 = memisc::cases(
      
      "review-in-1" = (Q6_3 == 1 | Q6_3 == 1) & Q6_2 == 1,
      "review-in-2" = (Q6_3 == 3 | 
                          Q6_3 == 4) & Q6_2 == 1 & (Q1_7_1 == 1 | 
                                                      Q1_7_2 == 1),
      "review-in-3" = (Q6_3 != 2 | 
                          Q6_3 != 4) & Q6_2 == 1 & (Q1_7_1 == 1 | 
                                                      Q1_7_2 == 2),
      "review-in-4" = is.na(Q6_3) & Q6_2 == 1)})
    )
  
  ### request 2 - 
  # Q6_4 Franchises watched
  
  for (counter in franciseCodes){
    
    # create the command to add column 
    cdmColumnCreation <- paste0('suppressMessages(
                                dataset <- memisc::within(dataset, 
                                {reviewChapter6_2_',
                                counter,
                                '= memisc::cases(
                                "review-in-1" = (Q6_4_',
                                counter,
                                ' == 1 | Q6_4_',
                                counter,
                                '  == 0) & (Q6_2 == 1 |
                                                  Q1_7_1 == 1 | 
                                                  Q1_7_2 == 1),
                                "review-in-2" = is.na(Q6_4_',
                                counter,
                                ') & (Q6_2 == 1 |
                                                Q1_7_1 == 1 | 
                                                Q1_7_2 == 1),
                                "review-in-3" = Q6_4_',
                                counter,
                                ' == 0 & Q6_4A44 == 1)}))')
    
    # run command line 
    eval(parse(text = cdmColumnCreation))
    
  }
  
  ### request 3 & 4 in 1 loop 
  # On top of that, they should be aware of the franchise so:
  # &
  # And the other way around as well
  # 
  # All the way through Q6_4A42 / Q2_4_42
  
  
  for (counter in franciseCodes){
    
    # create the command to add column for 3
    cdmColumnCreation3 <- paste0('dataset$reviewChapter6_3_',
                                 counter,
                                 '[(dataset$Q6_4_',
                                 counter,
                                 ' == 1 | dataset$Q6_4_',
                                 counter,
                                 '  == 0) & dataset$Q2_4_',
                                 counter,
                                 '  != 5] <- "review-UnEqual"')
    
    # create the command to add column for 4
    cdmColumnCreation4 <- paste0('dataset$reviewChapter6_4_',
                                 counter,
                                 '[(dataset$Q6_4_',
                                 counter,
                                 ' == 1 | dataset$Q6_4_',
                                 counter,
                                 '  == 0) & dataset$Q2_4_',
                                 counter,
                                 '  == 5] <- "review-Equal"')
    
    # # run command line for 3 
    eval(parse(text = cdmColumnCreation3))
    # # run command line for 4
    eval(parse(text = cdmColumnCreation4))
    
  }

  
  ### request 5
  # Q6_7_1 Watch live streamed video content of games & Q6_7_2 Watch 
  # pre-recorded or reruns of streamed video content of games:
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter6_5 = memisc::cases(
      
      "review-in-1" = Q1_6A10 == 1 & Q6_7_1 == 1,
      "review-in-2" = Q1_6A10 == 1 & Q6_7_1 == 2,
      "review-in-3" = Q1_6A10 == 1 & Q6_7_2 == 1,
      "review-in-4" = Q1_6A10 == 1 & Q6_7_2 == 2,
      
      "review-in-5" = Q1_6A10 == 0 & Q6_7_1 == 1,
      "review-in-6" = Q1_6A10 == 0 & is.na(Q6_7_1),
      "review-in-7" = Q1_6A10 == 0 & Q6_7_2 == 1,
      "review-in-9" = Q1_6A10 == 0 & is.na(Q6_7_2))})
    
  )
  
  
  ### request 6
  # Q6_8_1  Frequency of watching live streams
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter6_6 = memisc::cases(
      
      "review-in-1" = Q6_7_1 == 1 & Q6_8_1 %in% c(2:6),
      "review-flag-never" = Q6_7_1 == 1 & Q6_8_1 == 1,
      "review-in-2" = Q6_7_1 == 2 & (Q6_8_1 == 0 | is.na(Q6_8_1)))})
    
  )
  
  ### request 7
  # Q6_8_2 Frequency of watching prerecorded
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter6_7 = memisc::cases(
      
      "review6-in-1" = Q6_7_2 == 1 & Q6_8_2 %in% c(2:6),
      "review6-flag-never" = Q6_7_2 == 1 & Q6_8_2 == 1,
      "review6-in-2" = Q6_7_2 == 2 & (Q6_8_2 == 0 | is.na(Q1_8_2)))})
    
  )
  
  return(dataset)
}