Chapter1Checks <- function(dataset) {
  
  # function that does the checks for Chapter 1
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  ### request 1 - 
  # Age Absolutes: Check if values don’t exceed 50 or dip below 10 
  
  dataset$reviewChapter1_1[dataset$Q1_2 > 50 | 
                             dataset$Q1_2 < 10] <- 'review1-Out'
  
  
  ### request 2 - 
  # Q1_4 Work Situation & Q1.5 Home Situation
  
  dataset$reviewChapter1_2[dataset$Q1_2 < 15 & 
                             is.na(dataset$Q1_4)] <- 'review2-In'
  
  ### request 3 - 
  # Check if Q1_2_1 > 15 have values (1 to 9 for Q1_4, 1 – 6 for Q1_5)
  
  # create accepted Values 
  Q1_4_Accepted_Values <- c(1:9)
  Q1_5_Accepted_Values <- c(1:6)
  
  # create the memisc conditions 
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_3 = memisc::cases(
      "review3-In" = Q1_2 > 15 & 
        Q1_4 %in% Q1_4_Accepted_Values &
        Q1_5 %in% Q1_5_Accepted_Values,
      "NAs" = Q1_2 < 15)})
  )
  
  # correct custom NAs 
  dataset$reviewChapter1_3[dataset$reviewChapter1_3 == "NAs"] <- NA
  
  ### request 4 - 
  # Q1_7_1 Watch live streamed video content of games & Q1_7_2 Watch 
  # pre-recorded or reruns of streamed video content of games
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_4 = memisc::cases(
      
      "review4-in-1" = Q1_6A10 == 1 & Q1_7_1 == 1,
      "review4-in-2" = Q1_6A10 == 1 & Q1_7_1 == 2,
      "review4-in-3" = Q1_6A10 == 1 & Q1_7_2 == 1,
      "review4-in-4" = Q1_6A10 == 1 & Q1_7_2 == 2,
      
      "review4-in-5" = Q1_6A10 == 0 & Q1_7_1 == 1,
      "review4-in-6" = Q1_6A10 == 0 & is.na(Q1_7_1),
      "review4-in-7" = Q1_6A10 == 0 & Q1_7_2 == 1,
      "review4-in-9" = Q1_6A10 == 0 & is.na(Q1_7_2))})
    
  )
  
  ### request 5 - 
  # Q1_8_1  Frequency of watching live streams
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_5 = memisc::cases(
      
      "review5-in-1" = Q1_7_1 == 1 & Q1_8_1 %in% c(2:6),
      "review5-flag-never" = Q1_7_1 == 1 & Q1_8_1 == 1,
      "review5-in-2" = Q1_7_1 == 2 & (Q1_8_1 == 0 | is.na(Q1_8_1)))})
    
  )
  
  
  ### request 6 - 
  # Q1_8_2 Frequency of watching prerecorded
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_6 = memisc::cases(
      
      "review6-in-1" = Q1_7_2 == 1 & Q1_8_2 %in% c(2:6),
      "review6-flag-never" = Q1_7_2 == 1 & Q1_8_2 == 1,
      "review6-in-2" = Q1_7_2 == 2 & (Q1_8_2 == 0 | is.na(Q1_8_2)))})
    
  )
  
  ### request 7 - 
  # Q1_9 Statements on gaming
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_7 = memisc::cases(
      
      "review7-in-1" = (Q1_7_1 == 1 |
                          Q1_7_2 == 1) & (Q1_9_1 == 1 |
                                            Q1_9_1 == 2) & (Q1_9_2 == 1 | 
                                                              Q1_9_2 == 2),
      "review7-in-2" = (Q1_7_1 == 2 | 
                          Q1_7_2 == 2) & is.na(Q1_9_1) & is.na(Q1_9_2))})
    
  )
  
  
  ### request 8 - 
  # Q1_10 Do you play games?
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_8 = memisc::cases(
      
      "review8-1" = Q1_10 == 1 | Q1_10 == 2,
      "review8-flag-NA" = is.na(Q1_10))})
    
  )
  
  ### request 9 - 
  # Q1_11 Have you ever played
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_9 = memisc::cases(
      
      "review9-1" = Q1_10 == 2 & Q1_11 %in% c(1:5),
      "review9-2" = Q1_10 == 1 & is.na(Q1_11))})
    
  )
  
  ### request 10 - 
  # Q1_12 Have you ever played
  
  suppressMessages(
    dataset <- memisc::within(dataset, {reviewChapter1_10 = memisc::cases(
      
      "review10-1" = Q1_10 == 2 & (Q1_12 == 1 | Q1_12 == 2),
      "review10-2" = Q1_10 == 1 & is.na(Q1_12))})
    
  )
  
  return(dataset)
}