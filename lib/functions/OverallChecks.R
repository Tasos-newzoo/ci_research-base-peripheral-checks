OverallChecks <- function(dataset) {
  
  # function that does the checks for Overall Part
  # 
  # Parameters used:
  # dataset = the provided dataset
  # 
  # returns the same dataset with the added checks 
  
  ### 1 check - 
  ### If people ARE NOT Active PC/Console gamer (APC), 
  ### make sure all columns are N/A after Q2.2 
  
  # find the index of last Q_2 question 
  lastQ2Index <- which(colnames(dataset) == "Q2_2_3")
  nextQ2Index <- lastQ2Index + 1
  lastQuestionIndex <- length(dataset)
  
  # tag if all of the answers is NA or not
  suppressMessages(
    dataset <- memisc::within(dataset, 
                              {reviewChapter0_1_tag = memisc::cases(
                                "all_Q2_NA" = is.na(Q2_2_1) & 
                                  is.na(Q2_2_2) &
                                  is.na(Q2_2_3),
                                "not_all_Q2_NA" = !is.na(Q2_2_1) | 
                                  !is.na(Q2_2_2) |
                                  !is.na(Q2_2_3))}))
  
  # All-NA/ check in the rest of questions (after Q2 to end)
  dataset$reviewChapter0_1_ALL_NA <- apply(dataset[, nextQ2Index:
                                                     lastQuestionIndex], 
                                           MARGIN = 1, 
                                           FUN = function(x) all(is.na(x)))
  
  # check both requests together 
  suppressMessages(
    dataset <- memisc::within(dataset, 
                              {reviewChapter0_1_combined = memisc::cases(
                                "Not Active And All NA" = 
                                  reviewChapter0_1_tag %in% "all_Q2_NA" &
                                  reviewChapter0_1_ALL_NA == TRUE,
                                "Not Active And Not All NA" = 
                                  reviewChapter0_1_tag %in% "all_Q2_NA" &
                                  reviewChapter0_1_ALL_NA == FALSE,
                                "Active And All NA" = 
                                  reviewChapter0_1_tag %in% "not_all_Q2_NA" &
                                  reviewChapter0_1_ALL_NA == TRUE,
                                "Active And Not All NA" = 
                                  reviewChapter0_1_tag %in% "not_all_Q2_NA" &
                                  reviewChapter0_1_ALL_NA == FALSE)}))
  

  ### 2 Check 
  ### APC == 1, if either (Q2_1_2 > 2) OR (Q2_1_3 > 2 AND Q2_1AA2 == 1) 
  
  
  dataset$reviewChapter0_2[(dataset$HAPC == 1) & 
                             ((dataset$Q2_1_2 > 2) | 
                                (dataset$Q2_1_3 > 2 &
                                   dataset$Q2_1AA2 ==1))] <- 
    "APC-Check-In-Conditions"
  
  return(dataset)
}