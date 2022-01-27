WriteChecksFile <- function(dataset, checksPath) {
  
  # function that reads on the columns that start with "Review"
  # writes the occurancies in the log file 
  #
  # Parameters Used:
  # dataset = data that contain the review columns 
  # checksPath = the path to *.log file that contain the tests
  #
  # returns the log file with all the checks 
  
  
  # pattern to online retailer columns 
  chkPattern <- "review"
  
  # find the columns that correspond to social media 
  ckcColumns <- dataset %>% 
    dplyr:: select(starts_with(chkPattern)) %>% 
    colnames()
  
  # find the amount of social media columns 
  numberCKCColumns <- length(ckcColumns)
  
  # open log file
  checksLogger <- file(checksPath,"w")
  
  # separator for each log entry
  checkSeperator <- "-------------------------------------------------"
  
  # loop for checking all review columns 
  for (counter in 1:numberCKCColumns) {
    
    # write separator per entry in log file
    writeLines(paste0(checkSeperator), con = checksLogger, sep = "\n")
    
    # create command for doing the check 
    cmdtoRun <- paste0('entry <- table(dataset$',
                       ckcColumns[counter],
                       ', useNA = "always") %>% as.data.table()')
    
    # run command lines
    eval(parse(text = cmdtoRun))
    
    # write the column to be tested
    writeLines(paste0("Column to be tested: ", 
                      as.character(ckcColumns[counter])), 
               con = checksLogger, sep = "\n")
    
    # write the entry itself
    writeLines(paste0(entry), con = checksLogger, sep = "\n")
    
  }
  
  # write separator per entry in log file
  writeLines(paste0(checkSeperator), con = checksLogger, sep = "\n")
  
  # write summary of tests ran
  writeLines(paste0("Test ran successfully for ", numberCKCColumns, 
                    " columns..."),
             con = checksLogger)
  
  # close log file
  close(checksLogger)
  
  return(numberCKCColumns)
}