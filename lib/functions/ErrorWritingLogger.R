ErrorWritingLogger <- function(e, runningFunctionName, 
                               logPath, OutputLogError) {
  
  # function to log errors and quit session properly
  #
  # Parameters used:
  # e = error message retrieved from the tryCatch 
  # runningFunctionName = a character variable pointing on the function 
  #                         that the error occurred
  # logPath = path to the log file
  # OutputLogError = how to handle the logged error write to file or
  #                     just print to console 
  # 
  # No returns, except writing the file /printing in the console
  
  if (OutputLogError %in% "write") {
    # open log file
    errors.logger <- file(logPath,"w")
    # write log file
    writeLines(paste0("An error occured with - ", runningFunctionName),
               con = errors.logger, sep = "\n")
    writeLines(paste0(Sys.time()," - ","main.R - ", as.character(e)),
               con = errors.logger, sep = "\n")
    writeLines("Session will be terminated properly...", con = errors.logger)
    # close log file
    close(errors.logger)
    
  } else if (OutputLogError %in% "print") {
    print(paste0("An error occured with - ", runningFunctionName))
    print(paste0(Sys.time()," - ","main.R - ", as.character(e)))
    print("Session will be terminated properly...")
  }
  
  # stop session after catching an error and logging it
  quit("yes")
}
