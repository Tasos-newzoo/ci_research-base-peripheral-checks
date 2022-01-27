mainPeripheralChecks <- function(workingDir) {
  
  # function that is automated to read the *.sav file in the ./data/input_data
  # folder and run sequentially the 5 steps to  clean up the dataset
  # and returns the updated/cleaned up *.sav file
  # 
  # Parameter used:
  # workingDir = the working directory where the project lays
  # 
  # returns updated/cleaned up *.sav file in the ./data/output_data
  
  # libraries needed 
  pacman::p_load(here, yaml, memisc, data.table, dplyr, haven, arsenal)
  
  # # read global parameters list
  # assign("globalParameters",
  #        yaml::yaml.load_file(paste0(workingDir,
  #                                    '/config/global_parameters.yml')), 
  #        envir = .GlobalEnv)
  # 
  # 
  # #### assign parameters needed 
  # # setting source file to read data from 
  # assign("sourceFile",
  #        paste0(workingDir,
  #               globalParameters$sourceFile), 
  #        envir = .GlobalEnv)
  # 
  # # setting output file to export
  # assign("outputFile",
  #        paste0(workingDir,
  #               globalParameters$outputFile), 
  #        envir = .GlobalEnv)
  # 
  # # variables for question 1 
  # assign("question1CountryCondtion2",
  #        globalParameters$question1CountryCondtion2,
  #        envir = .GlobalEnv)
  # 
  # assign("question1CountryCondtion3",
  #        globalParameters$question1CountryCondtion3,
  #        envir = .GlobalEnv)
  # 
  # # variables for question 4
  # assign("question4CountryCondtion1",
  #        globalParameters$question4CountryCondtion1,
  #        envir = .GlobalEnv)
  # 
  # assign("question4CountryCondtion2",
  #        globalParameters$question4CountryCondtion2,
  #        envir = .GlobalEnv)
  # 
  # assign("question4CountryCondtion4",
  #        globalParameters$question4CountryCondtion4,
  #        envir = .GlobalEnv)
  # 
  # 
  # # variables for question 5
  # assign("question5AgeCondtion1",
  #        globalParameters$question5AgeCondtion1,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion2",
  #        globalParameters$question5AgeCondtion2,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion3",
  #        globalParameters$question5AgeCondtion3,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion4",
  #        globalParameters$question5AgeCondtion4,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion5",
  #        globalParameters$question5AgeCondtion5,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion6",
  #        globalParameters$question5AgeCondtion6,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion7",
  #        globalParameters$question5AgeCondtion7,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion8",
  #        globalParameters$question5AgeCondtion8,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion9",
  #        globalParameters$question5AgeCondtion9,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion10",
  #        globalParameters$question5AgeCondtion10,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion11",
  #        globalParameters$question5AgeCondtion11,
  #        envir = .GlobalEnv)
  # 
  # assign("question5AgeCondtion12",
  #        globalParameters$question5AgeCondtion12,
  #        envir = .GlobalEnv)
  # 
  # 
  # 
  # # log configuration
  # assign("outputLogError",
  #        globalParameters$outputLogError,
  #        envir = .GlobalEnv)
  # assign("outputLogPath",
  #        paste0(workingDir,globalParameters$outputLogPath),
  #        envir = .GlobalEnv)

  #### Custom functions built ####
  # function to check Overall
  source(paste0(workingDir,
                '/lib/functions/OverallChecks.R'))
  # function to check Chapter 1
  source(paste0(workingDir,
                '/lib/functions/Chapter1Checks.R'))
  # function to check Chapter 2
  source(paste0(workingDir,
                '/lib/functions/Chapter2Checks.R'))
  # function to check Chapter 3
  source(paste0(workingDir,
                '/lib/functions/Chapter3Checks.R'))
  # function to check Chapter 4
  source(paste0(workingDir,
                '/lib/functions/Chapter4Checks.R'))
  # function to check Chapter 5
  source(paste0(workingDir,
                '/lib/functions/Chapter5Checks.R'))
  # function to check Chapter 6
  source(paste0(workingDir,
                '/lib/functions/Chapter6Checks.R'))
  # function to check Chapter Funnel
  source(paste0(workingDir,
                '/lib/functions/FunnelChecks.R'))
  # function to log the checks
  source(paste0(workingDir,
                '/lib/functions/WriteChecksFile.R'))
  # function to write log file
  source(paste0(workingDir,
                '/lib/functions/ErrorWritingLogger.R'))
  
  #### Main Script #### 
  # read the data from the source
  # tryCatch to log any connection or retrieve data errors
  tryCatch({
    dataset <- haven::read_spss('./data/correct/newzoo52us[06-12-2021].sav') %>%
                as.data.table()
    
  }, error = function(e) {ErrorWritingLogger(e, "ReadSourceFile", outputLogPath, 
                                             outputLogError)})
  
  # get the initial column number of the dataset to use it in the summaries 
  initialColumnNumber <- length(dataset)
  
  ############## Overall check
  dataset <- OverallChecks(dataset)
  
  ############## Chapter 1 - General Questions 
  dataset <- Chapter1Checks(dataset)
  
  ############## Chapter 2 - General game behavior and preferences
  dataset <- Chapter2Checks(dataset)
  
  ############## Chapter 3 - Purchase funnel game peripherals
  dataset <- Chapter3Checks(dataset)
  
  ############## Chapter Funnels 
  dataset <- FunnelChecks(dataset)
  
  ############## Chapter 4 - General Peripheral topics
  dataset <- Chapter4Checks(dataset)
  
  ############## Chapter 5 - Console & PC Hardware
  dataset <- Chapter5Checks(dataset)

  ############## Chapter 6 - Gamer behavior
  tryCatch({
    franciseCodes <- openxlsx::read.xlsx('./data/franchise_codes.xlsx')
    
  }, error = function(e) {ErrorWritingLogger(e, "openxlsx::read.xlsx",
                                             outputLogPath, outputLogError)})
  
  # turn dataset to vector 
  franciseCodes <- franciseCodes$code
  
  # perform Chapter 6 checks 
  dataset <- Chapter6Checks(dataset, franciseCodes)
  
  ############## Perform Summaries 
  checksPath <- "./data/ChecksPerformed.txt"
  
  # Perform Summaries 
  tryCatch({
    
    numberOfChecks <- WriteChecksFile(dataset, checksPath)
    
  }, error = function(e) {ErrorWritingLogger(e, "WriteChecksFile", 
                                             outputLogPath, outputLogError)})
  
  ##############  store data to output folder
  tryCatch({

    write_sav(dataset, './testedFile.sav')

  }, error = function(e) {ErrorWritingLogger(e, "write_sav", outputLogPath,
                                             outputLogError)})
  
  return(dataset)
}
