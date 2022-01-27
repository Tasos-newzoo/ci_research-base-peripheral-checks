# run.R executes the update/clean up of the Newzoo assignment
options(warn = -1)

assign("workingDir", 
       here::here(), 
       envir = .GlobalEnv)

source(paste0(workingDir,
              '/bin/main.R'))

mainPeripheralChecks(workingDir)
