### - Peripheral 2021 - April Wave - 05/2021
# - Script Overview
# 1.  Set base file path
# 2.  Read the data (mapping docs on sheets)
# 3.  Set backup file path
# 4.  Question grouping
# 5.  Variable name override
# 6.  Answer mapping override
# 7.  Custom Variable Sorting
# 8.  Row based filters
# 9.  Weights
# 10. Calculate derived variables
# 11. Save output files
# increase java memory limits =================================

# optimized for 8GB machine
options(java.parameters = "-Xmx8000m")

# load some libraries ----
library('haven')
library('dplyr')
library('rlist')
library('googlesheets4') # adapted 2020
library('googledrive') # added 2020
library('tidyr')
library('xlsx')
library('slackr')
library('spssmergeR')
library('digest')
library('tictoc')
library('beepr')
library('lubridate')
library('readr')
library('survey')
library('stringr')

tic(msg = 'Done loading / transforming data')

'%!in%' <- Negate('%in%')
active_user <- Sys.getenv("USERNAME")

# 1. set base path.
onedrive_project_basepath <- paste0("C:/Users/", active_user,"/Newzoo/Consumer Research Syndicated - Documents/Peripheral Brand Tracker/2021/April wave/automation/")

# 2. read the data from google sheets ----
gs_handle <- googledrive::drive_get("Peripheral_2021_April_mapping") # updated 2021
# gs_handle <- gs_title("peripheral_2019_mapping", verbose = FALSE)

gsheetsdataBackupFilepath <- paste0(onedrive_project_basepath, "mapping_backups/mappings " , lubridate::today() , ".xlsx")
# was turned off in latest CIP19 script

drive_download(gs_handle, path = gsheetsdataBackupFilepath, overwrite = TRUE, verbose = FALSE)

# gs_handle %>% gs_download(to = gsheetsdataBackupFilepath, overwrite = TRUE, verbose = FALSE)

# gsheetsdataBackupFilepath <-  paste0("C:/Users/", active_user,"/Documents/Github/peripheral_2020/peripheral_2020_mapping.xlsx")
# was turned on in latest CIP19 script, updated it before though

questionGrouping <- read.xlsx(gsheetsdataBackupFilepath, sheetName = 'question_grouping')
questionGrouping <- questionGrouping %>% select(country_code, inputfile_question_id, question_id) %>% na.omit()
questionGrouping$country_code  <- as.character(questionGrouping$country_code)
questionGrouping$inputfile_question_id  <- as.character(questionGrouping$inputfile_question_id)
questionGrouping$question_id  <- as.character(questionGrouping$question_id)
questionGrouping$inputfile_question_id <-  trimws(questionGrouping$inputfile_question_id)
questionGrouping$question_id <-  trimws(questionGrouping$question_id)

variableNameOverride <-  read.xlsx(gsheetsdataBackupFilepath,
                                   sheetName = "question_label_override",
                                   colClasses=c(question_id='character',
                                                label='character'),
                                   stringsAsFactors=FALSE)

variableNameOverride <- variableNameOverride[!is.na(variableNameOverride$question_id),] # drop empty varnames
variableNameOverride <- variableNameOverride[!is.na(variableNameOverride$question_label),] # drop empty labels
variableNameOverride <- variableNameOverride %>% select(question_id, question_label)

answerMappingOverride <- read.xlsx(gsheetsdataBackupFilepath,
                                   sheetName = "answer_label_override",
                                   stringsAsFactors=FALSE)

answerMappingOverride$question_id <- as.character(answerMappingOverride$question_id)
answerMappingOverride$answer_id <- as.character(answerMappingOverride$answer_id)
answerMappingOverride$answer_value <- as.character(answerMappingOverride$answer_value)

answerMappingOverride <- answerMappingOverride %>%
  filter(!is.na(question_id)) %>%
  filter(!is.na(answer_id)) %>%
  filter(!is.na(answer_value)) %>%
  # filter(ignore != TRUE) %>%
  select(question_id, answer_id, answer_value)

variablesToDelete <- read.xlsx(gsheetsdataBackupFilepath, sheetName = "variables_to_delete", stringsAsFactors=FALSE) %>% select(question_id) %>% filter(!is.na(question_id))

customVariableSorting <- read.xlsx(gsheetsdataBackupFilepath, sheetName = "custom_variable_sorting", stringsAsFactors=FALSE)
customVariableSorting <- customVariableSorting$sort[!is.na(customVariableSorting$sort)]
customVariableSorting_duplicated_entries <- customVariableSorting[duplicated(customVariableSorting)]

if(length(customVariableSorting_duplicated_entries) > 0){
  warning("Duplicate entries found in custom variable sorting tab:")
  warning(paste(customVariableSorting_duplicated_entries, collapse=', '))
}

customVariableSorting <- customVariableSorting[!duplicated(customVariableSorting)] # overwrite the custom sort with deduplicated version

participantWhitelist <- read.xlsx(gsheetsdataBackupFilepath,
                                  sheetName = "participant_whitelist",
                                  colClasses=c(SSI_PID='character',
                                               notes='character'),
                                  stringsAsFactors=FALSE)

participantWhitelist <- ( participantWhitelist %>% filter(!is.na(SSI_PID)) %>% select(SSI_PID) )

participantBlacklist <- read.xlsx(gsheetsdataBackupFilepath,
                                  sheetName = "participant_blacklist",
                                  colClasses=c(SSI_PID='character',
                                               notes='character'),
                                  stringsAsFactors=FALSE)
participantBlacklist <- participantBlacklist %>% filter(!is.na(SSI_PID) ) %>% select(SSI_PID)

rowBasedFilters <-  read.xlsx(gsheetsdataBackupFilepath,
                              sheetName = "row_based_filters",
                              colClasses=c(parameter='character',
                                           logical_operator='character',
                                           value='character'),
                              stringsAsFactors=FALSE)

rowBasedFilters <- rowBasedFilters[complete.cases(rowBasedFilters),]

weightsGsheets <- read.xlsx(gsheetsdataBackupFilepath,
                            sheetName = "weights",
                            colClasses=c(question_id='character',
                                         answer_value='character'),
                            stringsAsFactors=FALSE)

weightsGsheets <- weightsGsheets %>%
  filter(!is.na(country_code)) %>%
  filter(!is.na(total_online_n_per_everything_except_device))

weightsGsheets <- weightsGsheets %>% select(country_code,
                                            age_bracket,
                                            gender,
                                            total_online_n_per_everything_except_device)

weightsGsheets$period_countrycode_agebracket_gender_key <- paste0('2021', '_',
                                                                  weightsGsheets$country_code, '_',
                                                                  weightsGsheets$age_bracket, '_',
                                                                  weightsGsheets$gender)

western_NA_rules <- read.xlsx(gsheetsdataBackupFilepath,
                              sheetName = "western_NA_rules",
                              colClasses=c(variable_name_NEW='character',
                                           answer_value='character'),
                              stringsAsFactors=FALSE)

western_NA_rules <- western_NA_rules[,c("period", "variable_name_NEW", "country_code")]
western_NA_rules <- western_NA_rules %>% filter(period != 0)

china_NA_rules <- read.xlsx(gsheetsdataBackupFilepath,
                            sheetName = "china_NA_rules",
                            colClasses=c(variable_name_NEW='character',
                                         answer_value='character'),
                            stringsAsFactors=FALSE)

china_NA_rules <- china_NA_rules[,c("period", "variable_name_NEW", "country_code")]
china_NA_rules <- china_NA_rules %>% filter(period != 0)

# make a list of the files
# myfiles <- index_savfiles(paste0(onedrive_project_basepath,'input'))
input_location <- paste0("C:/Users/", active_user,"/Newzoo/Consumer Research Syndicated - Documents/Peripheral Brand Tracker/2021/April wave/automation/input")
myfiles <- list.files(input_location, full.names = TRUE)

# generate the mapping of the answer ids > answer values, read the files into one df
reference_country <- "US"
mydataList <- list()
answer_id_value_mapping <- data.frame("period" = numeric(),"country_code" = character(), "question_id" = character(), "answer_id" = character(), "answer_value" = character())
for (i in 1:length(myfiles)){
  currentfilename_short <- tail((strsplit(myfiles[[i]],"/"))[[1]], n=1)
  currentfile_period <- 2021
  currentcountrycode <- substr(currentfilename_short, 0, 2)
  
  if(currentcountrycode == reference_country){
    reference_country_index <- i
  }
  
  mydataList[[i]] <- read_spss(myfiles[i]) # add the tibble to the list
  mydataList[[i]]$country_code <- currentcountrycode
  mydataList[[i]]$period <- currentfile_period
  current_file_ncol <- ncol(mydataList[[i]])
  
  # loop over columns in file, get the question value mapping per column
  current_file_mapping <- data.frame()
  for (column_i in 1:(current_file_ncol))
  {
    # generate a DF of the question Ids -> values mapping for the current question in the current file
    current_file_column_id <- colnames(mydataList[[i]][column_i])
    
    # if the combination [period + GLOBAL + column_id] is in the override, override the column id
    if(nrow(subset(questionGrouping, tolower(country_code) == 'global' & inputfile_question_id == current_file_column_id)) > 0)
    {
      colnames(mydataList[[i]])[column_i] <- subset(questionGrouping, tolower(country_code) == 'global' &  inputfile_question_id == current_file_column_id)$question_id[1]
    }
    
    # if the combination [period + country_code + column_id] is in the override, override the column id
    if(nrow(subset(questionGrouping, country_code == currentcountrycode & inputfile_question_id == current_file_column_id)) > 0)
    {
      colnames(mydataList[[i]])[column_i] <- subset(questionGrouping, country_code == currentcountrycode &  inputfile_question_id == current_file_column_id)$question_id[1]
    }
    
    current_file_column_attributes <- ((attributes(mydataList[[i]][[column_i]])))
    current_file_column_attributes_labels <- current_file_column_attributes$labels # get the labels
    current_file_column_attributes_labels_df <- data.frame('answer_id' = current_file_column_attributes_labels) # generate df from labels
    current_file_column_attributes_labels_df$answer_value <- rownames(current_file_column_attributes_labels_df) # set answer labels in df
    rownames(current_file_column_attributes_labels_df) <- NULL # remove redundant row names
    
    try((current_file_column_attributes_labels_df$question_id <- colnames(mydataList[[i]][column_i])), silent = TRUE) # add the question_id
    
    # append it to the file specific df
    if(nrow(current_file_mapping) == 0)
    {
      current_file_mapping <- current_file_column_attributes_labels_df
    }
    else
    {
      current_file_mapping <- bind_rows(current_file_mapping, current_file_column_attributes_labels_df)
    }
    current_file_column_attributes_labels_df <<- current_file_column_attributes_labels_df
  }
  # add metadata to the file specific mappings
  current_file_mapping$period <- currentfile_period
  current_file_mapping$country_code <- currentcountrycode
  current_file_mapping$filename <- currentfilename_short
  
  # bind the question value mappings for all files together
  if(nrow(answer_id_value_mapping) == 0)
  {
    answer_id_value_mapping <- current_file_mapping
  }
  else
  {
    answer_id_value_mapping <- bind_rows(answer_id_value_mapping, current_file_mapping)
  }
  answer_id_value_mapping <- answer_id_value_mapping[, c("period","country_code", "question_id", "answer_id", "answer_value")]
}


# generate row binded DF
mydata <- Reduce(bind_rows,mydataList)

reference_country_question_labels <- get_question_labels(mydataList[[reference_country_index]])
reference_country_answer_labels <- get_answer_labels(mydataList[[reference_country_index]])

mydata <- apply_question_label_overrides(mydata, reference_country_question_labels)
mydata <- apply_answer_label_overrides(mydata, reference_country_answer_labels)

mydata <- apply_answer_label_overrides(mydata, answerMappingOverride)
mydata <- apply_question_label_overrides(mydata, variableNameOverride)

###########################################################################################################################
# insert derived variables here ---------------
# generate respondentID
mydata$RespondentID <- generate_respondent_ids(mydata)

# country recode =================================
# NB: the label recodes for COUNTRY should be in the mapping!
mydata$COUNTRY <- NA
mydata$COUNTRY[mydata$country_code == 'US'] <- 1 # United States, 
mydata$COUNTRY[mydata$country_code == 'UK'] <- 2 # United Kingdom,
mydata$COUNTRY[mydata$country_code == 'CN'] <- 3 # China, 
mydata$COUNTRY[mydata$country_code == 'SE'] <- 4 # Sweden, 
mydata$COUNTRY[mydata$country_code == 'FR'] <- 5 # France, 
mydata$COUNTRY[mydata$country_code == 'DE'] <- 6 # Germany,  
mydata$COUNTRY[mydata$country_code == 'RU'] <- 7 # Russia, 
mydata$COUNTRY[mydata$country_code == 'PL'] <- 8 # Poland, 
mydata$COUNTRY[mydata$country_code == 'JP'] <- 9 # Japan, 


# Age / gender buckets =================================
# Age buckets
mydata$Q1_2 <- cut(mydata$Q1_2_1, 
                   breaks = c(0, 15,20,25,30,35,40,45,50), 
                   labels = c("10 - 15",
                              "16 - 20",
                              "21 - 25",
                              "26 - 30",
                              "31 - 35",
                              "36 - 40",
                              "41 - 45",
                              "46 - 50"))

mydata$Total_Population <- 1

mydata$Age_18_plus <- NA
mydata$Age_18_plus[!is.na(mydata$Q1_2_1)] <- 0
mydata$Age_18_plus[mydata$Q1_2_1 >= 18] <- 1

mydata$Age_21_plus <- NA
mydata$Age_21_plus[!is.na(mydata$Q1_2_1)] <- 0
mydata$Age_21_plus[mydata$Q1_2_1 >= 21] <- 1

# Age buckets combined
mydata$Age_Combined1 <- as.numeric(between(mydata$Q1_2_1, 10,20))
mydata$Age_Combined2 <- as.numeric(between(mydata$Q1_2_1, 21,35))
mydata$Age_Combined3 <- as.numeric(between(mydata$Q1_2_1, 36,50))

# Age + Gender combinations
# males
mydata$Age_Gender1 <- as.numeric( (mydata$Age_Combined1 == 1 & mydata$Q1_1 == 1) )
mydata$Age_Gender2 <- as.numeric( (mydata$Age_Combined2 == 1 & mydata$Q1_1 == 1) )
mydata$Age_Gender3 <- as.numeric( (mydata$Age_Combined3 == 1 & mydata$Q1_1 == 1) )

# females
mydata$Age_Gender4 <- as.numeric( (mydata$Age_Combined1 == 1 & mydata$Q1_1 == 2) )
mydata$Age_Gender5 <- as.numeric( (mydata$Age_Combined2 == 1 & mydata$Q1_1 == 2) )
mydata$Age_Gender6 <- as.numeric( (mydata$Age_Combined3 == 1 & mydata$Q1_1 == 2) )


# Income buckets =================================
options(warn=-1)
# United Kingdom
mydata$Q1_3_UK <- NA
mydata$Q1_3_UK[mydata$country_code == 'UK' & mydata$Q1_3 %in% (1:3)] <- 1 # Low
mydata$Q1_3_UK[mydata$country_code == 'UK' & mydata$Q1_3 %in% (4:6)] <- 2 # Mid
mydata$Q1_3_UK[mydata$country_code == 'UK' & mydata$Q1_3 %in% (7:11)] <- 3 # High
mydata$Q1_3_UK[mydata$country_code == 'UK' & mydata$Q1_3 == 12] <- 4 # Not stated

# United States
mydata$Q1_3_US <- NA
mydata$Q1_3_US[mydata$country_code == 'US' & mydata$Q1_3 %in% (1:3)] <- 1 # Low
mydata$Q1_3_US[mydata$country_code == 'US' & mydata$Q1_3 %in% (4:6)] <- 2 # Mid
mydata$Q1_3_US[mydata$country_code == 'US' & mydata$Q1_3 %in% (7:9)] <- 3 # High
mydata$Q1_3_US[mydata$country_code == 'US' & mydata$Q1_3 == 10] <- 4 # Not stated

# Germany
mydata$Q1_3_DE <- NA
mydata$Q1_3_DE[mydata$country_code == 'DE' & mydata$Q1_3 %in% (1:3)] <- 1 # Low
mydata$Q1_3_DE[mydata$country_code == 'DE' & mydata$Q1_3 %in% (4:6)] <- 2 # Mid
mydata$Q1_3_DE[mydata$country_code == 'DE' & mydata$Q1_3 %in% (7:11)] <- 3 # High
mydata$Q1_3_DE[mydata$country_code == 'DE' & mydata$Q1_3 == 12] <- 4 # Not stated

# France
mydata$Q1_3_FR <- NA
mydata$Q1_3_FR[mydata$country_code == 'FR' & mydata$Q1_3 %in% (1:2)] <- 1 # Low
mydata$Q1_3_FR[mydata$country_code == 'FR' & mydata$Q1_3 %in% (3:5)] <- 2 # Mid
mydata$Q1_3_FR[mydata$country_code == 'FR' & mydata$Q1_3 %in% (6:11)] <- 3 # High
mydata$Q1_3_FR[mydata$country_code == 'FR' & mydata$Q1_3 == 12] <- 4 # Not stated

# Poland
mydata$Q1_3_PL <- NA
mydata$Q1_3_PL[mydata$country_code == 'PL' & mydata$Q1_3 %in% (1:2)] <- 1 # Low
mydata$Q1_3_PL[mydata$country_code == 'PL' & mydata$Q1_3 %in% (3:6)] <- 2 # Mid
mydata$Q1_3_PL[mydata$country_code == 'PL' & mydata$Q1_3 %in% (7:11)] <- 3 # High
mydata$Q1_3_PL[mydata$country_code == 'PL' & mydata$Q1_3 == 12] <- 4 # Not stated

# Sweden
mydata$Q1_3_SE <- NA
mydata$Q1_3_SE[mydata$country_code == 'SE' & mydata$Q1_3 %in% (1:4)] <- 1 # Low
mydata$Q1_3_SE[mydata$country_code == 'SE' & mydata$Q1_3 %in% (5:7)] <- 2 # Mid
mydata$Q1_3_SE[mydata$country_code == 'SE' & mydata$Q1_3 %in% (8:10)] <- 3 # High
mydata$Q1_3_SE[mydata$country_code == 'SE' & mydata$Q1_3 == 11] <- 4 # Not stated

# Russia
mydata$Q1_3_RU <- NA
mydata$Q1_3_RU[mydata$country_code == 'RU' & mydata$Q1_3 %in% (1:4)] <- 1 # Low
mydata$Q1_3_RU[mydata$country_code == 'RU' & mydata$Q1_3 %in% (5:8)] <- 2 # Mid
mydata$Q1_3_RU[mydata$country_code == 'RU' & mydata$Q1_3 %in% (9:12)] <- 3 # High
mydata$Q1_3_RU[mydata$country_code == 'RU' & mydata$Q1_3 == 13] <- 4 # Not stated

# China
mydata$Q1_3_CN <- NA
mydata$Q1_3_CN[mydata$country_code == 'CN' & mydata$Q1_3 %in% (1:2)] <- 1 # Low
mydata$Q1_3_CN[mydata$country_code == 'CN' & mydata$Q1_3 %in% (3:7)] <- 2 # Mid
mydata$Q1_3_CN[mydata$country_code == 'CN' & mydata$Q1_3 %in% (8:9)] <- 3 # High
mydata$Q1_3_CN[mydata$country_code == 'CN' & mydata$Q1_3 == 10] <- 4 # Not stated

# Japan
mydata$Q1_3_JP <- NA
mydata$Q1_3_JP[mydata$country_code == 'JP' & mydata$Q1_3 %in% (1:2)] <- 1 # Low
mydata$Q1_3_JP[mydata$country_code == 'JP' & mydata$Q1_3 %in% (3:5)] <- 2 # Mid
mydata$Q1_3_JP[mydata$country_code == 'JP' & mydata$Q1_3 %in% (6:11)] <- 3 # High
mydata$Q1_3_JP[mydata$country_code == 'JP' & mydata$Q1_3 == 12] <- 4 # Not stated

allCountryCodes <- unique(mydata$country_code)
mydata$Q1_3 <- NA
for(i in 1:length(allCountryCodes)){
  country <- allCountryCodes[i]
  cmd <- paste0("mydata$Q1_3[mydata$country_code == '", country, "'] <- mydata$Q1_3_", country,"[mydata$country_code == '", country, "']")
  try(eval(parse(text=cmd)))
}

# Players =================================
# Mobile Players	Code to 1 if Q2_1_1 > 1, code all others to 0
mydata$Players_1 <- 0
mydata$Players_1[mydata$Q2_1_1 > 1] <- 1

# Console Players	Code to 1 if Q2_1_2 > 1, code all others to 0
mydata$Players_2 <- 0
mydata$Players_2[mydata$Q2_1_2 > 1] <- 1

# PC Players	Code to 1 if Q2_1_3 > 1, code all others to 0
mydata$Players_3 <- 0
mydata$Players_3[mydata$Q2_1_3 > 1] <- 1

# Total Players
mydata$Players_4 <- 0
mydata$Players_4[mydata$Q1_10 == 1] <- 1

# Non Gamers
mydata$Players_5 <- 0
mydata$Players_5[mydata$Q1_10 == 2] <- 1

# Platform_overlap_1 - Mobile only
mydata$Platform_overlap_1 <- NA
mydata$Platform_overlap_1[mydata$Players_1 == 1 & mydata$Players_2 == 0 & mydata$Players_3 == 0] <- 1
mydata$Platform_overlap_1[is.na(mydata$Platform_overlap_1) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_2 - Console only
mydata$Platform_overlap_2 <- NA
mydata$Platform_overlap_2[mydata$Players_1 == 0 & mydata$Players_2 == 1 & mydata$Players_3 == 0] <- 1
mydata$Platform_overlap_2[is.na(mydata$Platform_overlap_2) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_3 - PC only
mydata$Platform_overlap_3 <- NA
mydata$Platform_overlap_3[mydata$Players_1 == 0 & mydata$Players_2 == 0 & mydata$Players_3 == 1] <- 1
mydata$Platform_overlap_3[is.na(mydata$Platform_overlap_3) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_4 - Mobile/Console only
mydata$Platform_overlap_4 <- NA
mydata$Platform_overlap_4[mydata$Players_1 == 1 & mydata$Players_2 == 1 & mydata$Players_3 == 0] <- 1
mydata$Platform_overlap_4[is.na(mydata$Platform_overlap_4) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_5 - Mobile/PC only
mydata$Platform_overlap_5 <- NA
mydata$Platform_overlap_5[mydata$Players_1 == 1 & mydata$Players_2 == 0 & mydata$Players_3 == 1] <- 1
mydata$Platform_overlap_5[is.na(mydata$Platform_overlap_5) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_6 - Console/PC only
mydata$Platform_overlap_6 <- NA
mydata$Platform_overlap_6[mydata$Players_1 == 0 & mydata$Players_2 == 1 & mydata$Players_3 == 1] <- 1
mydata$Platform_overlap_6[is.na(mydata$Platform_overlap_6) & mydata$Q1_10 == 1] <- 0

# Platform_overlap_7 - Play on all
mydata$Platform_overlap_7 <- NA
mydata$Platform_overlap_7[mydata$Players_1 == 1 & mydata$Players_2 == 1 & mydata$Players_3 == 1] <- 1
mydata$Platform_overlap_7[is.na(mydata$Platform_overlap_7) & mydata$Q1_10 == 1] <- 0

# # data corrections ####

# Active PC OR Console  needed for None/Other variables
mydata$Active_PC_or_Console_Gamers <- as.numeric(mydata$Q2_1_2 > 2 | (mydata$Q2_1_3 > 2 & mydata$Q2_1AA2 == 1))  # Active PC/Console Gamers	 if (Q2_1_3 > 2 OR Q2_1_4 > 2) then 1 else if Q2_1_3  == NA  and Q2_1_4 == NA then NA else 0
mydata$Active_PC_or_Console_Gamers[is.na(mydata$Q2_1_2) | is.na(mydata$Q2_1_3)] <- 0

# Streamer recodes - People that say they watch/create content in Q1_6 & Q6_6, but never to both live and recorded content creation

# Recode respondents that said "Never" in Q1_8_1 & Q1_8_2 to Q1_7_1 & Q1_7_2 = 2 ("No")
mydata$Q1_7_1[mydata$Q1_8_1 == 1] <- 2 
mydata$Q1_7_2[mydata$Q1_8_2 == 1] <- 2 

# Recode Q1_6A10 to 0 if both Q1_7_1 & Q1_7_2 are  2 = no
mydata$Q1_6A10[(mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2)] <- 0 


# Recode Q1_7_1 & Q1_7_2 to 2 ("No") if Q1_6A10 = 0 to have all respondents as base
mydata$Q1_7_1[mydata$Q1_6A10 == 0] <- 2 
mydata$Q1_7_2[mydata$Q1_6A10 == 0] <- 2 

# Recode Q1_8_1 & Q1_8_2 to 1 ("Never") if Q1_7_1 & Q1_7_2 = 2 ("No") to have all respondents as base
mydata$Q1_8_1[mydata$Q1_7_1 == 2] <- 1 
mydata$Q1_8_2[mydata$Q1_7_2 == 2] <- 1 

# If people now have 0's in all columns of Q1_6A because of the previous step, recode Q1_6A15 (Other) == 1 
mydata$Q1_6A15[   mydata$Q1_6A16 != 1 &
                    ( mydata$Q1_6A1   == 0 &
                        mydata$Q1_6A2   == 0 &
                        mydata$Q1_6A3   == 0 &
                        mydata$Q1_6A4   == 0 &
                        mydata$Q1_6A5   == 0 &
                        mydata$Q1_6A6   == 0 &
                        mydata$Q1_6A7   == 0 &
                        mydata$Q1_6A8   == 0 &
                        mydata$Q1_6A9   == 0 &
                        mydata$Q1_6A10  == 0 &
                        mydata$Q1_6A11  == 0 &
                        mydata$Q1_6A12  == 0 &
                        mydata$Q1_6A13  == 0 &
                        mydata$Q1_6A14  == 0 &
                        mydata$Q1_6A15  == 0)
] <- 1 # True

#Recode respondents that wrongly got Q1_9_1, Q1_9_2, Q6_3, Q6_4 and Q6_5 
mydata$Q1_9_1[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA
mydata$Q1_9_2[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA
mydata$Q6_3[mydata$Q6_3 %in% c(3,4) & mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA

for(i in 1:42)
{
  cmd1 <- paste0('mydata$Q6_4A',i,'[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA')
  cmd2 <- paste0('mydata$Q6_5_',i,'A1[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA')
  cmd3 <- paste0('mydata$Q6_5_',i,'A2[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA')
  cmd4 <- paste0('mydata$Q6_5_',i,'A3[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
}

mydata$Q6_4A43[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA
mydata$Q6_4A44[mydata$Q1_7_1 == 2 & mydata$Q1_7_2 == 2] <- NA

# Recode respondents that said "Never" in Q6_8_1 & Q6_8_2 to Q6_7_1 & Q6_7_2 = 2 ("No")
mydata$Q6_7_1[mydata$Q6_8_1 == 1] <- 2 
mydata$Q6_7_2[mydata$Q6_8_2 == 1] <- 2 

# Recode Q6_6A10 to 0 if both Q6_7_1 & Q6_7_2 are  2 = no
mydata$Q6_6A10[(mydata$Q6_7_1 == 2 & mydata$Q6_7_2 == 2)] <- 0 

# Recode Q6_7_1 & Q6_7_2 to N/A if Q6_6A10 = 0 but recode them to 2 ("No") if they are APC to have them as a base
mydata$Q6_7_1[mydata$Q6_6A10 == 0] <- NA 
mydata$Q6_7_1[mydata$Q6_6A10 == 0 & mydata$Active_PC_or_Console_Gamers == 1] <- 2 
mydata$Q6_7_2[mydata$Q6_6A10 == 0] <- NA 
mydata$Q6_7_2[mydata$Q6_6A10 == 0 & mydata$Active_PC_or_Console_Gamers == 1] <- 2 

# Recode Q6_8_1 & Q6_8_2 to 1 ("Never") if Q6_7_1 & Q6_7_2 = 2 ("No") to have APCs as a base
mydata$Q6_8_1[mydata$Q6_7_1 == 2] <- 1 #NEW
mydata$Q6_8_2[mydata$Q6_7_2 == 2] <- 1 #NEW

#Recode respondents that wrongly got Q6_9
for(i in 1:7)
{
  cmd <- paste0('mydata$Q6_9_',i,'[mydata$Q6_7_1 == 2 & mydata$Q6_7_2 == 2] <- NA')
  eval(parse(text = cmd))
}

# If people now have 0's in all columns of Q6_6A because of the previous step, recode Q6_6A15 (Other) == 1 
mydata$Q6_6A15[   mydata$Q6_6A16 != 1 &
                    ( mydata$Q6_6A1   == 0 &
                        mydata$Q6_6A2   == 0 &
                        mydata$Q6_6A3   == 0 &
                        mydata$Q6_6A4   == 0 &
                        mydata$Q6_6A5   == 0 &
                        mydata$Q6_6A6   == 0 &
                        mydata$Q6_6A7   == 0 &
                        mydata$Q6_6A8   == 0 &
                        mydata$Q6_6A9   == 0 &
                        mydata$Q6_6A10  == 0 &
                        mydata$Q6_6A11  == 0 &
                        mydata$Q6_6A12  == 0 &
                        mydata$Q6_6A13  == 0 &
                        mydata$Q6_6A14  == 0 &
                        mydata$Q6_6A15  == 0)
] <- 1 # True

# esports viewers 
mydata$Esports_Segmentation_1 <- as.numeric(!is.na(mydata$Q6_3) & mydata$Q6_3 == 4) # Enthusiastic Viewers	if Q1_20 = 4
mydata$Esports_Segmentation_1[is.na(mydata$Q6_3)] <- NA

mydata$Esports_Segmentation_2 <- as.numeric(!is.na(mydata$Q6_3) & mydata$Q6_3 == 3) # Occassional Viewers	if Q1_20 = 3
mydata$Esports_Segmentation_2[is.na(mydata$Q6_3)] <- NA

mydata$Esports_Segmentation_3 <- as.numeric(!is.na(mydata$Q6_3) & ( (mydata$Q6_3 == 3) | mydata$Q6_3 == 4) )# Audience	if Q1_20 = 3 or Q1_20 = 4
mydata$Esports_Segmentation_3[is.na(mydata$Q6_3)] <- NA

mydata$Esports_Segmentation_4 <- as.numeric(!is.na(mydata$Q6_2) & mydata$Q6_2 == 1 ) # Awareness	if Q1_19 = 1
mydata$Esports_Segmentation_4[is.na(mydata$Q6_2)] <- NA


# Active PC OR Console  needed for None/Other variables
mydata$Active_PC_or_Console_Gamers <- as.numeric(mydata$Q2_1_2 > 2 | (mydata$Q2_1_3 > 2 & mydata$Q2_1AA2 == 1))  # Active PC/Console Gamers	 if (Q2_1_3 > 2 OR Q2_1_4 > 2) then 1 else if Q2_1_3  == NA  and Q2_1_4 == NA then NA else 0
mydata$Active_PC_or_Console_Gamers[is.na(mydata$Q2_1_2) | is.na(mydata$Q2_1_3)] <- 0

# "None" = "Other" recodes for the funnels

mydata$Q3_2_35[is.na(mydata$Q3_2_35) & 
                 mydata$Active_PC_or_Console_Gamers == 1 &
                 ((mydata$Q3_2_1   == 2  | is.na(mydata$Q3_2_1)  ) &
                    (mydata$Q3_2_2   == 2  | is.na(mydata$Q3_2_2)  ) &
                    (mydata$Q3_2_3   == 2  | is.na(mydata$Q3_2_3)  ) &
                    (mydata$Q3_2_4   == 2  | is.na(mydata$Q3_2_4)  ) &
                    (mydata$Q3_2_5   == 2  | is.na(mydata$Q3_2_5)  ) &
                    (mydata$Q3_2_6   == 2  | is.na(mydata$Q3_2_6)  ) &
                    (mydata$Q3_2_7   == 2  | is.na(mydata$Q3_2_7)  ) &
                    (mydata$Q3_2_8   == 2  | is.na(mydata$Q3_2_8)  ) &
                    (mydata$Q3_2_9   == 2  | is.na(mydata$Q3_2_9)  ) &
                    (mydata$Q3_2_10  == 2  | is.na(mydata$Q3_2_10) )  &
                    (mydata$Q3_2_11  == 2  | is.na(mydata$Q3_2_11) )  &
                    (mydata$Q3_2_12  == 2  | is.na(mydata$Q3_2_12) )  &
                    (mydata$Q3_2_13  == 2  | is.na(mydata$Q3_2_13) )  &
                    (mydata$Q3_2_14  == 2  | is.na(mydata$Q3_2_14) )  &
                    (mydata$Q3_2_15  == 2  | is.na(mydata$Q3_2_15) )  &
                    (mydata$Q3_2_16  == 2  | is.na(mydata$Q3_2_16) )  &
                    (mydata$Q3_2_17  == 2  | is.na(mydata$Q3_2_17) )  &
                    (mydata$Q3_2_18  == 2  | is.na(mydata$Q3_2_18) )  &
                    (mydata$Q3_2_19  == 2  | is.na(mydata$Q3_2_19) )  &
                    (mydata$Q3_2_20  == 2  | is.na(mydata$Q3_2_20) )  &
                    (mydata$Q3_2_21  == 2  | is.na(mydata$Q3_2_21) )  &
                    (mydata$Q3_2_22  == 2  | is.na(mydata$Q3_2_22) )  &
                    (mydata$Q3_2_23  == 2  | is.na(mydata$Q3_2_23) )  &
                    (mydata$Q3_2_24  == 2  | is.na(mydata$Q3_2_24) )  &
                    (mydata$Q3_2_25  == 2  | is.na(mydata$Q3_2_25) )  &
                    (mydata$Q3_2_26  == 2  | is.na(mydata$Q3_2_26) )  &
                    (mydata$Q3_2_27  == 2  | is.na(mydata$Q3_2_27) )  &
                    (mydata$Q3_2_28  == 2  | is.na(mydata$Q3_2_28) )  &
                    (mydata$Q3_2_29  == 2  | is.na(mydata$Q3_2_29) )  &
                    (mydata$Q3_2_30  == 2  | is.na(mydata$Q3_2_30) )  &
                    (mydata$Q3_2_31  == 2  | is.na(mydata$Q3_2_31) )  &
                    (mydata$Q3_2_32  == 2  | is.na(mydata$Q3_2_32) )  &
                    (mydata$Q3_2_33  == 2  | is.na(mydata$Q3_2_33) )  &
                    (mydata$Q3_2_34  == 2  | is.na(mydata$Q3_2_34) )  )
]  <- 2


# Awareness: "None" recode #01 - where any are True, put None to false


mydata$Q3_2_36[(  mydata$Q3_2_1   == 1 |
                    mydata$Q3_2_2   == 1 |
                    mydata$Q3_2_3   == 1 |
                    mydata$Q3_2_4   == 1 |
                    mydata$Q3_2_5   == 1 |
                    mydata$Q3_2_6   == 1 |
                    mydata$Q3_2_7   == 1 |
                    mydata$Q3_2_8   == 1 |
                    mydata$Q3_2_9   == 1 |
                    mydata$Q3_2_10  == 1 |
                    mydata$Q3_2_11  == 1 |
                    mydata$Q3_2_12  == 1 |
                    mydata$Q3_2_13  == 1 |
                    mydata$Q3_2_14  == 1 |
                    mydata$Q3_2_15  == 1 |
                    mydata$Q3_2_16  == 1 |
                    mydata$Q3_2_17  == 1 |
                    mydata$Q3_2_18  == 1 |
                    mydata$Q3_2_19  == 1 |
                    mydata$Q3_2_20  == 1 |
                    mydata$Q3_2_21  == 1 |
                    mydata$Q3_2_22  == 1 |
                    mydata$Q3_2_23  == 1 |
                    mydata$Q3_2_24  == 1 |
                    mydata$Q3_2_25  == 1 |
                    mydata$Q3_2_26  == 1 |
                    mydata$Q3_2_27  == 1 |
                    mydata$Q3_2_28  == 1 |
                    mydata$Q3_2_29  == 1 |
                    mydata$Q3_2_30  == 1 |
                    mydata$Q3_2_31  == 1 |
                    mydata$Q3_2_32  == 1 |
                    mydata$Q3_2_33  == 1 |
                    mydata$Q3_2_34  == 1 |
                    mydata$Q3_2_35  == 1)
] <- 2 # False

mydata$Q3_2_36[   mydata$Active_PC_or_Console_Gamers == 1 &
                    (mydata$Q3_2_1   == 2 | is.na(mydata$Q3_2_1 ) ) &
                    (mydata$Q3_2_2   == 2 | is.na(mydata$Q3_2_2 ) ) &
                    (mydata$Q3_2_3   == 2 | is.na(mydata$Q3_2_3 ) ) &
                    (mydata$Q3_2_4   == 2 | is.na(mydata$Q3_2_4 ) ) &
                    (mydata$Q3_2_5   == 2 | is.na(mydata$Q3_2_5 ) ) &
                    (mydata$Q3_2_6   == 2 | is.na(mydata$Q3_2_6 ) ) &
                    (mydata$Q3_2_7   == 2 | is.na(mydata$Q3_2_7 ) ) &
                    (mydata$Q3_2_8   == 2 | is.na(mydata$Q3_2_8 ) ) &
                    (mydata$Q3_2_9   == 2 | is.na(mydata$Q3_2_9 ) ) &
                    (mydata$Q3_2_10  == 2 | is.na(mydata$Q3_2_10) ) &
                    (mydata$Q3_2_11  == 2 | is.na(mydata$Q3_2_11) ) &
                    (mydata$Q3_2_12  == 2 | is.na(mydata$Q3_2_12) ) &
                    (mydata$Q3_2_13  == 2 | is.na(mydata$Q3_2_13) ) &
                    (mydata$Q3_2_14  == 2 | is.na(mydata$Q3_2_14) ) &
                    (mydata$Q3_2_15  == 2 | is.na(mydata$Q3_2_15) ) &
                    (mydata$Q3_2_16  == 2 | is.na(mydata$Q3_2_16) ) &
                    (mydata$Q3_2_17  == 2 | is.na(mydata$Q3_2_17) ) &
                    (mydata$Q3_2_18  == 2 | is.na(mydata$Q3_2_18) ) &
                    (mydata$Q3_2_19  == 2 | is.na(mydata$Q3_2_19) ) &
                    (mydata$Q3_2_20  == 2 | is.na(mydata$Q3_2_20) ) &
                    (mydata$Q3_2_21  == 2 | is.na(mydata$Q3_2_21) ) &
                    (mydata$Q3_2_22  == 2 | is.na(mydata$Q3_2_22) ) &
                    (mydata$Q3_2_23  == 2 | is.na(mydata$Q3_2_23) ) &
                    (mydata$Q3_2_24  == 2 | is.na(mydata$Q3_2_24) ) &
                    (mydata$Q3_2_25  == 2 | is.na(mydata$Q3_2_25) ) &
                    (mydata$Q3_2_26  == 2 | is.na(mydata$Q3_2_26) ) &
                    (mydata$Q3_2_27  == 2 | is.na(mydata$Q3_2_27) ) &
                    (mydata$Q3_2_28  == 2 | is.na(mydata$Q3_2_28) ) &
                    (mydata$Q3_2_29  == 2 | is.na(mydata$Q3_2_29) ) &
                    (mydata$Q3_2_30  == 2 | is.na(mydata$Q3_2_30) ) &
                    (mydata$Q3_2_31  == 2 | is.na(mydata$Q3_2_31) ) &
                    (mydata$Q3_2_32  == 2 | is.na(mydata$Q3_2_32) ) &
                    (mydata$Q3_2_33  == 2 | is.na(mydata$Q3_2_33) ) &
                    (mydata$Q3_2_34  == 2 | is.na(mydata$Q3_2_34) ) &
                    (mydata$Q3_2_35  == 2 | is.na(mydata$Q3_2_35) )
] <- 1 # True

# Awareness: "None" recode #02 - where None is True, put all others to False
awareness_correction <- function(imax){
  for(i in (1:imax))
  {
    cmd <- paste0('mydata$Q3_2_',i,'[mydata$Q3_2_36 == 1] <<- 2')
    print(cmd)
    eval(parse(text=cmd))
  }
}

awareness_correction(35) 

# table(mydata %>% filter(Q3_2_36 == 1) %>% select(Q3_2_1), useNA = 'always') KG 2020: not sure what this is doing?

# Awareness: "Other" recode - Where Other is NA, None is False and any other brands is true
mydata$Q3_2_35[is.na(mydata$Q3_2_35) &
                 mydata$Q3_2_36 != 1 &
                 (mydata$Q3_2_1   == 1 |
                    mydata$Q3_2_2   == 1 |
                    mydata$Q3_2_3   == 1 |
                    mydata$Q3_2_4   == 1 |
                    mydata$Q3_2_5   == 1 |
                    mydata$Q3_2_6   == 1 |
                    mydata$Q3_2_7   == 1 |
                    mydata$Q3_2_8   == 1 |
                    mydata$Q3_2_9   == 1 |
                    mydata$Q3_2_10  == 1 |
                    mydata$Q3_2_11  == 1 |
                    mydata$Q3_2_12  == 1 |
                    mydata$Q3_2_13  == 1 |
                    mydata$Q3_2_14  == 1 |
                    mydata$Q3_2_15  == 1 |
                    mydata$Q3_2_16  == 1 |
                    mydata$Q3_2_17  == 1 |
                    mydata$Q3_2_18  == 1 |
                    mydata$Q3_2_19  == 1 |
                    mydata$Q3_2_20  == 1 |
                    mydata$Q3_2_21  == 1 |
                    mydata$Q3_2_22  == 1 |
                    mydata$Q3_2_23  == 1 |
                    mydata$Q3_2_24  == 1 |
                    mydata$Q3_2_25  == 1 |
                    mydata$Q3_2_26  == 1 |
                    mydata$Q3_2_27  == 1 |
                    mydata$Q3_2_28  == 1 |
                    mydata$Q3_2_29  == 1 |
                    mydata$Q3_2_30  == 1 |
                    mydata$Q3_2_31  == 1 |
                    mydata$Q3_2_32  == 1 |
                    mydata$Q3_2_33  == 1 |
                    mydata$Q3_2_34  == 1 )
] <- 2

# # Headset consideration: "None" recode #01 - where any are True, put None to false
mydata$Q4A1_36[(  mydata$Q4A1_1   == 1 |
                    mydata$Q4A1_2   == 1 |
                    mydata$Q4A1_3   == 1 |
                    mydata$Q4A1_4   == 1 |
                    mydata$Q4A1_5   == 1 |
                    mydata$Q4A1_7   == 1 |
                    mydata$Q4A1_8   == 1 |
                    mydata$Q4A1_9   == 1 |
                    
                    mydata$Q4A1_10  == 1 |
                    mydata$Q4A1_11  == 1 |
                    mydata$Q4A1_12  == 1 |
                    mydata$Q4A1_13  == 1 |
                    mydata$Q4A1_14  == 1 |
                    mydata$Q4A1_15  == 1 |
                    mydata$Q4A1_16  == 1 |
                    mydata$Q4A1_17  == 1 |
                    mydata$Q4A1_18  == 1 |
                    mydata$Q4A1_19  == 1 |
                    
                    mydata$Q4A1_20  == 1 |
                    mydata$Q4A1_21  == 1 |
                    mydata$Q4A1_22  == 1 |
                    mydata$Q4A1_23  == 1 |  
                    mydata$Q4A1_24  == 1 |
                    mydata$Q4A1_25  == 1 |
                    mydata$Q4A1_26  == 1 |
                    
                    mydata$Q4A1_32  == 1 |
                    mydata$Q4A1_34  == 1 |
                    mydata$Q4A1_35  == 1)
] <- 2 # False

# Q4A1_36 is True, when all others are false
mydata$Q4A1_36[     mydata$Active_PC_or_Console_Gamers == 1 &
                      (mydata$Q4A1_1   == 2  | is.na(mydata$Q4A1_1)) &
                      (mydata$Q4A1_2   == 2  | is.na(mydata$Q4A1_2)) &
                      (mydata$Q4A1_3   == 2  | is.na(mydata$Q4A1_3)) &
                      (mydata$Q4A1_4   == 2  | is.na(mydata$Q4A1_4)) &
                      (mydata$Q4A1_5   == 2  | is.na(mydata$Q4A1_5)) &
                      (mydata$Q4A1_7   == 2  | is.na(mydata$Q4A1_7)) &
                      (mydata$Q4A1_8   == 2  | is.na(mydata$Q4A1_8)) &
                      (mydata$Q4A1_9   == 2  | is.na(mydata$Q4A1_9)) &
                      
                      (mydata$Q4A1_10  == 2 | is.na(mydata$Q4A1_10)) &
                      (mydata$Q4A1_11  == 2 | is.na(mydata$Q4A1_11)) &
                      (mydata$Q4A1_12  == 2 | is.na(mydata$Q4A1_12)) &
                      (mydata$Q4A1_13  == 2 | is.na(mydata$Q4A1_13)) &
                      (mydata$Q4A1_14  == 2 | is.na(mydata$Q4A1_14)) &
                      (mydata$Q4A1_15  == 2 | is.na(mydata$Q4A1_15)) &
                      (mydata$Q4A1_16  == 2 | is.na(mydata$Q4A1_16)) &
                      (mydata$Q4A1_17  == 2 | is.na(mydata$Q4A1_17)) &
                      (mydata$Q4A1_18  == 2 | is.na(mydata$Q4A1_18)) &
                      (mydata$Q4A1_19  == 2 | is.na(mydata$Q4A1_19)) &
                      
                      (mydata$Q4A1_20  == 2 | is.na(mydata$Q4A1_20)) &
                      (mydata$Q4A1_21  == 2 | is.na(mydata$Q4A1_21)) &
                      (mydata$Q4A1_22  == 2 | is.na(mydata$Q4A1_22)) &
                      (mydata$Q4A1_23  == 2 | is.na(mydata$Q4A1_23)) &  
                      (mydata$Q4A1_24  == 2 | is.na(mydata$Q4A1_24)) &
                      (mydata$Q4A1_25  == 2 | is.na(mydata$Q4A1_25)) &
                      (mydata$Q4A1_26  == 2 | is.na(mydata$Q4A1_26)) &
                      
                      (mydata$Q4A1_32  == 2 | is.na(mydata$Q4A1_32)) &
                      (mydata$Q4A1_34  == 2 | is.na(mydata$Q4A1_34)) &
                      (mydata$Q4A1_35  == 2 | is.na(mydata$Q4A1_35))
] <- 1 # True

# Mouse consideration: "None" recode #01 - where any are True, put None to false
mydata$Q4B1_36[(mydata$Q4B1_2   == 1 |
                  
                  mydata$Q4B1_3   == 1 |
                  mydata$Q4B1_4   == 1 |
                  mydata$Q4B1_5   == 1 |
                  mydata$Q4B1_7   == 1 |
                  mydata$Q4B1_8   == 1 |
                  mydata$Q4B1_9   == 1 |
                  
                  mydata$Q4B1_10  == 1 |
                  mydata$Q4B1_15  == 1 |
                  mydata$Q4B1_16  == 1 |
                  mydata$Q4B1_17  == 1 |
                  mydata$Q4B1_18  == 1 |
                  mydata$Q4B1_19  == 1 |
                  
                  mydata$Q4B1_22  == 1 |
                  mydata$Q4B1_23  == 1 |
                  mydata$Q4B1_28  == 1 |
                  mydata$Q4B1_29  == 1 |
                  
                  mydata$Q4B1_35  == 1)
] <- 2 # False

# Mouse consideration: Q4B1_36 is True when all others are false
mydata$Q4B1_36[   mydata$Active_PC_or_Console_Gamers == 1 &
                    (mydata$Q4B1_2   == 2  | is.na(mydata$Q4B1_2)) &
                    
                    (mydata$Q4B1_3   == 2  | is.na(mydata$Q4B1_3)) &
                    (mydata$Q4B1_4   == 2  | is.na(mydata$Q4B1_4)) &
                    (mydata$Q4B1_5   == 2  | is.na(mydata$Q4B1_5)) &
                    (mydata$Q4B1_7   == 2  | is.na(mydata$Q4B1_7)) &
                    (mydata$Q4B1_8   == 2  | is.na(mydata$Q4B1_8)) &
                    (mydata$Q4B1_9   == 2  | is.na(mydata$Q4B1_9)) &
                    
                    (mydata$Q4B1_10  == 2 | is.na(mydata$Q4B1_10)) &
                    (mydata$Q4B1_15  == 2 | is.na(mydata$Q4B1_15)) &
                    (mydata$Q4B1_16  == 2 | is.na(mydata$Q4B1_16)) &
                    (mydata$Q4B1_17  == 2 | is.na(mydata$Q4B1_17)) &
                    (mydata$Q4B1_18  == 2 | is.na(mydata$Q4B1_18)) &
                    (mydata$Q4B1_19  == 2 | is.na(mydata$Q4B1_19)) &
                    
                    (mydata$Q4B1_22  == 2 | is.na(mydata$Q4B1_22)) &
                    (mydata$Q4B1_23  == 2 | is.na(mydata$Q4B1_23)) &
                    (mydata$Q4B1_28  == 2 | is.na(mydata$Q4B1_28)) &
                    (mydata$Q4B1_29  == 2 | is.na(mydata$Q4B1_29)) &
                    
                    (mydata$Q4B1_35  == 2 | is.na(mydata$Q4B1_35)) 
] <- 1 # True

# Keyboard consideration: "None" recode #01 - where any are True, put None to false
mydata$Q4C1_36[(mydata$Q4C1_2   == 1 |
                  
                  mydata$Q4C1_3   == 1 |
                  mydata$Q4C1_4   == 1 |
                  mydata$Q4C1_5   == 1 |
                  mydata$Q4C1_7   == 1 |
                  mydata$Q4C1_8   == 1 |
                  mydata$Q4C1_9   == 1 |
                  mydata$Q4C1_10  == 1 |
                  
                  mydata$Q4C1_15  == 1 |
                  mydata$Q4C1_16  == 1 |
                  mydata$Q4C1_17  == 1 |
                  mydata$Q4C1_18  == 1 |
                  mydata$Q4C1_19  == 1 |
                  
                  mydata$Q4C1_22  == 1 |
                  mydata$Q4C1_23  == 1 |
                  mydata$Q4C1_28  == 1 |
                  mydata$Q4C1_29  == 1 |
                  
                  mydata$Q4C1_30  == 1 |
                  mydata$Q4C1_33  == 1 |
                  
                  mydata$Q4C1_35  == 1)
] <- 2 # False

# Keyboard consideration: Q4C1_36 is True when all others all False
mydata$Q4C1_36[   mydata$Active_PC_or_Console_Gamers == 1 &
                    (mydata$Q4C1_2   == 2 | is.na(mydata$Q4C1_2 )) &
                    
                    (mydata$Q4C1_3   == 2 | is.na(mydata$Q4C1_3 )) &
                    (mydata$Q4C1_4   == 2 | is.na(mydata$Q4C1_4 )) &
                    (mydata$Q4C1_5   == 2 | is.na(mydata$Q4C1_5 )) &
                    (mydata$Q4C1_7   == 2 | is.na(mydata$Q4C1_7 )) &
                    (mydata$Q4C1_8   == 2 | is.na(mydata$Q4C1_8 )) &
                    (mydata$Q4C1_9   == 2 | is.na(mydata$Q4C1_9 )) &
                    (mydata$Q4C1_10  == 2| is.na(mydata$Q4C1_10))  &
                    
                    (mydata$Q4C1_15  == 2| is.na(mydata$Q4C1_15))  &
                    (mydata$Q4C1_16  == 2| is.na(mydata$Q4C1_16))  &
                    (mydata$Q4C1_17  == 2| is.na(mydata$Q4C1_17))  &
                    (mydata$Q4C1_18  == 2| is.na(mydata$Q4C1_18))  &
                    (mydata$Q4C1_19  == 2| is.na(mydata$Q4C1_19))  &
                    
                    (mydata$Q4C1_22  == 2| is.na(mydata$Q4C1_22))  &
                    (mydata$Q4C1_23  == 2| is.na(mydata$Q4C1_23))  &
                    (mydata$Q4C1_28  == 2| is.na(mydata$Q4C1_28))  &
                    (mydata$Q4C1_29  == 2| is.na(mydata$Q4C1_29))  &
                    
                    (mydata$Q4C1_30  == 2| is.na(mydata$Q4C1_30))  &
                    (mydata$Q4C1_33  == 2| is.na(mydata$Q4C1_33))  &
                    
                    (mydata$Q4C1_35  == 2| is.na(mydata$Q4C1_35)) 
] <- 1 # True

# Surface consideration: "None" recode #01 - where any are True, put None to false
mydata$Q4D1_36[(mydata$Q4D1_2   == 1 |
                  mydata$Q4D1_3   == 1 |
                  mydata$Q4D1_4   == 1 |
                  mydata$Q4D1_7   == 1 |
                  mydata$Q4D1_8   == 1 |
                  mydata$Q4D1_9   == 1 |
                  mydata$Q4D1_10  == 1 |
                  
                  mydata$Q4D1_15  == 1 |
                  mydata$Q4D1_16  == 1 |
                  mydata$Q4D1_17  == 1 |
                  mydata$Q4D1_18  == 1 |
                  mydata$Q4D1_19  == 1 |
                  
                  mydata$Q4D1_22  == 1 |
                  mydata$Q4D1_23  == 1 |
                  mydata$Q4D1_28  == 1 |
                  mydata$Q4D1_29  == 1 |
                  
                  mydata$Q4D1_35  == 1)
] <- 2 # False

# Surface consideration: "None" Q4D1_36 is True when others are False
mydata$Q4D1_36[    mydata$Active_PC_or_Console_Gamers == 1 &
                     (mydata$Q4D1_2   == 2 | is.na(mydata$Q4D1_2 )) &
                     (mydata$Q4D1_3   == 2 | is.na(mydata$Q4D1_3 )) &
                     (mydata$Q4D1_4   == 2 | is.na(mydata$Q4D1_4 )) &
                     (mydata$Q4D1_7   == 2 | is.na(mydata$Q4D1_7 )) &
                     (mydata$Q4D1_8   == 2 | is.na(mydata$Q4D1_8 )) &
                     (mydata$Q4D1_9   == 2 | is.na(mydata$Q4D1_9 )) &
                     (mydata$Q4D1_10  == 2 | is.na(mydata$Q4D1_10)) &
                     
                     (mydata$Q4D1_15  == 2 | is.na(mydata$Q4D1_15)) &
                     (mydata$Q4D1_16  == 2 | is.na(mydata$Q4D1_16)) &
                     (mydata$Q4D1_17  == 2 | is.na(mydata$Q4D1_17)) &
                     (mydata$Q4D1_18  == 2 | is.na(mydata$Q4D1_18)) &
                     (mydata$Q4D1_19  == 2 | is.na(mydata$Q4D1_19)) &
                     
                     (mydata$Q4D1_22  == 2 | is.na(mydata$Q4D1_22)) &
                     (mydata$Q4D1_23  == 2 | is.na(mydata$Q4D1_23)) &
                     (mydata$Q4D1_28  == 2 | is.na(mydata$Q4D1_28)) &
                     (mydata$Q4D1_29  == 2 | is.na(mydata$Q4D1_29)) &
                     
                     (mydata$Q4D1_35  == 2 | is.na(mydata$Q4D1_35))
] <- 1 # True

# Controller consideration: "None" recode #01 - where any are True, put None to false
mydata$Q4E1_36[(mydata$Q4E1_1   == 1 |
                  mydata$Q4E1_6   == 1 |
                  mydata$Q4E1_9   == 1 |
                  
                  mydata$Q4E1_11   == 1 |
                  mydata$Q4E1_13   == 1 |
                  mydata$Q4E1_14   == 1 |
                  mydata$Q4E1_16   == 1 |
                  mydata$Q4E1_17   == 1 |
                  
                  mydata$Q4E1_20  == 1 |
                  mydata$Q4E1_23  == 1 |
                  mydata$Q4E1_24  == 1 |
                  mydata$Q4E1_27  == 1 |
                  
                  mydata$Q4E1_31  == 1 |
                  mydata$Q4E1_35  == 1)
] <- 2 # False

# Controller consideration: "None" Q4E1_36 is True where all others are False
mydata$Q4E1_36[    mydata$Active_PC_or_Console_Gamers == 1 &
                     (mydata$Q4E1_1   == 2 | is.na(mydata$Q4E1_1 )) &
                     (mydata$Q4E1_6   == 2 | is.na(mydata$Q4E1_6 )) &
                     (mydata$Q4E1_9   == 2 | is.na(mydata$Q4E1_9 )) &
                     
                     (mydata$Q4E1_11  == 2 | is.na(mydata$Q4E1_11)) &
                     (mydata$Q4E1_13  == 2 | is.na(mydata$Q4E1_13)) &
                     (mydata$Q4E1_14  == 2 | is.na(mydata$Q4E1_14)) &
                     (mydata$Q4E1_16  == 2 | is.na(mydata$Q4E1_16)) &
                     (mydata$Q4E1_17  == 2 | is.na(mydata$Q4E1_17)) &
                     
                     (mydata$Q4E1_20  == 2 | is.na(mydata$Q4E1_20)) &
                     (mydata$Q4E1_23  == 2 | is.na(mydata$Q4E1_23)) &
                     (mydata$Q4E1_24  == 2 | is.na(mydata$Q4E1_24)) &
                     (mydata$Q4E1_27  == 2 | is.na(mydata$Q4E1_27)) &
                     
                     (mydata$Q4E1_31  == 2 | is.na(mydata$Q4E1_31)) &
                     (mydata$Q4E1_35  == 2 | is.na(mydata$Q4E1_35))
] <- 1 # True

# Preference fix - Include rest of APC in the base; if APC = 1 and preference is NA put 99 'Rest of APC'

# headset prefence Q4A2
mydata$Q4A2[is.na(mydata$Q4A2) & mydata$Active_PC_or_Console_Gamers == 1] <- 99

# mouse prefence Q4B2
mydata$Q4B2[is.na(mydata$Q4B2) & mydata$Active_PC_or_Console_Gamers == 1] <- 99

# keyboard prefence Q4C2
mydata$Q4C2[is.na(mydata$Q4C2) & mydata$Active_PC_or_Console_Gamers == 1] <- 99

# surface prefence Q4D2
mydata$Q4D2[is.na(mydata$Q4D2) & mydata$Active_PC_or_Console_Gamers == 1] <- 99

# controller prefence Q4E2
mydata$Q4E2[is.na(mydata$Q4E2) & mydata$Active_PC_or_Console_Gamers == 1] <- 99


# Recode type of controller mostly used (Q4E12) - 
# If respondents have a controller budget lower than 69 dollars (Q4E5 - value 1,2,3) 
# recode Pro/elite major brand (value 1) and pro/elite specialty website (value 2) to standard controller (value 3)
mydata$Q4E12B <- mydata$Q4E12
mydata$Q4E12B[mydata$Q4E12 %in% c(1,2) & mydata$Q4E5 %in% c(1,2,3)] <- 3



## Personas ----

# Time spent =================================
mydata$Time_spent_mobile <- 0
mydata$Time_spent_mobile[mydata$Q2_2_1 == 1] <- 0.5
mydata$Time_spent_mobile[mydata$Q2_2_1 == 2] <- 1.5
mydata$Time_spent_mobile[mydata$Q2_2_1 == 3] <- 2.5
mydata$Time_spent_mobile[mydata$Q2_2_1 == 4] <- 4.5
mydata$Time_spent_mobile[mydata$Q2_2_1 == 5] <- 8
mydata$Time_spent_mobile[mydata$Q2_2_1 == 6] <- 12.5
mydata$Time_spent_mobile[mydata$Q2_2_1 == 7] <- 15

mydata$Time_spent_console <- 0
mydata$Time_spent_console[mydata$Q2_2_2 == 1] <- 0.5
mydata$Time_spent_console[mydata$Q2_2_2 == 2] <- 1.5
mydata$Time_spent_console[mydata$Q2_2_2 == 3] <- 2.5
mydata$Time_spent_console[mydata$Q2_2_2 == 4] <- 4.5
mydata$Time_spent_console[mydata$Q2_2_2 == 5] <- 8
mydata$Time_spent_console[mydata$Q2_2_2 == 6] <- 12.5
mydata$Time_spent_console[mydata$Q2_2_2 == 7] <- 15

mydata$Time_spent_pc <- 0
mydata$Time_spent_pc[mydata$Q2_2_3 == 1] <- 0.5
mydata$Time_spent_pc[mydata$Q2_2_3 == 2] <- 1.5
mydata$Time_spent_pc[mydata$Q2_2_3 == 3] <- 2.5
mydata$Time_spent_pc[mydata$Q2_2_3 == 4] <- 4.5
mydata$Time_spent_pc[mydata$Q2_2_3 == 5] <- 8
mydata$Time_spent_pc[mydata$Q2_2_3 == 6] <- 12.5
mydata$Time_spent_pc[mydata$Q2_2_3 == 7] <- 15

mydata$Sum_of_time_spent <- mydata$Time_spent_mobile + mydata$Time_spent_console + mydata$Time_spent_pc

mydata$Brackets_time_spent <- NA
mydata$Brackets_time_spent[between(mydata$Sum_of_time_spent, 0.5,4)] <- 1
mydata$Brackets_time_spent[between(mydata$Sum_of_time_spent, 4.5,8)] <- 2
mydata$Brackets_time_spent[mydata$Sum_of_time_spent >= 8.5] <- 3

# Playing Segmentation =================================
mydata$Playing_Segmentation_1 <- ifelse(mydata$Q6_1 == 3 | mydata$Brackets_time_spent == 1, 1 ,NA)
mydata$Playing_Segmentation_2 <- ifelse( (mydata$Q6_1 < 3 & mydata$Brackets_time_spent == 2) | (mydata$Q6_1 < 3 & mydata$Brackets_time_spent == 3 & mydata$Q1_9_3 == 2), 1, NA)
mydata$Playing_Segmentation_3 <- ifelse( (mydata$Q6_1 < 3 &  mydata$Brackets_time_spent == 3 & mydata$Q1_9_3 == 1), 1, NA)

# Viewing Segmentation =================================
mydata <- mydata %>% mutate(Max_Value_Viewing=pmax(Q1_8_1,
                                                   Q1_8_2,
                                                   
                                                   na.rm = TRUE))

mydata$Viewing_Segmentation_1  <- ifelse(mydata$Max_Value_Viewing == 1 , 1, NA)
mydata$Viewing_Segmentation_2 <- ifelse(mydata$Max_Value_Viewing %in% c(2,3), 1, NA)
mydata$Viewing_Segmentation_3 <- ifelse( mydata$Max_Value_Viewing %in% c(4,5), 1, NA)
mydata$Viewing_Segmentation_3[mydata$Max_Value_Viewing == 6 & (mydata$Q1_9_1 == 2 | mydata$Q1_9_2 == 2) & !(mydata$Q1_9_1 == 1 & mydata$Q1_9_2 == 1)] <- 1
mydata$Viewing_Segmentation_4 <- ifelse( (mydata$Max_Value_Viewing == 6 & mydata$Q1_9_1 == 1 & mydata$Q1_9_2 == 1),1, NA)

# Owning Segmentation =================================
mydata$Owning_Segmentation_1 <- ifelse(mydata$Q5_1 == 1, 1, NA)
mydata$Owning_Segmentation_2 <- ifelse(mydata$Q5_2 == 1 | mydata$Q5_1 == 2, 1, NA)
mydata$Owning_Segmentation_3 <- ifelse(mydata$Q5_2 == 2 & mydata$Q5_1 > 2, 1, NA)
mydata$Owning_Segmentation_4 <- ifelse(mydata$Q5_2 == 3 & mydata$Q5_1 > 2, 1, NA)

# Gamer persona segmentation
mydata$Persona1 <- NA
mydata$Persona2 <- NA
mydata$Persona3 <- NA
mydata$Persona4 <- NA
mydata$Persona5 <- NA
mydata$Persona6 <- NA
mydata$Persona7 <- NA
mydata$Persona8 <- NA
mydata$Persona9 <- NA
mydata$Persona_non <- NA
mydata$Persona_all <- NA

mydata$Persona1[mydata$Playing_Segmentation_3 == 1 & (mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1 ) & (mydata$Owning_Segmentation_3 == 1 | mydata$Owning_Segmentation_4 == 1)] <- 1
mydata$Persona2[mydata$Playing_Segmentation_2 == 1 & (mydata$Viewing_Segmentation_2 == 1 | mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1 ) & (mydata$Owning_Segmentation_3 == 1 | mydata$Owning_Segmentation_4 == 1)] <- 1
mydata$Persona3[(mydata$Playing_Segmentation_2 == 1 | mydata$Playing_Segmentation_3 == 1) & (mydata$Viewing_Segmentation_1 == 1 | mydata$Viewing_Segmentation_2 == 1 | mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1 ) & (mydata$Owning_Segmentation_1 == 1 | mydata$Owning_Segmentation_2 == 1)] <- 1
mydata$Persona4[(mydata$Playing_Segmentation_3 == 1 & (mydata$Viewing_Segmentation_1 == 1 | mydata$Viewing_Segmentation_2 == 1) & (mydata$Owning_Segmentation_3 == 1 | mydata$Owning_Segmentation_4 == 1)) | (mydata$Playing_Segmentation_2 == 1 & mydata$Viewing_Segmentation_1 == 1 & (mydata$Owning_Segmentation_3 == 1 | mydata$Owning_Segmentation_4 == 1))] <- 1
mydata$Persona5[mydata$Playing_Segmentation_1 == 1 & (mydata$Viewing_Segmentation_1 == 1 | mydata$Viewing_Segmentation_2 == 1 | mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1) & (mydata$Owning_Segmentation_3 == 1 | mydata$Owning_Segmentation_4 == 1)] <- 1
mydata$Persona6[mydata$Playing_Segmentation_1 == 1 & (mydata$Viewing_Segmentation_2 == 1 | mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1 ) & (mydata$Owning_Segmentation_1 == 1 | mydata$Owning_Segmentation_2 == 1 )] <- 1
mydata$Persona7[mydata$Players_5 == 1 & (mydata$Viewing_Segmentation_2 == 1 | mydata$Viewing_Segmentation_3 == 1 | mydata$Viewing_Segmentation_4 == 1)] <- 1
mydata$Persona8[mydata$Playing_Segmentation_1 == 1 & mydata$Viewing_Segmentation_1 == 1 & (mydata$Owning_Segmentation_1 == 1 | mydata$Owning_Segmentation_2 == 1)] <- 1
mydata$Persona9[mydata$Q1_11 %in% (1:4) & is.na(mydata$Persona7)] <- 1 #ADDED 2020
mydata$Persona_non[is.na(mydata$Persona1) & is.na(mydata$Persona2) & is.na(mydata$Persona3) & is.na(mydata$Persona4) & is.na(mydata$Persona5) & is.na(mydata$Persona6) & is.na(mydata$Persona7) & is.na(mydata$Persona8)] <- 1 #CHANGED 2020
mydata$Persona_all[mydata$Persona1 == 1 | mydata$Persona2 == 1 | mydata$Persona3 == 1 | mydata$Persona4 == 1 | mydata$Persona5 == 1 | mydata$Persona6 == 1 | mydata$Persona7 == 1 | mydata$Persona8 == 1] <- 1 #ADDED 2020

# brand consideration vars
generate_brand_consideration_vars <- function(imax){
  for(i in 1:imax){
    
    Q41A_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4A1_%s' %in% colnames(mydata))){mydata$Q4A1_%s <<- NA}")
    Q41B_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4B1_%s' %in% colnames(mydata))){mydata$Q4B1_%s <<- NA}")
    Q41C_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4C1_%s' %in% colnames(mydata))){mydata$Q4C1_%s <<- NA}")
    Q41D_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4D1_%s' %in% colnames(mydata))){mydata$Q4D1_%s <<- NA}")
    Q41E_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4E1_%s' %in% colnames(mydata))){mydata$Q4E1_%s <<- NA}")
    
    eval(parse(text = Q41A_i_string))
    eval(parse(text = Q41B_i_string))
    eval(parse(text = Q41C_i_string))
    eval(parse(text = Q41D_i_string))
    eval(parse(text = Q41E_i_string))
    
    # people that are aware of the brand but haven't considered it
    brand_consideration_awareness_formula <- "mydata$Q7_2_%s[mydata$Q3_2_%s == 1] <<- 0"
    myexpression_2 <- gsub(pattern = '%s', replacement = i, brand_consideration_awareness_formula)
    print(myexpression_2)
    eval(parse(text = myexpression_2))
    
    # people that considered 
    brand_consideration_formula_string <- "mydata$Q7_2_%s[mydata$Q3_2_%s == 1 & (mydata$Q4A1_%s == 1 | mydata$Q4B1_%s == 1 | mydata$Q4C1_%s == 1 | mydata$Q4D1_%s == 1 | mydata$Q4E1_%s == 1)] <<- 1"
    myexpression <- gsub(pattern = '%s', replacement = i, brand_consideration_formula_string)
    print(myexpression)
    eval(parse(text = myexpression))
    
    
  }
}
generate_brand_consideration_vars(imax = 35)

# consideration: None
mydata$Q7_2_36 <- as.numeric(mydata$Q4A1_36 == 1 & mydata$Q4B1_36 == 1 & mydata$Q4C1_36 == 1 & mydata$Q4D1_36 == 1 & mydata$Q4E1_36 == 1)

# brand preference vars
generate_brand_preference_vars <- function(imax){
  for(i in 1:imax){
    brand_preference_formula_string <- "mydata$Q7_3_%s <<- as.numeric(mydata$Q4A2 == %s | mydata$Q4B2 == %s | mydata$Q4C2 == %s | mydata$Q4D2 == %s | mydata$Q4E2 == %s)"
    myexpression <- gsub(pattern = '%s', replacement = i, brand_preference_formula_string)
    print(myexpression)
    eval(parse(text = myexpression))
  }
}
generate_brand_preference_vars(imax = 35)


# brand ownership vars
generate_brand_ownership_vars <- function(imax){
  for(i in 1:imax){
    
    Q4A3Ai_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4A3A%s' %in% colnames(mydata))){mydata$Q4A3A%s <<- NA}")
    Q4B3Ai_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4B3A%s' %in% colnames(mydata))){mydata$Q4B3A%s <<- NA}")
    Q4C3Ai_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4C3A%s' %in% colnames(mydata))){mydata$Q4C3A%s <<- NA}")
    Q4D3Ai_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4D3A%s' %in% colnames(mydata))){mydata$Q4D3A%s <<- NA}")
    Q4E3Ai_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4E3A%s' %in% colnames(mydata))){mydata$Q4E3A%s <<- NA}")
    
    eval(parse(text = Q4A3Ai_string))
    eval(parse(text = Q4B3Ai_string))
    eval(parse(text = Q4C3Ai_string))
    eval(parse(text = Q4D3Ai_string))
    eval(parse(text = Q4E3Ai_string))
    
    # people that are aware of the brand but no ownership
    brand_ownership_awareness_formula <- "mydata$Q7_4_%s[mydata$Q3_2_%s == 1] <<- 0"
    myexpression_2 <- gsub(pattern = '%s', replacement = i, brand_ownership_awareness_formula)
    print(myexpression_2)
    eval(parse(text = myexpression_2))
    
    # people that own 
    brand_ownership_formula_string <- "mydata$Q7_4_%s[mydata$Q3_2_%s == 1 & (mydata$Q4A3A%s == 1 | mydata$Q4B3A%s == 1 | mydata$Q4C3A%s == 1 | mydata$Q4D3A%s == 1 | mydata$Q4E3A%s == 1)] <<- 1"
    myexpression <- gsub(pattern = '%s', replacement = i, brand_ownership_formula_string)
    print(myexpression)
    eval(parse(text = myexpression))
  }
}
generate_brand_ownership_vars(imax = 35)

# ownership: None
mydata$Q7_4_36 <- as.numeric(mydata$Q4A3A36 == 1 & mydata$Q4B3A36 == 1 & mydata$Q4C3A36 == 1 & mydata$Q4D3A36 == 1 & mydata$Q4E3A36 == 1)

# brand loyalty vars
generate_brand_loyalty_vars <- function(imax){
  for(i in 1:imax){
    
    Q44A_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4A4_%s' %in% colnames(mydata))){mydata$Q4A4_%s <<- NA}")
    Q44B_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4B4_%s' %in% colnames(mydata))){mydata$Q4B4_%s <<- NA}")
    Q44C_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4C4_%s' %in% colnames(mydata))){mydata$Q4C4_%s <<- NA}")
    Q44D_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4D4_%s' %in% colnames(mydata))){mydata$Q4D4_%s <<- NA}")
    Q44E_i_string <- gsub(pattern = '%s', replacement = i, "if(!('Q4E4_%s' %in% colnames(mydata))){mydata$Q4E4_%s <<- NA}")
    
    eval(parse(text = Q44A_i_string))
    eval(parse(text = Q44B_i_string))
    eval(parse(text = Q44C_i_string))
    eval(parse(text = Q44D_i_string))
    eval(parse(text = Q44E_i_string))
    
    # people that are aware of the brand but haven't bought any again
    brand_loyalty_awareness_formula <- "mydata$Q7_5_%s[mydata$Q3_2_%s == 1] <<- 0"
    myexpression_2 <- gsub(pattern = '%s', replacement = i, brand_loyalty_awareness_formula)
    print(myexpression_2)
    eval(parse(text = myexpression_2))
    
    # people that bought again
    brand_loyalty_formula_string <- "mydata$Q7_5_%s[mydata$Q3_2_%s == 1 & (mydata$Q4A4_%s == 1 | mydata$Q4B4_%s == 1 | mydata$Q4C4_%s == 1 | mydata$Q4D4_%s == 1 | mydata$Q4E4_%s == 1)] <<- 1"
    myexpression <- gsub(pattern = '%s', replacement = i, brand_loyalty_formula_string)
    print(myexpression)
    eval(parse(text = myexpression))
  }
}
generate_brand_loyalty_vars(imax = 35)

# Extra player variables + Owners and intenders
mydata$Active_PC_or_Console_Gamers <- as.numeric(mydata$Q2_1_2 > 2 | (mydata$Q2_1_3 > 2 & mydata$Q2_1AA2 == 1))  # Active PC/Console Gamers	 if (Q2_1_3 > 2 OR Q2_1_4 > 2) then 1 else if Q2_1_3  == NA  and Q2_1_4 == NA then NA else 0
mydata$Active_PC_or_Console_Gamers[is.na(mydata$Q2_1_2) | is.na(mydata$Q2_1_3)] <- 0

mydata$Active_PC_AND_Console <- as.numeric(mydata$Q2_1_2 > 2 & (mydata$Q2_1_3 > 2 & mydata$Q2_1AA2 == 1))  # Active PC/Laptop and Console Gamers	if (Q2_1_3 > 2) AND (Q2_1_4 > 2)
mydata$Active_PC_AND_Console[is.na(mydata$Q2_1_2) | is.na(mydata$Q2_1_3)] <- NA

mydata$Active_PC_Laptop <- as.numeric(mydata$Q2_1_3 > 2 & mydata$Q2_1AA2 == 1) # Active PC/Laptop Gamers 
mydata$Active_PC_Laptop[is.na(mydata$Q2_1_3)] <- NA

mydata$Active_Console <- as.numeric(mydata$Q2_1_2 > 2) # Active Console Gamers	
mydata$Active_Console[is.na(mydata$Q2_1_2)] <- NA 

mydata$Active_Mobile <- as.numeric(mydata$Q2_1_1 > 2) # Active Mobile Gamers	
mydata$Active_Mobile[is.na(mydata$Q2_1_1)] <- NA 

mydata$Core_PC_AND_Console <-  as.numeric(mydata$Q2_2_2 > 4 & mydata$Q2_2_3 > 4)	# Core PC/Laptop and Console Gamers	if (Q2_2_3 = 5 OR Q2_2_3 = 6 OR Q2_2_3 = 7) AND (Q2_2_2 = 5 OR Q2_2_2 = 6 OR Q2_2_2 = 7)
mydata$Core_PC_AND_Console[is.na(mydata$Q2_2_2) | is.na(mydata$Q2_2_3)] <- NA

mydata$Core_PC_OR_Console <-  as.numeric(mydata$Q2_2_2 > 4 | mydata$Q2_2_3 > 4)	# Core PC/Laptop or Console Gamers	if (Q2_2_3 = 5 OR Q2_2_3 = 6 OR Q2_2_3 = 7) OR (Q2_2_2 = 5 OR Q2_2_2 = 6 OR Q2_2_2 = 7)
mydata$Core_PC_OR_Console[is.na(mydata$Q2_2_2) | is.na(mydata$Q2_2_3)] <- NA

mydata$Core_PC_Laptop <- as.numeric(mydata$Q2_2_3 > 4) # Core PC/Laptop Gamers	if Q2_2_3 = 5 OR Q2_2_3 = 6 OR Q2_2_3 = 7
mydata$Core_PC_Laptop[is.na(mydata$Q2_2_3)] <- NA

mydata$Core_Console <- as.numeric(mydata$Q2_2_2 > 4)	# Core Console Gamers	if Q2_2_2 = 5 OR Q2_2_2 = 6 OR Q2_2_2 = 7
mydata$Core_Console[is.na(mydata$Q2_2_2)] <- NA

mydata$Core_Mobile <- as.numeric(mydata$Q2_2_1 > 4)	# Core Console Gamers	if Q2_2_2 = 5 OR Q2_2_2 = 6 OR Q2_2_2 = 7
mydata$Core_Mobile[is.na(mydata$Q2_2_1)] <- NA

mydata$Headset_Owners <- as.numeric(mydata$Q3_1_1 == 1 | mydata$Q3_1_1 == 2)
mydata$Headset_Intenders <- as.numeric(mydata$Q3_1_1  == 2 | mydata$Q3_1_1 == 3)
mydata$Headset_dont_own_or_intend <- as.numeric(mydata$Q3_1_1  == 4)

mydata$Mouse_Owners <- as.numeric(mydata$Q3_1_2  == 1 | mydata$Q3_1_2 == 2)
mydata$Mouse_Intenders <- as.numeric(mydata$Q3_1_2  == 2 | mydata$Q3_1_2 == 3)
mydata$Mouse_dont_own_or_intend <- as.numeric(mydata$Q3_1_2  == 4)

mydata$Keyboard_Owners <- as.numeric(mydata$Q3_1_3  == 1 | mydata$Q3_1_3 == 2)
mydata$Keyboard_Intenders <- as.numeric(mydata$Q3_1_3  == 2 | mydata$Q3_1_3 == 3)
mydata$Keyboard_dont_own_or_intend <- as.numeric(mydata$Q3_1_3  == 4)

mydata$Surface_Owners <- as.numeric(mydata$Q3_1_4  == 1 | mydata$Q3_1_4 == 2)
mydata$Surface_Intenders <- as.numeric(mydata$Q3_1_4  == 2 | mydata$Q3_1_4 == 3)
mydata$Surface_dont_own_or_intend <- as.numeric(mydata$Q3_1_4  == 4)

mydata$Controller_Owners <- as.numeric(mydata$Q3_1_5  == 1 | mydata$Q3_1_5 == 2)
mydata$Controller_Intenders <- as.numeric(mydata$Q3_1_5  == 2 | mydata$Q3_1_5 == 3)
mydata$Controller_dont_own_or_intend <- as.numeric(mydata$Q3_1_5  == 4)

# shoppers
mydata$Online_Shoppers	<- as.numeric(mydata$Q4Z2 == 1 | mydata$Q4Z2 == 2) # Online Shoppers	if Q4Z3 = 1 OR Q4Z3 = 2
mydata$SteelSeries_Online_Shoppers	<- as.numeric((mydata$Q4Z2 == 1 | mydata$Q4Z2 == 2) & mydata$Q7_4_23 == 1) # SteelSeries Owners Online Shoppers
mydata$SteelSeries_Physical_Shoppers <- as.numeric(mydata$Q4Z2 == 3 & mydata$Q7_4_23 == 1) # 	SteelSeries Owners Physical Shoppers

# Franchises Aware =================================
for(i in 1:42)
{
  cmd1 <- paste0('mydata$Q8_1_',i,' <- NA')
  cmd2 <- paste0('mydata$Q8_1_',i,'[mydata$Q2_4_',i,' %in% c(1:4)]', '<- 1')
  cmd3 <- paste0('mydata$Q8_1_',i,'[is.na(mydata$Q8_1_',i,') & mydata$QActive_PC_or_Console_Gamers == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Franchises played (past month)  ======
for(i in 1:42)
{
  cmd1 <- paste0('mydata$Q8_2_',i,' <- NA')
  cmd2 <- paste0('mydata$Q8_2_',i,'[mydata$Q2_4_',i,'== 1]', '<- 1')
  cmd3 <- paste0('mydata$Q8_2_',i,'[is.na(mydata$Q8_2_',i,') & mydata$QActive_PC_or_Console_Gamers == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Franchises played (past 3 months)  =================================
for(i in 1:42)
{
  cmd1 <- paste0('mydata$Q8_3_',i,' <- NA')
  cmd2 <- paste0('mydata$Q8_3_',i,'[mydata$Q2_4_',i,'%in% c(1,2)]', '<- 1')
  cmd3 <- paste0('mydata$Q8_3_',i,'[is.na(mydata$Q8_3_',i,') & mydata$QActive_PC_or_Console_Gamers == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Brand attributes - strongly agree + agree combinations per brand ================

# Astro Q3_2_1
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_1_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_1_',i,'[mydata$Q3_3_1_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_1_',i,'[is.na(mydata$Q9_1_1_',i,') & mydata$Q3_2_1 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Corsair Q3_2_4
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_4_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_4_',i,'[mydata$Q3_3_4_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_4_',i,'[is.na(mydata$Q9_1_4_',i,') & mydata$Q3_2_4 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# HyperX Q3_2_7
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_7_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_7_',i,'[mydata$Q3_3_7_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_7_',i,'[is.na(mydata$Q9_1_7_',i,') & mydata$Q3_2_7 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Logitech G Q3_2_9
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_9_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_9_',i,'[mydata$Q3_3_9_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_9_',i,'[is.na(mydata$Q9_1_9_',i,') & mydata$Q3_2_9 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# PowerA Q3_2_14
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_14_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_14_',i,'[mydata$Q3_3_14_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_14_',i,'[is.na(mydata$Q9_1_14_',i,') & mydata$Q3_2_14 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Razer Q3_2_17
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_17_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_17_',i,'[mydata$Q3_3_17_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_17_',i,'[is.na(mydata$Q9_1_17_',i,') & mydata$Q3_2_17 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Roccat Q3_2_19
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_19_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_19_',i,'[mydata$Q3_3_19_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_19_',i,'[is.na(mydata$Q9_1_19_',i,') & mydata$Q3_2_19 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# SteelSeries Q3_2_23
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_23_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_23_',i,'[mydata$Q3_3_23_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_23_',i,'[is.na(mydata$Q9_1_23_',i,') & mydata$Q3_2_23 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# Turtle Beach Q3_2_26
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_26_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_26_',i,'[mydata$Q3_3_26_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_26_',i,'[is.na(mydata$Q9_1_26_',i,') & mydata$Q3_2_26 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# JBL Quantum Q3_2_34
for(i in 1:17)
{
  cmd1 <- paste0('mydata$Q9_1_34_',i,' <- NA')
  cmd2 <- paste0('mydata$Q9_1_34_',i,'[mydata$Q3_3_34_',i,' %in% c(4,5)]', '<- 1')
  cmd3 <- paste0('mydata$Q9_1_34_',i,'[is.na(mydata$Q9_1_34_',i,') & mydata$Q3_2_34 == 1] <- 0')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}

# apply western NA rules ----
apply_western_NA_rules <- function(mydata, western_NA_rules)
{
  for(i in 1:nrow(western_NA_rules))
  {
    mydata[[western_NA_rules[i,]$variable_name_NEW]][mydata$country_code != western_NA_rules[i,]$country_code] <- NA
  }
  return(mydata)
}

mydata <- apply_western_NA_rules(mydata, western_NA_rules)

# apply China specific NA rules ----
apply_china_NA_rules <- function(mydata, china_NA_rules)
{
  for(i in 1:nrow(china_NA_rules))
  {
    mydata[[china_NA_rules[i,]$variable_name_NEW]][mydata$country_code == china_NA_rules[i,]$country_code] <- NA
  }
  return(mydata)
}

mydata <- apply_china_NA_rules(mydata, china_NA_rules)

# PL + CN exceptions

# replace all values with missing for these questions (Tritton - 2 nd brand - empty for China or Poland only)
NA_for_CN_and_PL <- c('Q3_2_25', 'Q4A1_25', 'Q4A3A25', 'Q4A4_25')


replace_CN_PL_values_with_NA <- function(mydata, NA_for_CN_and_PL){
  for (i in 1:length(NA_for_CN_and_PL))
  {
    mydata[[NA_for_CN_and_PL[i]]][mydata$country_code %in% c('CN', 'PL')] <- NA 
  }
  return(mydata)
}

mydata <- replace_CN_PL_values_with_NA(mydata, NA_for_CN_and_PL)


# # replace all values with missing for these questions (Bloody - 3 rd brand - empty for all countries except China and Poland)
NA_for_everything_except_CN_and_PL <- c('Q3_2_2', 'Q4A1_2', 'Q4A3A2', 'Q4A4_2', 
                                        'Q4B1_2', 'Q4B3A2', 'Q4B4_2', 
                                        'Q4C1_2', 'Q4C3A2', 'Q4C4_2',
                                        'Q4D1_2', 'Q4D3A2', 'Q4D4_2')

replace_values_with_NA_except_for_CN_or_PL <- function(mydata, NA_for_everything_except_CN_and_PL){
  for (i in 1:length(NA_for_everything_except_CN_and_PL))
  {
    mydata[[NA_for_everything_except_CN_and_PL[i]]][mydata$country_code %!in% c('CN', 'PL')] <- NA 
  }
  return(mydata)
}

mydata <- replace_values_with_NA_except_for_CN_or_PL(mydata, NA_for_everything_except_CN_and_PL)


# row based filters and white/blacklist ----
# exceptions to the filter rules - force keep these participants
mydata_whitelist_participants <- mydata[mydata$SSI_PID %in% participantWhitelist$SSI_PID, ]

# mydata <- mydata[,which(colnames(mydata) %!in% variablesToDelete$variable_name_NEW)] # DROP UNWANTED VARIABLES
mydata <- apply_row_based_filters(mydata, rowBasedFilters, verbose=TRUE) # apply row based filters
mydata_whitelist_participants <- mydata_whitelist_participants[which(mydata_whitelist_participants$SSI_PID %!in% mydata$SSI_PID),] # avoid duplicate entries for non-filtered whitelisted participants
mydata <- rbind(mydata,mydata_whitelist_participants)

# apply blacklist
mydata <- mydata[which(mydata$SSI_PID %!in% participantBlacklist$SSI_PID),]

# # Weights  =====
period_country_age_gender_summary <- mydata %>% group_by(period,
                                                         country_code,
                                                         Q1_2,
                                                         Q1_1) %>% summarize(n=n()) %>% mutate(low_n = n < 50)

lookup_age_bracket_label_to_code <- function(label)
{
  return(which(label == attr(mydata$Q1_2, 'levels')))
}

mydata$age_bracket_code <- apply(as.data.frame(mydata$Q1_2),1,lookup_age_bracket_label_to_code)


period_country_age_gender_summary$age_bracket_code <- apply(as.data.frame(period_country_age_gender_summary$Q1_2),1,lookup_age_bracket_label_to_code)

period_country_age_gender_summary$period_countrycode_agebracket_gender_key <- paste0(period_country_age_gender_summary$period, '_',
                                                                                     period_country_age_gender_summary$country_code, '_',
                                                                                     period_country_age_gender_summary$age_bracket_code, '_',
                                                                                     period_country_age_gender_summary$Q1_1
)

mydata$period_countrycode_agebracket_gender_key <- paste0(mydata$period,'_',
                                                          mydata$country_code, '_',
                                                          mydata$age_bracket_code,'_',
                                                          mydata$Q1_1)

weightsdf <- merge(x = period_country_age_gender_summary, y = weightsGsheets, all.x = TRUE)

weightsdf <- weightsdf %>% select(period,
                                  country_code,
                                  period_countrycode_agebracket_gender_key,
                                  Q1_1,
                                  Q1_2,
                                  n,
                                  low_n,
                                  age_bracket_code,
                                  total_online_n_per_everything_except_device
)


# the $total_online.. column can be character sometimes and the division between numeric and character is not valid.
# also, need to remove the commas in the character column so it is recognized when turning it into as.numeric()
if (sapply(weightsdf, class)$total_online_n_per_everything_except_device == "character") {
  weightsdf$total_online_n_per_everything_except_device <- str_replace_all(weightsdf$total_online_n_per_everything_except_device, "([,])", "")
  weightsdf$weight <- as.numeric(weightsdf$total_online_n_per_everything_except_device) / weightsdf$n
} else {
  weightsdf$weight <- weightsdf$total_online_n_per_everything_except_device / weightsdf$n
}

weightsmergetable <- weightsdf %>% select(period_countrycode_agebracket_gender_key, weight)

mydata <- merge(mydata,
                weightsmergetable,
                all.x = TRUE, by = 'period_countrycode_agebracket_gender_key')

mydata$weight <- round(mydata$weight)
colnames(mydata)[colnames(mydata)=="weight"] <- "Weight"
attr(mydata$Weight, 'label') <- 'Weight'

# Rake weights correction on gamer growth ----
cell_dist_filepath <- paste0('C:/Users/', active_user,'/Newzoo/Consumer Research Syndicated - Documents/Peripheral Brand Tracker/2021/April wave/Rake Weights/peripheral_rake_weights_cells.csv')
cell_dist <- readr::read_csv(cell_dist_filepath)
cell_dist <- cell_dist %>% filter(!is.na(country_code))

gamers_dist_filepath <- paste0('C:/Users/', active_user,'/Newzoo/Consumer Research Syndicated - Documents/Peripheral Brand Tracker/2021/April wave/Rake Weights/peripheral_rake_weights_gamers.csv')
gamers_dist <- readr::read_csv(gamers_dist_filepath)
gamers_dist <- gamers_dist %>% filter(!is.na(country_code))
# (b)


mydata$cell <- NA
# Q1_1 = gender [1 = male, 2 = female]
# Q1_2 = age [1:8]

mydata$cell <- ((as.integer(mydata$Q1_1) - 1) * 8) + (as.integer(mydata$Q1_2)) # age gender combo


# weights per country ----
country_codes <- unique(mydata$country_code)
weighted_data <- list()

for (i in 1:length(country_codes)) {
  current_country_code <- country_codes[i]
  country_data <- mydata[mydata$country_code == current_country_code,]
  
  survey_design <- svydesign(
    ids = ~SSI_PID,
    probs = NULL,
    strata = NULL,
    variables = ~ cell + ~Q1_10,
    fpc = NULL,
    data = country_data,
    nest = FALSE,
    weights = NULL,
    pps = FALSE
  )
  
  # (c)
  country_population_agegender_dist <- cell_dist %>% filter(country_code == current_country_code) %>% select(cell, Freq)
  country_population_gamers_dist <- gamers_dist %>% filter(country_code == current_country_code) %>% select(Q1_10, Freq)
  
  # (d)
  my_rake_object <- rake(survey_design,
                         sample.margins = list(~cell, ~Q1_10),
                         population.margins = list(country_population_agegender_dist, country_population_gamers_dist),
                         control = list(maxit = 99, epsilon = 1, verbose = FALSE), compress = NULL
  )
  
  untrimmed_weights_ok <- TRUE
  
  if (min(weights(my_rake_object)) / mean(weights(my_rake_object)) < 0.3) {
    print(paste0("Minimum weight is too low for country_code ", current_country_code, " - using trimmed weights"))
    untrimmed_weights_ok <- FALSE
  }
  if (max(weights(my_rake_object)) / mean(weights(my_rake_object)) > 3) {
    untrimmed_weights_ok <- FALSE
    print(paste0("Maximum weight is too high for country_code ", current_country_code, " - using trimmed weights"))
  }
  
  lower_bound <- (0.3 * mean(weights(my_rake_object)))
  upper_bound <- (3 * mean(weights(my_rake_object)))
  
  my_rake_object_trimmed <- trimWeights(my_rake_object,
                                        lower = lower_bound, upper = upper_bound,
                                        strict = TRUE
  )
  
  if (untrimmed_weights_ok == TRUE){
    country_data$weights <- weights(my_rake_object)
  } else{
    country_data$weights <- weights(my_rake_object_trimmed)
  }
  weighted_data[[i]] <- country_data
}


mydata <- bind_rows(weighted_data)


# Wrapup =================================
##  end of derived variables section

# reapply answer label overrides to include the derived variables
mydata <- apply_answer_label_overrides(mydata, answerMappingOverride)

# reorder columns alphabetically
mydata <- mydata[ , order(names(mydata))]

# REORDER TO CUSTOM SORT
applicable_custom_variable_sorting <- customVariableSorting[which(customVariableSorting %in% colnames(mydata))] # only use the column names that are actually in the merged df
mydata <- mydata[, c(applicable_custom_variable_sorting, setdiff(names(mydata), applicable_custom_variable_sorting))]

mydata <- apply_question_label_overrides(mydata, variableNameOverride) # reapply question label fixes

mydata <- convert_to_F80(mydata) # convert numerical data type columns to F8.0 SPSS type

## character encoding corrections
# westernCharEncodingAnswerLabels <- get_answer_labels_and_replace_nonwestern_chars(mydata) # extract char encoding corrected mapping
# mydata <- apply_answer_label_overrides(mydata, westernCharEncodingAnswerLabels) # apply the corrected mapping
mydata <- apply_answer_label_overrides(mydata, answerMappingOverride)
mydata <- apply_question_label_overrides(mydata, variableNameOverride) # reapply question label fixes


# haven::write_sav(mydata, 'C:/Users/walter/Newzoo B.V/Consumer Research Syndicated - Documents/Peripheral Brand Tracker/2019/automation/output/latest/Peripheral_2019_including_new_weights.sav')

toc()

# save files ----

save_files <- function()
{
  print('Saving files...')
  tic()
  
  # save to one-drive
  latestGlobalFilepath = paste0(onedrive_project_basepath, "output/latest/Peripheral_2021.sav")
  print(paste0('Starting work on ', latestGlobalFilepath))
  haven::write_sav(mydata[,which(colnames(mydata) %!in% variablesToDelete$question_id)], path = latestGlobalFilepath) # write to Latest
  
  dateFolderinOneDriveLocation <- paste0(onedrive_project_basepath, "output/",lubridate::today())
  if(!dir.exists(dateFolderinOneDriveLocation))
  {
    #   drop_create(paste0("Newzoo/Clients - Documents/- wizards of the coast/Client Analysis/Custom wizards project - Global 2019/Magic The Gathering/June 2019/automation", "output/",lubridate::today()))
    dir.create(file.path(onedrive_project_basepath, "output/",lubridate::today()))
    Sys.sleep(10) # wait until dropbox/onedrive is 'aware' of the new folder
  }
  
  dateSpecificGlobalFilePath <- paste0(dateFolderinOneDriveLocation, "/Peripheral_2021 ", lubridate::today(),".sav")
  print(paste0('Starting work on ', dateSpecificGlobalFilePath))
  haven::write_sav(mydata[,which(colnames(mydata) %!in% variablesToDelete$question_id)], path = dateSpecificGlobalFilePath) # write to date subfolder
  
  for (i in 1:length(unique(mydata$country_code)))
  {
    # 'latest' folder
    country_onedrive_filepath_latest <- paste0(onedrive_project_basepath, "/output/latest/Peripheral_2021_", unique(mydata$country_code)[i],".sav")
    print(paste0('Starting work on ', country_onedrive_filepath_latest))
    country_data <- mydata[mydata$country_code == (unique(mydata$country_code)[i]),]
    country_data <- country_data[,which(colnames(country_data) %!in% variablesToDelete$question_id)]
    country_data <- apply_question_label_overrides(country_data, variableNameOverride)
    haven::write_sav(country_data, path = country_onedrive_filepath_latest)
    
    # date specific folder
    country_onedrive_filepath_today <- paste0(dateFolderinOneDriveLocation, "/Peripheral_2021 ", lubridate::today(), " ",unique(mydata$country_code)[i],".sav")
    print(paste0('Starting work on ', country_onedrive_filepath_today))
    haven::write_sav(country_data, path = country_onedrive_filepath_today)
  }
  toc()
  print('Done saving files! :D')
}

save_files()