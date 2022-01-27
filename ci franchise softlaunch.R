library("survey")
library("dplyr")
library("haven")
library("xlsx")
library("readr")
library('readxl')


filepath <- paste0("C:/Users/niklas.melcher/Newzoo B.V/Consumer Research Syndicated - Documents/2. Consumer Insights - Game Franchises/2021/8. Sample and Soft Launch Tests/SL datasets/newzoo51[23-8-2021] (inc aborts).sav")
mydata <- read_sav(filepath)
mydata <- mydata[mydata$STATE == 1,] #only checking for completes
intenders <- mydata[mydata$G_ITD == 1,]
mydata <- mydata[mydata$G_PLYR == 1,] #only checking for players


setwd("C:/Users/niklas.melcher/Newzoo B.V/Consumer Research Syndicated - Documents/2. Consumer Insights - Game Franchises/2021/8. Sample and Soft Launch Tests")

franchise_codes <- read_excel("franchise_codes.xlsx", sheet = "all")
versions6 <- read_excel("franchise_codes.xlsx", sheet = "versions_6")
versions5 <- read_excel("franchise_codes.xlsx", sheet = "versions_5")
versions4 <- read_excel("franchise_codes.xlsx", sheet = "versions_4")
versions3 <- read_excel("franchise_codes.xlsx", sheet = "versions_3")
versions2 <- read_excel("franchise_codes.xlsx", sheet = "versions_2")
P2P <- read_excel("franchise_codes.xlsx", sheet = "P2P")
DLC <- read_excel("franchise_codes.xlsx", sheet = "DLC")
Meta <- read_excel("franchise_codes.xlsx", sheet = "Meta")
Esports <- read_excel("franchise_codes.xlsx", sheet = "Esports")


# DEVICES
for(i in franchise_codes){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A1)] <- "Problem',i,'"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A2)] <- "Problem',i,'"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A3)] <- "Problem',i,'"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A4)] <- "Problem',i,'"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A5)] <- "Problem',i,'"')
  cmd6 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A6)] <- "Problem',i,'"')
  cmd7 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A7)] <- "Problem',i,'"')
  cmd8 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A8)] <- "Problem',i,'"')
  cmd9 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_DVCS_',i,'A9)] <- "Problem',i,'"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
  eval(parse(text = cmd6))
  eval(parse(text = cmd7))
  eval(parse(text = cmd8))
  eval(parse(text = cmd9))
}
table(mydata$ISSUE)

# VERSIONS
## 6 versions
for(i in versions6){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A3)] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A4)] <- "Problem"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A5)] <- "Problem"')
  cmd6 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A6)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
  eval(parse(text = cmd6))
}
table(mydata$ISSUE)

## 5 versions
for(i in versions5){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A3)] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A4)] <- "Problem"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A5)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
}
table(mydata$ISSUE)

## 4 versions
for(i in versions4){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A3)] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A4)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
}
table(mydata$ISSUE)

## 3 versions
for(i in versions3){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A3)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}
table(mydata$ISSUE)

## 2 versions
for(i in versions3){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_VRSN_',i,'A2)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
}
table(mydata$ISSUE)

# TIME SPENT + MONEY SPENT + NPS
## Time spent, Money spent No, NPS
for(i in franchise_codes){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1) & is.na(mydata$F_TMSPT_',i,')] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MNYSP_',i,'A3)] <- "Problem"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & mydata$F_MNYSP_',i,'A3 == 0 & is.na(mydata$F_MNY_ABS_',i,')] <- "Problem',i,'"')
  cmd6 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & mydata$F_MNYSP_',i,'A3 == 0 & is.na(mydata$F_MNY_',i,')] <- "Problem',i,'"')
  cmd7 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_RCCM_',i,')] <- "Problem',i,'"')
  cmd8 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_NPS_',i,')] <- "Problem',i,'"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
  eval(parse(text = cmd6))
  eval(parse(text = cmd7))
  eval(parse(text = cmd8))
}
table(mydata$ISSUE)

# Money spent P2P
for(i in P2P){
  mydata$ISSUE <- NA
  cmd <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MNYSP_',i,'A1)] <- "Problem"')
  eval(parse(text = cmd))
}
table(mydata$ISSUE) 

# Money spent DLC
for(i in DLC){
  mydata$ISSUE <- NA 
  cmd <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MNYSP_',i,'A2)] <- "Problem"')
  eval(parse(text = cmd))
}
table(mydata$ISSUE) 

# CHURN
for(i in franchise_codes){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A3)] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A4)] <- "Problem"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A5)] <- "Problem"')
  cmd6 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A6)] <- "Problem"')
  cmd7 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A7)] <- "Problem"')
  cmd8 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A8)] <- "Problem"')
  cmd9 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A9)] <- "Problem"')
  cmd10 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A10)] <- "Problem"')
  cmd11 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A11)] <- "Problem"')
  cmd12 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A12)] <- "Problem"')
  cmd13 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3) & is.na(mydata$F_RSCH_',i,'A13)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
  eval(parse(text = cmd6))
  eval(parse(text = cmd7))
  eval(parse(text = cmd8))
  eval(parse(text = cmd9))
  eval(parse(text = cmd10))
  eval(parse(text = cmd11))
  eval(parse(text = cmd12))
  eval(parse(text = cmd13))
}
table(mydata$ISSUE)


# FUTURE PLAY
for(i in franchise_codes){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(3:4) & is.na(mydata$F_PLYFT_',i,')] <- "Problem"')
  eval(parse(text = cmd1))
}
table(mydata$ISSUE)

# METAVERSE
for(i in Meta){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A3)] <- "Problem"')
  cmd4 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A4)] <- "Problem"')
  cmd5 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A5)] <- "Problem"')
  cmd6 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A6)] <- "Problem"')
  cmd7 <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:2) & is.na(mydata$F_MTVRS_',i,'A7)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
  eval(parse(text = cmd4))
  eval(parse(text = cmd5))
  eval(parse(text = cmd6))
  eval(parse(text = cmd7))
}
table(mydata$ISSUE)


# WATCHING
for(i in franchise_codes){
  mydata$ISSUE <- NA
  cmd <- paste0('mydata$ISSUE[mydata$F_POP_',i,' %in% c(1:4) & (mydata$S_WTCH_1 == 1 | mydata$S_WTCH_2 == 1) & is.na(mydata$F_WTCHFR_',i,')] <- "Problem"')
  eval(parse(text = cmd))
}
table(mydata$ISSUE)


for(i in Esports){
  mydata$ISSUE <- NA
  cmd1 <- paste0('mydata$ISSUE[mydata$F_WTCHFR_',i,' == 1 & mydata$S_ESPFQ %in% c(3:4) & is.na(mydata$F_WTTY_',i,'A1)] <- "Problem"')
  cmd2 <- paste0('mydata$ISSUE[mydata$F_WTCHFR_',i,' == 1 & mydata$S_ESPFQ %in% c(3:4) & is.na(mydata$F_WTTY_',i,'A2)] <- "Problem"')
  cmd3 <- paste0('mydata$ISSUE[mydata$F_WTTY_',i,'A2 == 1 & (is.na(mydata$F_WTESP_',i,') | mydata$F_WTESP_',i,' == 0)] <- "Problem"')
  eval(parse(text = cmd1))
  eval(parse(text = cmd2))
  eval(parse(text = cmd3))
}
table(mydata$ISSUE)

# INTENDERS
for(i in franchise_codes){
  intenders$ISSUE <- NA
  cmd <- paste0('intenders$ISSUE[intenders$G_ITD == 1 & intenders$I_INTAW_',i,' == 1 & is.na(intenders$I_INPL_',i,')] <- "Problem"')
  eval(parse(text = cmd))
}
table(intenders$ISSUE)
