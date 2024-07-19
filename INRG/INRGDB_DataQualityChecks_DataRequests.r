##################################################################
##  INSTALL AND LOAD REQUIRED LIBRARIES
##################################################################
# Package names
packages <- c("purrr","dplyr","openxlsx")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##################################################################
##  READ DATA FROM FILE
##################################################################
inFile_dir <- dest_dir
inFile_name <- dest_file_name

currentDataSet <- read.csv(paste(inFile_dir, inFile_name, sep = ''),na.strings = c('NULL'))

##################################################################
##  SETUP OUTPUT TO XLSX
##################################################################
#outFile_dir <- '/Users/bfurner/Downloads/'
outFile_dir <- dest_dir
outFile_name <- paste("inrgqc_",format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
outFile <- paste(outFile_dir, outFile_name,sep = "")

wb <- createWorkbook()

writeToOutfile <- function(myData, sheetName) {
  # name <- deparse(substitute(myData))
  addWorksheet(wb, sheetName)
  writeData(wb, sheetName, myData)
}

##################################################################
##  ERROR HANDLING
##################################################################
command_runner <- function(expr_to_run) {
  tryCatch(expr = 
             {
               expr_to_run 
              },
           error = function(e){         
             print("There was an error message.")
           }
  )
}  

##################################################################
##  INRG
##################################################################
#Convert all column names to lowercase for later processing
colnames(currentDataSet) <- tolower(colnames(currentDataSet))
#
#All fields will be interpreted as integers so we need to force categorical variables to be interpreted as factors
#
# list of categorical variables to cast
my.factors <- c("init_treat",
                "pri_adre","pri_abdret","pri_neck","pri_thor","pri_pelv","pri_oth",
                "inss_stage",
                "ev_stg",
                "mycn",
                "ploidy",
                "hist",
                "diag",
                "grade",
                "mki",
                "met_bm","met_bone","met_dln","met_liv","met_skin","met_lung","met_cns","met_oth",
                "efscens","scens","gender","race","ethnicity","x11q_ubab","x1p_loab","x17q_gain",
                "cause_of_death","inrg_stage","second_malig_cens","rel_site_gen")

my.ints <- c("age","year","stime","efstime","second_malig_time")

# function that will cast variables
cast.as.factor <- function(columnName) {
  currentDataSet[,columnName] <<- as.factor(currentDataSet[,columnName])
}

cast.as.int <- function(columnName) {
  currentDataSet[,columnName] <<- as.integer(currentDataSet[,columnName])
}

# lapply factor cast function to that list
lapply(my.factors, cast.as.factor)
lapply(my.ints, cast.as.int)

# function that will reset blank values to NAs
fnc.set.blank.to.na <- function(columnName) {
  currentDataSet[,columnName] <<- replace(currentDataSet[,columnName], currentDataSet[,columnName]=="",NA)
  currentDataSet[,columnName] <<- replace(currentDataSet[,columnName], currentDataSet[,columnName]=="           .",NA)
  #currentDataSet[,columnName] <<- droplevels(currentDataSet[,columnName])
}

# list of variables from which to remove blanks
my.smn.vars <- c("smn_morph_icdo","smn_morph_sno","smn_morph_txt","relapse_site_specific")

# lapply reset blank function to that list
lapply(my.smn.vars, fnc.set.blank.to.na)



#
#create subsets
#
## patients with mets @ dx
command_runner(
ss.MetsYesAtDx <-
  currentDataSet %>%
  filter(
    met_bm == "1" |
      met_bone == "1" |
      met_cns == "1" |
      met_dln == "1" |
      met_liv == "1" |
      met_lung == "1" |
      met_oth == "1" |
      met_skin == "1"
  )
)
## patients with unknown mets @ dx
command_runner(
ss.MetsUnkAtDx <-
  currentDataSet %>%
  filter(
    met_bm == "9" &
      met_bone == "9" &
      met_cns == "9" &
      met_dln == "9" &
      met_liv == "9" &
      met_lung == "9" &
      met_oth == "9" &
      met_skin == "9"
  )
)

## patients with mets @ dx and inss stage in (1,2,3,4)
command_runner(
ss.dqc1 <- ss.MetsYesAtDx %>% 
  filter(inss_stage %in% c(1, 2, 3, 4)) %>% 
  mutate(., action='')
)

## patients with mets @ dx and inrg stage in (1,2)
command_runner(
ss.dqc2 <- ss.MetsYesAtDx %>% 
  filter(inrg_stage %in% c(1, 2)) %>% 
  mutate(., action='')
)

## INSS stage 4s, but >365 days old at diagnosis
command_runner(
  ss.dqc3 <- currentDataSet %>% 
  filter(inss_stage=="6" & age>365) %>% 
  mutate(., action='')
)

##INRGSS is MS, but >547 days old at diagnosis
command_runner(
  ss.dqc4 <- currentDataSet %>% 
  filter(inrg_stage=="4" & age>547) %>% 
  mutate(., action='')
)

##Negative values for age, year, DNA index, LDH, ferritin, EFSTIME, STIME, SECOND_MALIG_TIME
command_runner(
  ss.dqc6 <-
  currentDataSet %>%
  filter(age < 0 |
           year < 0 |
           ldh < 0 |
           ferritin < 0 |
           efstime < 0 |
           stime < 0 |
           second_malig_time < 0) %>%
  filter(efstime != -999 | stime != -999) %>% 
  mutate(., action='')
)

command_runner(
  ss.dqc8 <-
  currentDataSet %>%
  filter(age > 30000) %>% 
  mutate(., action='')
)

command_runner(
  ss.dqc9 <-
  currentDataSet %>%
  filter(year < 1970 | year > as.integer(format(Sys.Date(), "%Y"))) %>% 
  mutate(., action='')
)

command_runner(
  ss.dqc10 <-
  currentDataSet %>%
  filter(efstime > 22000 |
           stime > 22000 |
           second_malig_time > 22000) %>% 
  mutate(., action='')
)

command_runner(
  ss.dqc11 <-
  currentDataSet %>%
  filter(cause_of_death %in% c(0, 1, 2, 3)) %>%
  filter(scens=="9") %>% 
  mutate(., action='')
)

#INSS stage 4 or 4s, but no sites of mets at diagnosis reported
command_runner(
  ss.dqc12 <-
  ss.MetsUnkAtDx %>%
  filter(inss_stage %in% c(5, 6)) %>% 
  mutate(., action='')
)

#INRGSS M or MS, but no sites of mets at diagnosis reported
command_runner(
  ss.dqc13 <-
  ss.MetsUnkAtDx %>%
  filter(inrg_stage %in% c(3, 4)) %>% 
  mutate(., action='')
)

#Missing data for: AGE, YEAR, INIT_TREAT, INIT_TRIAL, MYCN, PLOIDY, DNA_INDEX, _11Q_UBAB, _1P_LOAB, _17Q_GAIN, ALK, FERRITIN, LDH, PRI_*, HIST, DIAG, GRADE, MKI, EFSCENS, EFSTIME, SCENS, STIME, RACE, ETHNICITY, SEX
command_runner(
  ss.dqc14 <-
  currentDataSet %>%
  filter(is.na(age) |
           is.na(year) |
           is.na(init_treat) | 
           is.na(init_trial) |
           is.na(mycn) |
           is.na(`x11q_ubab`) |
           is.na(`x1p_loab`) |
           is.na(`x17q_gain`) |
           is.na(ferritin) | 
           is.na(ldh) |
           is.na(hist) |
           is.na(diag) |
           is.na(grade) |
           is.na(mki) |
           is.na(efscens) |
           is.na(efstime) |
           is.na(scens) |
           is.na(stime) |
           is.na(race) |
           is.na(ethnicity) |
           is.na(gender)) %>% 
  mutate(., action='')
)

#Missing data for INSS_STAGE and INRG_STG
command_runner(
  ss.dqc15 <-
  currentDataSet %>%
  filter(inrg_stage=="9" & inss_stage=="9") %>% 
  mutate(., action='')
)

#SECOND_MALIG_CENS=1, but missing data for SMN_MORPH_ICD0 and SMN_TOP_SNO and SMN_TOP_ICD0
command_runner(
  ss.dqc16 <-
  currentDataSet %>%
  filter(second_malig_cens=="1") %>%
  filter(is.na(smn_morph_icdo) & is.na(smn_top_sno) & is.na(smn_top_icdo)) %>% 
  mutate(., action='')
)
#Pt had an event (EFSCENS=1), but REL_SITE_GEN or RELAPSE_SITE_SPECIFIC is missing
command_runner(
  ss.dqc17 <-
    currentDataSet %>%
    filter(efscens=="1") %>%
    filter(is.na(rel_site_gen) | is.na(relapse_site_specific)) %>% 
    mutate(., action='')
)
#Pt died (SCENS=1), but CAUSE_OF_DEATH is missing
command_runner(
  ss.dqc18 <-
    currentDataSet %>%
    filter(scens=="1" & is.na(cause_of_death)) %>% 
    mutate(., action='')
)
##################################################################
##  CREATE DATA FRAME TO HOLD SUMMARY SHEET INFORMATION
##################################################################
dfSumm <- data.frame(
  QcRuleNum=c('DQC 1', 
              'DQC 2', 
              'DQC 3', 
              'DQC 4', 
              'DQC 6', 
              'DQC 8', 
              'DQC 9', 
              'DQC 10', 
              'DQC 11', 
              'DQC 12', 
              'DQC 13', 
              'DQC 14', 
              'DQC 15', 
              # 'DQC 16', 
              'DQC 17', 
              'DQC 18')
  , QcRuleDescr=c('Mets at diagnosis reported, but INSS stage is one of Stage 1 [1], Stage 2a [2], Stage 2b [3], Stage 3 [4]',     #DQC1
                  'Mets at diagnosis reported, but INRGSS is one of: Stage L1 [1], Stage L2 [2]',                                  #DQC2
                  'INSS stage 4s [6], but >365 days old at diagnosis',                                       #DQC3
                  'INRGSS is MS [4], but >547 days old at diagnosis',                                        #DQC4
                  'Negative values for age, year, LDH, ferritin, EFSTIME, STIME, SECOND_MALIG_TIME',     #DQC6
                  'Age >30,000 days (~80 years)',                                                        #DQC8
                  'YEAR<1970 or YEAR>current date',                                                      #DQC9 
                  'EFSTIME or STIME or SECOND_MALIG_TIME >22,000 days (~60 years)',                      #DQC10
                  'Cause of death is known, but OS censoring flag (SCENS) and time to death/last contact (STIME) are missing',     #DQC11
                  'INSS stage 4 [5] or 4s [6], but no sites of mets at diagnosis reported',                      #DQC12
                  'INRGSS M [3] or MS [4], but no sites of mets at diagnosis reported',                          #DQC13
                  'Missing data for: AGE, YEAR, INIT_TREAT, INIT_TRIAL, MYCN, PLOIDY, _11Q_UBAB, _1P_LOAB, _17Q_GAIN, ALK, FERRITIN, LDH, PRI_*, HIST, DIAG, GRADE, MKI, EFSCENS, EFSTIME, SCENS, STIME, RACE, ETHNICITY, SEX',     #DQC14
                  'Missing data for INSS_STAGE and INRG_STG',                                            #DQC15
                  # 'SECOND_MALIG_CENS=1, but missing data for SMN_MORPH_ICD0 and SMN_TOP_SNO and SMN_TOP_ICD0',     #DQC16
                  'Pt had an event (EFSCENS=1), but REL_SITE_GEN or RELAPSE_SITE_SPECIFIC is missing',   #DQC17
                  'Pt died (SCENS=1), but CAUSE_OF_DEATH is missing'                                     #DQC18
  )
  , Category=c(
                  'RED'
                  ,'RED'
                  ,'RED'
                  ,'RED'
                  ,'YELLOW'
                  ,'YELLOW'
                  ,'YELLOW'
                  ,'YELLOW'
                  ,'YELLOW'
                  ,'GREEN'
                  ,'GREEN'
                  ,'GREEN'
                  ,'GREEN'
                  ,'GREEN'
                  # ,'GREEN'
                  ,'GREEN'
  )
  , NumMatchingCases=c(nrow(ss.dqc1),nrow(ss.dqc2),nrow(ss.dqc3),nrow(ss.dqc4),nrow(ss.dqc6),nrow(ss.dqc8)
               , nrow(ss.dqc9),nrow(ss.dqc10),nrow(ss.dqc11),nrow(ss.dqc12),nrow(ss.dqc13)
               , nrow(ss.dqc14),nrow(ss.dqc15),
               # nrow(ss.dqc16),
               nrow(ss.dqc17),nrow(ss.dqc18))
  , TotalCases=c(nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet)
                       , nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet),nrow(currentDataSet)
                       , nrow(currentDataSet),
                 # nrow(currentDataSet),
                 nrow(currentDataSet),nrow(currentDataSet),
                 nrow(currentDataSet))
)

command_runner(writeToOutfile(dfSumm, "Summary Sheet"))
command_runner(writeToOutfile(ss.dqc1, "DQC 1"))
command_runner(writeToOutfile(ss.dqc2, "DQC 2"))
command_runner(writeToOutfile(ss.dqc3, "DQC 3"))
command_runner(writeToOutfile(ss.dqc4, "DQC 4"))
command_runner(writeToOutfile(ss.dqc6, "DQC 6"))
command_runner(writeToOutfile(ss.dqc8, "DQC 8"))
command_runner(writeToOutfile(ss.dqc9, "DQC 9"))
command_runner(writeToOutfile(ss.dqc10, "DQC 10"))
command_runner(writeToOutfile(ss.dqc11, "DQC 11"))
command_runner(writeToOutfile(ss.dqc12, "DQC 12"))
command_runner(writeToOutfile(ss.dqc13, "DQC 13"))
command_runner(writeToOutfile(ss.dqc14, "DQC 14"))
command_runner(writeToOutfile(ss.dqc15, "DQC 15"))
command_runner(writeToOutfile(ss.dqc16, "DQC 16"))
command_runner(writeToOutfile(ss.dqc17, "DQC 17"))
command_runner(writeToOutfile(ss.dqc18, "DQC 18"))

##################################################################
##  ADD STYLES FOR SUMMARY PAGE
##################################################################
redStyle <- createStyle(fgFill = "#FF0000")
yellowStyle <- createStyle(fgFill = "#FFFF00")
greenStyle <- createStyle(fgFill = "#00FF00")
headerStyle <- createStyle(textDecoration = 'bold')
addStyle(wb, sheet = 1, redStyle, rows = 2:5, cols = 3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, yellowStyle, rows = 6:10, cols = 3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, greenStyle, rows = 11:17, cols = 3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, createStyle(wrapText = TRUE), rows = 2:17, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:5, gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, createStyle(valign = 'top'), rows=2:17, cols = 1:5, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, sheet = 1, cols = 2, widths = 50.0)

saveWorkbook(wb, file = outFile, overwrite = TRUE)

##################################################################
##  CLEAN UP OBJECTS
##################################################################
rm(list=ls())

