##################################################################
# INSTALL AND LOAD tidyverse
##################################################################
# Package names
packages <- c("tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##################################################################
# SET SOURCE AND DESTINATION FOLDERS
#     SET src_dir to the source directory with PCDC-supplied tsvs (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/tsvs/')
#     SET dst_dir to the destination directory where you would like the analytic file to be written (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/analysis/)
##################################################################
src_dir <- ''              #!!!CHANGE THIS
dest_dir <- ''             #!!!CHANGE THIS
dest_file_name <- paste('INRG_analytical_file.csv')
  
##################################################################
# READ FILES FROM SOURCE DIRECTORY INTO DATA FRAMES
##################################################################
disease_characteristic <- read_delim(paste(src_dir,'disease_characteristic.tsv', sep=''), delim='\t')
histology <- read_delim(paste(src_dir,'histology.tsv', sep=''), delim='\t')
lab <- read_delim(paste(src_dir,'lab.tsv', sep=''), delim='\t')
molecular_analysis <- read_delim(paste(src_dir,'molecular_analysis.tsv', sep=''), delim='\t')
person <- read_delim(paste(src_dir,'person.tsv', sep=''), delim='\t')
secondary_malignant_neoplasm <- read_delim(paste(src_dir,'secondary_malignant_neoplasm.tsv', sep=''), delim='\t')
staging <- read_delim(paste(src_dir,'staging.tsv', sep=''), delim='\t')
study <- read_delim(paste(src_dir,'study.tsv', sep=''), delim='\t')
subject <- read_delim(paste(src_dir,'subject.tsv', sep=''), delim='\t')
survival_characteristic <- read_delim(paste(src_dir,'survival_characteristic.tsv', sep=''), delim='\t')
timing <- read_delim(paste(src_dir,'timing.tsv', sep=''), delim='\t')
tumor_assessment <- read_delim(paste(src_dir,'tumor_assessment.tsv', sep=''), delim='\t')

##################################################################
# SUBSELECT FIELDS AND CAST VARIABLES TO PROPER TYPES
##################################################################
disease_characteristic <- disease_characteristic %>% 
  select(c('type','submitter_id', 'mki','initial_treatment_category','subjects.submitter_id','timings.submitter_id')) %>% 
  type_convert(col_types = 'fcffcc')
histology <- histology %>% 
  select(c('type','submitter_id','age_at_hist_assessment', 'histology', 'histology_grade', 'histology_inpc', 'subjects.submitter_id', 'timings.submitter_id')) %>% 
  type_convert(col_types='fcifffcc')
lab <- lab %>% 
  select(c('type','submitter_id','age_at_lab','lab_test','lab_result_numeric','subjects.submitter_id','timings.submitter_id')) %>% 
  type_convert(col_types='fcifncc')
molecular_analysis <- molecular_analysis %>% 
  select(c('type','submitter_id','age_at_molecular_analysis','dna_index','molecular_abnormality','molecular_abnormality_result','subjects.submitter_id','timings.submitter_id')) %>% 
  type_convert(col_types='fcifffcc')
person <- person %>% 
  select(c('type','submitter_id','sex','race','ethnicity')) %>% 
  type_convert(col_types='fcfff')
secondary_malignant_neoplasm <- secondary_malignant_neoplasm %>% 
  select(c('type','submitter_id','age_at_smn','smn_yn','smn_morph_sno','smn_morph_icdo','smn_morph_txt','smn_top_sno','smn_top_icdo','smn_top_txt','subjects.submitter_id')) %>% 
  type_convert(col_types='fcifccccccc')
staging <- staging %>% select(c('type','submitter_id','age_at_staging','stage_system','stage','subjects.submitter_id','timings.submitter_id')) %>% type_convert(col_types='fciffcc')
study <- study %>% 
  select(c('type','submitter_id','study_id','treatment_arm','subjects.submitter_id')) %>% 
  type_convert(col_types='fcffc')
subject <- subject %>% 
  select(c('type','submitter_id','honest_broker_subject_id','consortium','data_contributor_id','censor_status','age_at_censor_status','persons.submitter_id')) %>% 
  type_convert(col_types='fccfffic')
survival_characteristic <- survival_characteristic %>% 
  select(c('type','submitter_id','age_at_lkss','lkss','cause_of_death','cause_of_death_other','subjects.submitter_id','timings.submitter_id')) %>% 
  type_convert(col_types='fciffccc')
timing <- timing %>% 
  select(c('type','submitter_id','timing_type','disease_phase','course','disease_phase_number','age_at_disease_phase','year_at_disease_phase','subjects.submitter_id')) %>% 
  type_convert(col_types='fcffciiic')
tumor_assessment <- tumor_assessment %>% 
  select(c('type','submitter_id','age_at_tumor_assessment','tumor_classification','tumor_site','tumor_site_other','tumor_state','subjects.submitter_id','timings.submitter_id')) %>% 
  type_convert(col_types='fciffcfcc')


##################################################################
# JOIN TABLES
##################################################################
# create helper function that joins an input table with timing references (tbl_to_join) to a corresponding Timing file record using `timing.submitter_id={tbl_to_join}.timings.submitter_id`
join_timing <- function (tbl_to_join){
  tbl_to_return <- left_join(tbl_to_join, timing, by=c('timings.submitter_id'='submitter_id'), suffix=c('','_timing')) %>% select(-c('type_timing','timings.submitter_id','subjects.submitter_id_timing'))
  return(tbl_to_return)
}

disease_characteristic <- join_timing(disease_characteristic)
histology <- join_timing(histology)
lab <- join_timing(lab)
molecular_analysis <- join_timing(molecular_analysis)
staging <- join_timing(staging)
survival_characteristic <- join_timing(survival_characteristic)
tumor_assessment <- join_timing(tumor_assessment)

#join Person and Subject files using `person.submitter_id=subject.persons.submitter`
person_subject <- left_join(person, subject, by=c('submitter_id'='persons.submitter_id'), suffix=c('_person','_subject')) %>% 
  select(c('consortium','data_contributor_id','submitter_id','submitter_id_subject','honest_broker_subject_id','sex','race','ethnicity'))


##################################################################
# PIVOT CERTAIN TABLES TO PRESENT ROW-MODELED DATA AS COLUMNAR
##################################################################
pvt_labs <- lab %>% 
  select(c('subjects.submitter_id','lab_test','lab_result_numeric')) %>% 
  pivot_wider(names_from = lab_test, values_from = lab_result_numeric)

pvt_molecular_analysis <- molecular_analysis %>% 
  select(c('subjects.submitter_id','molecular_abnormality','molecular_abnormality_result')) %>% 
  drop_na('molecular_abnormality') %>% 
  pivot_wider(names_from = molecular_abnormality, values_from = molecular_abnormality_result)

pvt_staging <- staging %>%
  select(c('subjects.submitter_id','stage_system','stage')) %>%
  pivot_wider(names_from = stage_system, values_from = stage)

pvt_sites <- tumor_assessment %>% 
  filter(disease_phase=='Initial Diagnosis' & disease_phase_number==1) %>%
  select(c('subjects.submitter_id','tumor_classification','tumor_site','tumor_state')) %>%
  pivot_wider(names_from = c(tumor_classification,tumor_site), values_from = tumor_state)

##################################################################
# JOIN TOGETHER ANALYTIC DATA SET
##################################################################
analytic_data_set <- person_subject

join_analytic_tables <- function(tbl_to_join){
  tbl_to_return <- left_join(analytic_data_set, tbl_to_join, by=c('submitter_id_subject'='subjects.submitter_id'), suffix=c('l','r'))
  return(tbl_to_return)
}

analytic_data_set <- join_analytic_tables(pvt_labs)
analytic_data_set <- join_analytic_tables(pvt_molecular_analysis)
analytic_data_set <- join_analytic_tables(pvt_staging)
analytic_data_set <- join_analytic_tables(pvt_sites)

###
### TODO: add other fields to the `analytic_data_set` object
###

##################################################################
# OUTPUT ANALYTIC DATA SET AS CSV FILE
##################################################################
write_csv(analytic_data_set, file = paste(dest_dir, dest_file_name, sep = ''))


##################################################################
# CLEANUP ENVIRONMENT
##################################################################
rm(list = ls())

