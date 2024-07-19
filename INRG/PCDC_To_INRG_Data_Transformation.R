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
#     SET analytic_scripts_repo_dir to the directory where the pcdc_analysis_scripts repository resides (e.g., '/Users/jdoe/src/pcdc_analysis_scripts')
#     SET src_dir to the source directory with PCDC-supplied tsvs (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/tsvs/')
#     SET dst_dir to the destination directory where you would like the analytic file to be written (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/analysis/)
##################################################################
analytic_scripts_repo_dir <- ''    #!!!CHANGE THIS
setwd(paste(analytic_scripts_repo_dir, 'INRG', sep=''))
src_dir <- ''                    #!!!CHANGE THIS
dest_dir <- ''                   #!!!CHANGE THIS
dest_file_name <- paste('INRG_analytical_file_codes.csv')
dest_file_name_labeled <- paste('INRG_analytical_file_labels.csv')

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

# Join Person and Subject files using 'person.submitter_id' = 'subject.persons.submitter'
person_subject <- left_join(person, subject, by = c('submitter_id' = 'persons.submitter_id'), suffix = c('_person', '_subject')) %>% 
  select(c('consortium', 'data_contributor_id', 'submitter_id', 'submitter_id_subject', 'honest_broker_subject_id', 'sex', 'race', 'ethnicity', 'censor_status', 'age_at_censor_status'))

# Left join with disease_characteristic
person_subject <- left_join(person_subject, disease_characteristic %>% select(subjects.submitter_id, mki, initial_treatment_category), by = c('submitter_id_subject' = 'subjects.submitter_id'))

# Left join with histology
person_subject <- left_join(person_subject, histology %>% select(subjects.submitter_id, histology, histology_grade, histology_inpc), by = c('submitter_id_subject' = 'subjects.submitter_id'))

# Left join with molecular_analysis, filtering based on 'dna_index'
person_subject <- left_join(
  person_subject,
  molecular_analysis %>%
    filter(!is.na(dna_index)) %>%  # Filter records where dna_index is not NA
    select(subjects.submitter_id, dna_index),
  by = c('submitter_id_subject' = 'subjects.submitter_id')
)

# Create a temporary table 'temp_lkss_age' without duplicates for 'subjects.submitter_id'
temp_lkss_age <- survival_characteristic %>%
  select(subjects.submitter_id, lkss, age_at_lkss) %>%
  distinct(subjects.submitter_id, .keep_all = TRUE)

# Left join 'person_subject' with 'temp_lkss_age'
person_subject <- left_join(
  person_subject,
  temp_lkss_age,
  by = c('submitter_id_subject' = 'subjects.submitter_id')
)

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

# Filter and select the desired columns
pvt_timing <- timing %>%
  filter(disease_phase == 'Initial Diagnosis' & disease_phase_number == 1) %>%
  select(subjects.submitter_id, age_at_disease_phase, year_at_disease_phase)

# Group and summarize the data to obtain unique values
pvt_timing <- pvt_timing %>%
  group_by(subjects.submitter_id) %>%
  summarise(
    age = first(age_at_disease_phase),
    year = first(year_at_disease_phase)
  )

pvt_study <- study %>% 
  select(c('subjects.submitter_id','study_id')) %>% 
  group_by(subjects.submitter_id) %>% 
  summarise(study_id = str_c(study_id, collapse = ' ;; '))

survival_characteristic$cause_of_death_coded <-
  survival_characteristic$cause_of_death %>%
  as.character() %>%
  case_match("Disease Progression" ~ 0, 
             "Treatment-Related Mortality" ~ 1, 
             "Other" ~ 2, 
             "Unknown" ~ 9, 
             .default = NA)

pvt_survival_characteristic <- survival_characteristic %>%
  select(c('subjects.submitter_id','cause_of_death','cause_of_death_coded')) %>%
  group_by(subjects.submitter_id) %>% 
  summarise(cause_of_death = str_c(cause_of_death, collapse = ','), cause_of_death_coded = str_c(cause_of_death_coded, collapse = ','))

pvt_smn <- secondary_malignant_neoplasm %>%
  select(c('subjects.submitter_id','smn_yn','age_at_smn','smn_morph_icdo', 'smn_morph_sno', 'smn_morph_txt', 'smn_top_icdo', 'smn_top_sno', 'smn_top_txt'))

##################################################################
# CREATE AND POPULATE RELAPSE FIELDS (rel_site_gen and relapse_site_specific)
##################################################################
#primary
df_rel_site_primary <- tumor_assessment %>% 
  filter(disease_phase=='Relapse' & tumor_classification=='Primary' & tumor_state=='Present') %>% 
  select(subjects.submitter_id, tumor_classification, tumor_site, tumor_state) %>% 
  mutate('rel_site_primary'="Yes")
#mets
df_rel_site_metastatic <- tumor_assessment %>% 
  filter(disease_phase=='Relapse' & tumor_classification=='Metastatic' & tumor_state=='Present') %>% 
  select(subjects.submitter_id, tumor_classification, tumor_site, tumor_state) %>% 
  mutate('rel_site_metastatic'="Yes")
#unknown
df_rel_site_unknown <- tumor_assessment %>% 
  filter(disease_phase=='Relapse' & tumor_classification=='Unknown' & tumor_state=='Unknown') %>% 
  select(subjects.submitter_id, tumor_classification, tumor_site, tumor_state) %>% 
  mutate('rel_site_unknown'="Yes")

df_rel_site_all <- bind_rows(df_rel_site_primary, df_rel_site_metastatic, df_rel_site_unknown)

df_rel_site_gen <- left_join(person_subject, df_rel_site_primary,by=c('submitter_id_subject'='subjects.submitter_id')) %>% select('submitter_id_subject', 'rel_site_primary') %>% unique()
df_rel_site_gen <- left_join(df_rel_site_gen, df_rel_site_metastatic,by=c('submitter_id_subject'='subjects.submitter_id')) %>% select('submitter_id_subject', 'rel_site_primary', 'rel_site_metastatic') %>% unique()
df_rel_site_gen <- left_join(df_rel_site_gen, df_rel_site_unknown,by=c('submitter_id_subject'='subjects.submitter_id')) %>% select('submitter_id_subject', 'rel_site_primary', 'rel_site_metastatic', 'rel_site_unknown') %>% unique()
df_rel_site_gen <- df_rel_site_gen %>% mutate(rel_site_gen = NA, rel_site_gen_labeled = NA)

df_rel_site_gen <- df_rel_site_gen %>% mutate(rel_site_gen = rel_site_gen %>% 
                                                replace(rel_site_primary=='Yes' & rel_site_metastatic=='Yes', 3) %>%
                                                replace(rel_site_primary=='Yes' & is.na(rel_site_metastatic), 1) %>%
                                                replace(rel_site_metastatic=='Yes' & is.na(rel_site_primary), 2) %>%
                                                replace(rel_site_unknown=='Yes', 9)
)

df_rel_site_gen <- df_rel_site_gen %>% mutate(rel_site_gen_labeled = rel_site_gen_labeled %>% 
                                                replace(rel_site_primary=='Yes' & rel_site_metastatic=='Yes', 'Primary site and metastatic site(s)') %>%
                                                replace(rel_site_primary=='Yes' & is.na(rel_site_metastatic), 'Primary site only') %>%
                                                replace(rel_site_metastatic=='Yes' & is.na(rel_site_primary), 'Metastatic site(s) only') %>%
                                                replace(rel_site_unknown=='Yes', 'Unknown')
)

df_rel_site_gen <- df_rel_site_gen %>% select('subjects.submitter_id'='submitter_id_subject', 'rel_site_gen', 'rel_site_gen_labeled')

df_rel_site_specific <- df_rel_site_all %>% mutate(relapse_site_specific_coded = NA, relapse_site_specific_labeled=NA)
df_rel_site_specific <- df_rel_site_specific %>% mutate(relapse_site_specific_coded = (relapse_site_specific_coded %>% 
                                                                                         replace(tumor_classification=='Primary',1) %>%
                                                                                         replace(tumor_site=='Bone',2) %>%
                                                                                         replace(tumor_site=='Bone Marrow',3) %>%
                                                                                         replace(tumor_site=='Liver',4) %>%
                                                                                         replace(tumor_site=='Lymph Nodes',5) %>%
                                                                                         replace(tumor_site=='Lung',6) %>%
                                                                                         replace(tumor_site=='Other',7) %>%
                                                                                         replace(tumor_site=='Central Nervous System',8) %>%
                                                                                         replace(tumor_site=='Unknown',9)))

df_rel_site_specific <- df_rel_site_specific %>% mutate(relapse_site_specific_labeled = ifelse(tumor_classification=='Primary','Primary Site', as.character(tumor_site)))

pvt_rel_site_specific <- df_rel_site_specific %>%
  select(c('subjects.submitter_id','relapse_site_specific_coded','relapse_site_specific_labeled')) %>%
  group_by(subjects.submitter_id) %>% 
  summarise(relapse_site_specific_coded = str_c(relapse_site_specific_coded, collapse = ','),
            relapse_site_specific_labeled = str_c(relapse_site_specific_labeled, collapse = ','))

##################################################################
# RENAME FIELDS
##################################################################
labs_cols_to_rename <- c("ferritin"="Ferritin", "ldh"="LDH")
pvt_labs <- pvt_labs %>% rename(any_of(labs_cols_to_rename))

molecular_analysis_cols_to_rename <- c("17Q_GAIN"="17q gain", "mycn"="MYCN Amplification", "1P_LOAB"="1p deletion", "11Q_UBAB"="11q deletion")
pvt_molecular_analysis <- pvt_molecular_analysis %>% rename(any_of(molecular_analysis_cols_to_rename))

study_cols_to_rename <- c("init_trial" = "study_id")
pvt_study <- pvt_study %>% rename(any_of(study_cols_to_rename))

site_cols_to_rename <- c("pri_abdret"="Primary_Abdomen", "pri_adre"="Primary_Adrenal Gland", "pri_neck"="Primary_Neck", "pri_thor"="Primary_Other", "pri_pelv"="Primary_Pelvis", "pri_oth"="Primary_Thorax", "met_bm"="Metastatic_Bone", "met_bone"="Metastatic_Bone Marrow", "met_dln"="Metastatic_Central Nervous System", "met_liv"="Metastatic_Liver", "met_skin"="Metastatic_Lung", "met_lung"="Metastatic_Lymph Nodes", "met_cns"="Metastatic_Other", "met_oth"="Metastatic_Skin")
pvt_sites <- pvt_sites %>% rename(any_of(site_cols_to_rename))

hist_cols_to_rename <- c("diag"="histology","grade"="histology_grade","hist"="histology_inpc")
person_subject <- person_subject %>% rename(any_of(hist_cols_to_rename))

staging_cols_to_rename <- c("inss_stage"="INSS","inrg_stage"="INRGSS","ev_stg"="EVANS")
pvt_staging <- pvt_staging %>% rename(any_of(staging_cols_to_rename))

lkss_cols_to_rename <- c("scens"="lkss", "efscens"="censor_status")
person_subject <- person_subject %>% rename(any_of(lkss_cols_to_rename))

smn_cols_to_rename <- c("second_malig_cens"="smn_yn")
pvt_smn <- pvt_smn %>% rename((any_of(smn_cols_to_rename)))

misc_cols_to_rename <- c("init_treat"="initial_treatment_category", "ploidy"="dna_index", "gender"="sex")
person_subject <- person_subject %>% rename(any_of(misc_cols_to_rename))

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
analytic_data_set <- join_analytic_tables(pvt_survival_characteristic)
analytic_data_set <- join_analytic_tables(pvt_timing)
analytic_data_set <- join_analytic_tables(pvt_study)
analytic_data_set <- join_analytic_tables(pvt_smn)
analytic_data_set <- join_analytic_tables(df_rel_site_gen)
analytic_data_set <- join_analytic_tables(pvt_rel_site_specific)

### calculate time to event from ages
analytic_data_set$stime <-  analytic_data_set$age_at_lkss - analytic_data_set$age
analytic_data_set$efstime <-  analytic_data_set$age_at_censor_status - analytic_data_set$age
analytic_data_set$second_malig_time <-  analytic_data_set$age_at_smn - analytic_data_set$age

#create a copy of the analytic data set. we'll have one set with labels and one set with codes
analytic_data_set_labeled <- analytic_data_set

##################################################################
# RECODE LABELS AS VALUES
##################################################################
analytic_data_set$gender <- 
  analytic_data_set$gender %>% 
  as.character() %>%
  case_match("Male" ~ 1, 
             "Female" ~ 2, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$race <- 
  analytic_data_set$race %>% 
  as.character() %>% 
  case_match("White" ~ 1, 
             "Black or African American" ~ 2,
             "American Indian or Alaska Native" ~ 3,
             "Asian" ~ 4,
             "Native Hawaiian or Other Pacific Islander" ~ 5,
             "Multiracial" ~ 6,
             "Not reported" ~ 98,
             "Unknown" ~ 99,
             .default = NA)

analytic_data_set$ethnicity <- 
  analytic_data_set$ethnicity %>% 
  as.character() %>% 
  case_match("Not Hispanic or Latino" ~ 0,
             "Hispanic or Latino" ~ 1,
             "Unknown" ~ 2,
             .default = NA)

analytic_data_set$mki <- 
  analytic_data_set$mki %>% 
  as.character() %>% 
  case_match("Low (<2% or <100/5,000 cells)" ~ 1,
             "Intermediate (2-4% or 100-200/5,000 cells)" ~ 2,
             "High (>4% or >200/5,000 cells)" ~ 3,
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$diag <- 
  analytic_data_set$diag %>% 
  as.character() %>% 
  case_match("Neuroblastoma (Schwannian Stroma-Poor)" ~ 1,
             "Ganglioneuroblastoma, intermixed (Schwannian Stroma-Rich)" ~ 2,
             "Ganglioneuroma (Schwannian Stroma-Dominant), Maturing Subtype" ~ 3,
             "Ganglioneuroblastoma, Nodular (Composite)" ~ 4,
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$grade <- 
  analytic_data_set$grade %>% 
  as.character() %>% 
  case_match("Undifferentiated or Poorly Differentiated" ~ 0, 
             "Differentiating" ~ 1, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$hist <- 
  analytic_data_set$hist %>% 
  as.character() %>% 
  case_match("Favorable" ~ 0, 
             "Unfavorable" ~ 1, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$inss_stage <- 
  analytic_data_set$inss_stage %>% 
  as.character() %>% 
  case_match("Stage 1" ~ 1, 
             "Stage 2a" ~ 2, 
             "Stage 2b" ~ 3, 
             "Stage 3" ~ 4, 
             "Stage 4" ~ 5, 
             "Stage 4s" ~ 6, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$inrg_stage <- 
  analytic_data_set$inrg_stage %>% 
  as.character() %>% 
  case_match("Stage L1" ~ 1, 
             "Stage L2" ~ 2, 
             "Stage M" ~ 3, 
             "Stage MS" ~ 4, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$ev_stg <- 
  analytic_data_set$ev_stg %>% 
  as.character() %>% 
  case_match("Stage I" ~ 1, 
             "Stage II" ~ 2, 
             "Stage III" ~ 3, 
             "Stage IV" ~ 4, 
             "Stage IVs" ~ 5, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set <- 
  analytic_data_set %>% 
  mutate_at(
    dplyr::vars(starts_with('pri_')), 
    list(
      ~ case_when(.=="Absent" ~ 0, 
                  .=="Present" ~ 1, 
                  .=="Unknown" ~ 9)
    )
  )


analytic_data_set <- 
  analytic_data_set %>% 
  mutate_at(
    dplyr::vars(starts_with('met_')), 
    list(
      ~ case_when(.=="Absent" ~ 0, 
                  .=="Present" ~ 1, 
                  .=="Unknown" ~ 9)
    )
  )

analytic_data_set <- 
  analytic_data_set %>% 
  mutate_at(
    dplyr::vars(one_of('11Q_UBAB', '1P_LOAB', '17Q_GAIN', 'mycn')), 
    list(
      ~ case_when(.=="Absent" ~ 0, 
                  .=="Present" ~ 1, 
                  .=="Unknown" ~ 9)
    )
  )



analytic_data_set$ploidy <- 
  analytic_data_set$ploidy %>% 
  as.character() %>% 
  case_match("DNA Index <= 1 (Hypodiploid, Diploid)" ~ 1, 
             "DNA Index > 1 (Hyperdiploid)" ~ 0, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$scens <- 
  analytic_data_set$scens %>% 
  as.character() %>% 
  case_match("Alive" ~ 0, 
             "Dead" ~ 1, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$init_treat <- 
  analytic_data_set$init_treat %>% 
  as.character() %>% 
  case_match("None (observation)" ~ 0, 
             "Surgery alone" ~ 1, 
             "Conventional-dose chemotherapy (2-8 cycles) plus surgery" ~ 2, 
             "Intensive multi-modality therapy: specific type unknown" ~ 3, 
             "Intensive multi-modality therapy: no stem cell or bone marrow transplant" ~ 4, 
             "Intensive multi-modality therapy: plus stem cell or bone marrow transplant" ~ 5, 
             "Intensive multi-modality therapy: plus stem cell or bone marrow transplant and anti-GD2 antibody" ~ 6, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$efscens <- 
  analytic_data_set$efscens %>% 
  as.character() %>% 
  case_match("Subject is censored (i.e. has had no event(s))" ~ 0, 
             "Subject has had one or more events" ~ 1, 
             "Unknown" ~ 9,
             .default = NA)

analytic_data_set$second_malig_cens <- 
  analytic_data_set$second_malig_cens %>% 
  as.character() %>% 
  case_match("No" ~ 0, 
             "Yes" ~ 1, 
             "Unknown" ~ 9,
             .default = NA)

output_cols <- c('data_contributor_id', 'pt_id'='honest_broker_subject_id', 'age', 'year', 'init_treat', 
                 'pri_adre', 'pri_abdret', 'pri_neck', 'pri_thor', 'pri_pelv', 'pri_oth', 'inss_stage', 'ev_stg', 
                 'mycn', 'ploidy', 'ferritin', 'ldh', 'hist', 'diag', 'grade', 'mki', 
                 'met_bm', 'met_bone', 'met_dln', 'met_liv', 'met_skin', 'met_lung', 'met_cns', 'met_oth', 
                 'efscens', 'efstime', 'scens', 'stime', 'gender', 'race', 'ethnicity', '11Q_UBAB', '1P_LOAB', '17Q_GAIN', 
                 'init_trial', 'inrg_stage', 'second_malig_cens', 'second_malig_time', 'smn_morph_icdo', 
                 'smn_morph_sno', 'smn_morph_txt', 'smn_top_icdo', 'smn_top_sno', 'smn_top_txt')

output_cols_missing <- output_cols[!(output_cols %in% names(analytic_data_set))]

analytic_data_set <- 
  analytic_data_set %>% 
  select(c(all_of(output_cols[!(output_cols %in% output_cols_missing)]), 'cause_of_death'='cause_of_death_coded', 'rel_site_gen', 'relapse_site_specific'='relapse_site_specific_coded'))
analytic_data_set_labeled <- analytic_data_set_labeled %>% select(c(output_cols[!(output_cols %in% output_cols_missing)], 'cause_of_death', 'rel_site_gen'='rel_site_gen_labeled','relapse_site_specific'='relapse_site_specific_labeled'))

analytic_data_set[,output_cols_missing] <- NA
analytic_data_set_labeled[,output_cols_missing] <- NA

##################################################################
# OUTPUT ANALYTIC DATA SET AS CSV FILE
##################################################################
write_csv(analytic_data_set, file = paste(dest_dir, dest_file_name, sep = ''), na='NULL')
write_csv(analytic_data_set_labeled, file = paste(dest_dir, dest_file_name_labeled, sep = ''), na='NULL')

source('./INRGDB_DataQualityChecks_DataRequests.r')
##################################################################
# CLEANUP ENVIRONMENT
##################################################################
rm(list = ls())