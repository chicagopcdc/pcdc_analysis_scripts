##################################################################
# CREATE EMPTY DATA FRAMES
##################################################################

base_fields <- list(
  type  = character(),
  submitter_id = character()
)

disease_characteristic_fields <- list(
  mki = character(),
  nodular_splenic = character(),
  initial_treatment_category = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

histology_fields <- list(
  age_at_hist_assessment = character(),
  histology = character(),
  histology_grade = character(),
  histology_inpc = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

lab_fields <- list(
  age_at_lab = character(),
  lab_test = character(),
  lab_result = character(),
  lab_result_numeric = character(),
  lab_result_unit = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

molecular_analysis_fields <- list(
  age_at_molecular_analysis = character(),
  dna_index = character(),
  molecular_abnormality = character(),
  gene1 = character(),
  gene2 = character(),
  molecular_abnormality_result = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

person_fields <- list(
  sex = character(),
  race = character(),
  ethnicity = character()
)

secondary_malignant_neoplasm_fields <- list(
  age_at_smn = character(),
  smn_yn = character(),
  smn_morph_sno = character(),
  smn_morph_icdo = character(),
  smn_morph_txt = character(),
  smn_top_sno = character(),
  smn_top_icdo = character(),
  smn_top_txt = character(),
  subjects.submitter_id = character()
)

staging_fields <- list(
  age_at_staging = character(),
  stage_system = character(),
  stage = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

study_fields <- list(
  study_id = character(),
  treatment_arm = character(),
  subjects.submitter_id = character()
)

subject_fields <- list(
  honest_broker_subject_id = character(),
  consortium = character(),
  data_contributor_id = character(),
  censor_status = character(),
  age_at_censor_status = character(),
  persons.submitter_id = character()
)

survival_characteristic_fields <- list(
  age_at_lkss = character(),
  lkss = character(),
  cause_of_death = character(),
  cause_of_death_other = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)

timing_fields <- list(
  timing_type = character(),
  disease_phase = character(),
  course = character(),
  disease_phase_number = character(),
  age_at_disease_phase = character(),
  year_at_disease_phase = character(),
  subjects.submitter_id = character()
)

tumor_assessment_fields <- list(
  age_at_tumor_assessment = character(),
  tumor_classification = character(),
  tumor_site = character(),
  tumor_site_other = character(),
  tumor_state = character(),
  subjects.submitter_id = character(),
  timings.submitter_id = character()
)
  
disease_characteristic <- as.data.frame(c(base_fields,disease_characteristic_fields))
histology <- as.data.frame(c(base_fields,histology_fields))
lab <- as.data.frame(c(base_fields,lab_fields))
molecular_analysis <- as.data.frame(c(base_fields,molecular_analysis_fields))
person <- as.data.frame(c(base_fields,person_fields))
secondary_malignant_neoplasm <- as.data.frame(c(base_fields,secondary_malignant_neoplasm_fields))
staging <- as.data.frame(c(base_fields,staging_fields))
study <- as.data.frame(c(base_fields,study_fields))
subject <- as.data.frame(c(base_fields,subject_fields))
survival_characteristic <- as.data.frame(c(base_fields,survival_characteristic_fields))
timing <- as.data.frame(c(base_fields,timing_fields))
tumor_assessment <- as.data.frame(c(base_fields,tumor_assessment_fields))

create.empty.disease.characteristic <- function(){
  return(disease_characteristic)  
}

create.empty.histology <- function(){
  return(histology)  
}

create.empty.lab <- function(){
  return(lab)  
}

create.empty.molecular.analysis <- function(){
  return(molecular_analysis)  
}

create.empty.person <- function(){
  return(person)  
}

create.empty.secondary.malignant.neoplasm <- function(){
  return(secondary_malignant_neoplasm)  
}

create.empty.staging <- function(){
  return(staging)  
}

create.empty.study <- function(){
  return(study)  
}

create.empty.subject <- function(){
  return(subject)  
}

create.empty.staging <- function(){
  return(staging)  
}

create.empty.survival.characteristic <- function(){
  return(survival_characteristic)  
}

create.empty.timing <- function(){
  return(timing)  
}

create.empty.tumor.assessment <- function(){
  return(tumor_assessment)  
}

