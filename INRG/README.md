# INRG scripts
Repository of scripts to transform PCDC raw data into INRG-specific output

#### Setup the environment
- If you don't have R installed on your machine you can install from [here](https://posit.co/download/rstudio-desktop/)

#### PCDC_To_INRG_Data_Transformation.R
This script takes as input PCDC tsv files from the PCDC Data Portal and produces as output 2 csv files

- INRG_analytical_file_labels.csv
    - output with a single row per participant and lookup values coded to INRG-defined specification
- INRG_analytical_file_codes.csv
    - output with a single row per participant and values provided as text rather than coded to INRG-defined specification

In order to run this script, there are 3 variables that must be set 

- SET `analytic_scripts_repo_dir` to the directory where the pcdc_analysis_scripts repository resides (e.g., '/Users/jdoe/src/pcdc_analysis_scripts/')
- SET `src_dir` to the source directory with PCDC-supplied tsvs (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/tsvs/')
- SET `dst_dir` to the destination directory where you would like the analytic file to be written (e.g., '/Users/jdoe/Downloads/export_2023-09-19T15_45_33/analysis/)

Example: `RScript ./PCDC_To_INRG_Data_Transformation.R path/to/pcdc_analysis_scripts/ path/to/source/ path/to/destination/`

This script makes a call to the next script in order to produce QC output.

#### INRGDB_DataQualityChecks_DataRequests.r
This script, called at the end of the PCDC_To_INRG_Data_Transformation.R script, takes as input INRG-transformed PCDC data and produces an .xlsx file which contains all of the INRG-specific QC output.
