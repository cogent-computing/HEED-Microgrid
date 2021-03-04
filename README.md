# HEED: Microgrid

## Data repositories

The raw data for Microgrid system is available at the project Data Portal (https://heed-data-portal.coventry.ac.uk/sensor/152) under Systems and Sensors: Kigeme Microgrid. The portal is a one-stop-shop for the raw data collected through project's surveys, sensor and energy monitoring systems and photo reportages. The registration on the portal is free and easy.

The system data used for performance analysis for the paper titled 'Performance analysis of standalone solar systems in refugee camps in Rwanda' is deposited on Zenodo https://doi.org/10.5281/zenodo.3949777

## Analysis scripts

The below scripts have been used for analysis:

* MG_stitching_28May.R - to stitch the raw data together for selected variables
* MG_preprocessing_29May.R - to preprocess raw data and evaluate yield; test different imputation techniques
* MG_imputation_2Jul.R - to impute missing values and correct anomalies using a rule based approach
* MG_analysis_2Jul.R - to analyse and plot corrected data
* MG_predicted.R - to analyse and plot predicted data
* MG_data_upload.R - to prepare files for upload on Zenodo
* MG_HEED_dataPortal_upload.R - to prepare files for upload on Data portal
