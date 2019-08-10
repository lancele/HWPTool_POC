Running the GBM models requires R version 2.15.2 and installation of the "gbm" package included in the gbm zip folder provided here.

Unzip the gbm folder and copy the unzipped folder into your R library directory (usually in Documents\R\R-2.15.2\library\). Use the library(gbm) function to load the package into your R workspace.

The BRT functions are also required. Source the brt.functions file into your workspace as well (function: source(file="brt.functions.R")) These are from Elith, Leathwick & Hastie (2008), and the "JANE_1390_sm_AppendixS3" folder includes a tutorial for running BRT models.


Instructions for running the models specific to the HWI California project are included in the files:
1. create_gbmdata.R
2. run_gbm_models.R
3. allmod_predict.R
4. create_MMI_tables.R

These are to be followed sequentially to get table outputs. The splitsets.R and createMMI_func_CA.R files are functions that are necessary to create the outputs and will be read in during the process of creating the output tables.

R objects that were used in modeling were included in the R_Objects folder.