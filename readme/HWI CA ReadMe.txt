Data necessary for running the BRT models includes the CAdata_obs.csv file (the raw data used to run the models) and the CA_vars_meta table (a metadata file which is also used to direct the R scripts to response and explanatory variables as well as which indicators to use in creating MMIs). These should be read into an R workspace according to the instructions included in the R_Scripts folder.


File List:

CAdata_obs.csv - Raw data used for modeling, where the response variables include observed values only. This data would be used for creating new models from scratch.

CAdata_preds.csv - Raw data with predicted values for response variables (based on GBM modeling) merged into the existing observed data. This file is useful for calculating MMIs without requiring creation of models first.

CA_MMIs_wMod.csv - MMI results for the entire state of California

CA_MMIs_noMod.csv - MMI results for areas outside of the Desert Modoc regions (includes indices only calculated outside of Desert Modoc regions)

CA_NormIndicators.csv - Rank-normalized indicators for the entire state of California

CA_NormIndicators_noMod.csv - Rank-normalized indicators areas outside of the Desert Modoc regions

CA_DirAlignedNormIndicators.csv - Directionally-aligned rank-normalized indicators for the entire state of California

CA_DirAlignedNormIndicators_noMod.csv - Directionally-aligned rank-normalized indicators for areas outside of the Desert Modoc regions

CA_RelativeInfluences.csv - Relative influence values for every predictor variable in each model

HWI_California.gdb - Geodatabase with the California catchments shapefile and all tables

Correlation_wMod.xlsx - Results of Spearman correlation between all MMI values and all variable values for the entire state of California

Correlation_noMod.xlsx - Results of Spearman correlation between all MMI values and all variable values for areas outside of the Desert Modoc region

CAcatchments_noModoc.csv - A one-column table with catchment IDs for all catchments outside of the Deserts/Modoc region