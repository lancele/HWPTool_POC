gofNames <- data.frame(Statistic = c('Mean Error', 
                                     'Mean Absolute Error', 
                                     'Mean Squared Error', 
                                     'Root Mean Square Error', 
                                     'Normalized Root Mean Square Error', 
                                     'Percent Bias (\\%)', 
                                     'Ratio of RMSE to Std Dev of Obs', 
                                     'Ratio of Standard Deviations', 
                                     'Nash-Sutcliffe Efficiency', 
                                     'Modified Nash-Sutcliffe Efficiency', 
                                     'Relative Nash-Sutcliffe Efficiency', 
                                     'Index of Agreement', 
                                     'Modified Index of Efficiency', 
                                     'Relative Index of Efficiency', 
                                     'Persistence Index', 
                                     'Pearson Correlation',
                                     'Coefficient of Determination', 
                                     'R2 Multiplied by Slope of Regression Line', 
                                     'Kling-Gupta Efficiency', 
                                     'Volumetric Efficiency',
                                     'Spearman Correlation'),
                       Symbol = c('\\mathrm{ME}', 
                                  '\\mathrm{MAE}', 
                                  '\\mathrm{MSE}', 
                                  '\\mathrm{RMSE}', 
                                  '\\mathrm{nRMSE}', 
                                  '\\mathrm{PBIAS}', 
                                  '\\sigma_{RMSE}', 
                                  '\\sigma_{r}', 
                                  '\\mathrm{NSE}', 
                                  '\\mathrm{NSE_{m}}', 
                                  '\\mathrm{NSE_{r}}', 
                                  'd', 
                                  'd_{m}', 
                                  'd_{r}', 
                                  '\\mathrm{PI}', 
                                  'r', 
                                  'R^{2}',
                                  'bR^{2}', 
                                  '\\mathrm{KGE}', 
                                  '\\mathrm{VE}',
                                  '\\rho'),
                       Min = c('-\\infty', 
                               '-\\infty', 
                               '0',
                               '0', 
                               '-100\\%', 
                               '-\\infty\\%',
                               '0', 
                               '-\\infty', 
                               '-\\infty',
                               '-\\infty',
                               '-\\infty',
                               '0',
                               '0',
                               '0',
                               '0',
                               '-1',
                               '0',
                               '0',
                               '0',
                               '-\\infty',
                               '-1'),
                       Max = c('\\infty', 
                               '\\infty', 
                               '\\infty',
                               '\\infty', 
                               '100\\%', 
                               '\\infty\\%',
                               '\\infty', 
                               '\\infty', 
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1',
                               '1')
)

gofExclude <- c(2, 4, 5, 7, 10, 11, 13, 14, 18, 19, 20)


if( FALSE ){
  vars_meta <- read.csv('tables/CA_vars_meta.csv')
  MMI_names <- c('Watershed', 'Anthropogenic', 'ClimateChange', 'Fire', 
                 'LandCover','Landscape', 'Natural', 'WaterUse',
                 'PhysBio', 'WaterQuality', 'InstreamBio')
  ind_class <- vars_meta[, c('MMIclass', 'MMIclass2', 'MMIclass3')] 
  vars_meta_MMI <- matrix(nrow = nrow(vars_meta), ncol = length(MMI_names))
  n <- 0
  for(i in MMI_names){
    n <- n + 1
    ind_out <- rep(FALSE, length = nrow(vars_meta))
    for(j in 1:3){
      ind <- ind_class[, j] %in% i
      ind_out <- ind | ind_out
    }
    vars_meta_MMI[, n] <- ind_out
  }
  indDir <- rep(NA, nrow(vars_meta))
  indDir[vars_meta$IndDirection == 'good'] <- TRUE
  indDir[vars_meta$IndDirection == 'bad'] <- FALSE
  vars_meta_MMI <- cbind(vars_meta[, c('shortname', 'fullnames')],
                         indDir, vars_meta_MMI)
  names(vars_meta_MMI) <- c('shortname', 'fullnames', 'PositiveDir', MMI_names)
  write.csv(vars_meta_MMI, 'tables/CA_vars_meta_MMI.csv', row.names = F)
}