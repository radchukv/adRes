#' Produces a table with summary statistics for the specified set
#' of models, as in Supplementary Table S1
#'
#' @inheritParams parameters_definition
#'
#' @return Saves a table analogous to the Supplementary Table S1 to the specified location.
#' @export
#'
tab_efSizes_ST1 <- function(model_list, table_basename){
   data_EfSizes <-
     data.frame(Dataset = c('PRCS', rep(' ', 24), 'PRC', rep(' ', 25)),
     Response = c('Slope of temperature on years',
                  'Slope of phenological traits on temperature',
                  ' ', ' ', ' ',
                  'Slope of morphological traits on temperature',
                  ' ', ' ', #' ',
                  'Mean selection across phenological traits (presumably induced by temperature)',
                   rep(' ', 4),
                   'Mean selection across morphological traits (presumably induced by temperature)',
                   rep(' ', 4),
                   'Slope of precipitation on years',
                   'Slope of phenological traits on precipitation',
                   'Mean selection across phenological traits (presumably induced by precipitation)',
                   rep(' ', 4),
                   'Slope of temperature on years',
                   'Slope of phenological traits on temperature',
                   rep(' ', 9), 'Slope of morphological traits on temperature',
                   rep(' ', 8),
                   'Slope of precipitation on years',
                   'Slope of phenological traits on precipitation',
                   rep(' ', 4)),

                   Effect = c('Intercept', 'Intercept',
                   'Type of phenological measure',
                   ' ', ' ', 'Intercept',
                   'Type of morphological measure', ' ', #' ',
                   rep(c('Intercept', 'Fitness component', ' ', ' ',
                   'Generation length'), 2),
                   rep('Intercept', 3), 'Fitness component', ' ', ' ',
                   'Generation length', 'Intercept', 'Intercept',
                   'Taxon', rep(' ', 5),
                   'Type of phenological measure', ' ', ' ', 'Intercept',
                   'Taxon', ' ', ' ', 'Type of morphological measure',
                   ' ', ' ', 'Thermoregulation', ' ',
                   'Intercept', 'Intercept', 'Taxon', rep(' ', 3)),

                   Modality = c(' ', ' ',
                   'Arrival', 'Breeding', 'Development',
                   ' ', 'BCI', 'Mass', #'Size',
                   rep(c(' ', 'Recruitment', 'Reproduction',
                   'Survival', ' '), 2), rep(' ', 3),
                   'Recruitment', 'Reproduction', 'Survival',
                   rep(' ', 3), 'Aves', 'Mammalia', 'Reptilia',
                   'Amphibia', 'Arachnida', 'Insecta',
                   'Arrival', 'Breeding', 'Development',
                   ' ', 'Aves', 'Mammalia', 'Reptilia',
                    'BCI', 'Mass', 'Size',
                    'Endotherms', 'Ecthoterms', ' ', ' ',
                    'Aves', 'Mammalia', 'Amphibia','Insecta'))

   data_EfSizes$Estimate <- get_stat_effsizes(list = model_list, stat = 'globslope')
   data_EfSizes$SE <- get_stat_effsizes(list = model_list, stat = 'globse')

   save_xlsx(table = data_EfSizes,
             table_basename = table_basename,
             typeTab = 'efSizes')
   }





#' Produces a table with LRT statistics and variation due to random effects
#' for the specified list of models, as in Supplementary Table S2
#'
#' @inheritParams parameters_definition
#'
#' @return Saves a table analogous to the Supplementary Table S2 to the specified location.
#' @export
tab_LRT_ST2 <- function(model_list, table_basename){
 all_LRT <- get_stat_effsizes(list = model_list, stat = 'LRT')
 colnames(all_LRT) <- c('LRT', 'df', 'p')

 for (i in 1:nrow(all_LRT)){
   if (all_LRT$p[i] < 0.0001){
     all_LRT$'p value'[i] <- '<0.0001'
   } else {
   all_LRT$'p value'[i] <- format(round(all_LRT$p[i], 4),
                           nsmall = 4, scientific = FALSE)}
 }
 all_LRT$p <- NULL

 data_LRT <- data.frame(Dataset = c('PRCS', rep(' ', 15), 'PRC', rep(' ', 10)),

                            Response = c('Slope of temperature on years',
                            'Slope of phenological traits on temperature', ' ',
                            'Slope of morphological traits on temperature', ' ',
                            'Mean selection across phenological traits (presumably induced by temperature)',
                             rep(' ', 2),
                            'Mean selection across morphological traits (presumably induced by temperature)',
                             rep(' ', 2),
                            'Slope of precipitation on years',
                            'Slope of phenological traits on precipitation',
                            'Mean selection across phenological traits (presumably induced by precipitation)',
                             rep(' ', 2),
                            'Slope of temperature on years',
                            'Slope of phenological traits on temperature',
                             rep(' ', 2),
                            'Slope of morphological traits on temperature',
                             rep(' ', 3),
                            'Slope of precipitation on years',
                            'Slope of phenological traits on precipitation',
                             rep(' ', 1)),

                             Effect = c('Intercept', 'Intercept',
                             'Type of phenological measure',
                             'Intercept', 'Type of morphological measure',
                             rep(c('Intercept', 'Fitness component',
                             'Generation length'), 2),
                             rep('Intercept', 3), 'Fitness component',
                             'Generation length',
                             'Intercept', 'Intercept', 'Taxon',
                             'Type of phenological measure',
                             'Intercept', 'Taxon',
                             'Type of morphological measure', 'Thermoregulation',
                             'Intercept', 'Intercept', 'Taxon'))
 data_LRT <- cbind(data_LRT, all_LRT)
 data_LRT$'Random variance due to study ID' <-
                  get_stat_effsizes(list = model_list, stat = 'RandStudy')
 data_LRT$'Random variance due to publication ID' <-
                  get_stat_effsizes(list = model_list, stat = 'RandPub')
 save_xlsx(table = data_LRT, table_basename = table_basename,
          typeTab = 'LRT')
}


#' Produces a table with summary statistics for the models conducted
#' for sensitivity analysis, as in Supplementary Table S4
#'
#' @inheritParams parameters_definition
#'
#' @return Saves a table analogous to the Supplementary Table S4 to the specified location.
#' @export
#'
tab_efSizes_ST4 <- function(model_list, table_basename){
data_EfSizes_sens <- data.frame(Excluding = c('The study by Goodenough et al. (2011), outlier',
                                rep(' ', 5),
                                'The only study on mammal (Plard et al. 2014)', rep(' ', 5),
                                'Both the only study on mammal (Plard et al. 2014) and the outlier study (Goodenough et al. 2011)',
                                 rep(' ', 5)),

                                Response = rep(c('Slope of phenological traits on temperature',
                               'Mean selection across phenological traits separately (presumably induced by temperature)',
                                rep(' ', 4)), 3),

                               Effect = rep(c('Intercept', 'Intercept', 'Fitness component',
                               ' ', ' ', 'Generation length'), 3),

                               Modality = rep(c(' ', ' ', 'Recruitment', 'Reproduction',
                              'Survival', ' '), 3))
 data_EfSizes_sens$Estimate <- get_stat_effsizes(list = model_list,
                                                 stat = 'globslope')
 data_EfSizes_sens$SE <- get_stat_effsizes(list = model_list,
                                           stat = 'globse')
 save_xlsx(table = data_EfSizes_sens,
           table_basename = table_basename,
           typeTab = 'efSizes' )
}


#' Produces a table with LRT statistics and variation due to random effects,
#' for the models conducted for sensitivity analysis, as in Supplementary Table S5
#'
#' @inheritParams parameters_definition
#'
#' @return Saves a table analogous to the Supplementary Table S5 to the specified location.
#' @export

tab_LRT_ST5 <- function(model_list, table_basename){
 all_LRT_sens <- get_stat_effsizes(list = model_list, stat = 'LRT')
 colnames(all_LRT_sens) <- c('LRT', 'df', 'p')
 for (i in 1:nrow(all_LRT_sens)){
   if (all_LRT_sens$p[i] < 0.0001){
     all_LRT_sens$'p value'[i] <- '<0.0001'
   } else {
   all_LRT_sens$'p value'[i] <- format(round(all_LRT_sens$p[i], 4),
                           nsmall = 4, scientific = FALSE)}
 }
 all_LRT_sens$p <- NULL

 data_LRT_sens <-
   data.frame(Excluding = c('The study by Goodenough et al. (2011), outlier',
   rep(' ', 3),
   'The only study on mammal (Plard et al. 2014)', rep(' ', 3),
   'Both the only study on mammal (Plard et al. 2014) and the outlier study (Goodenough et al. 2011)',
   rep(' ', 3)),

   Response = rep(c('Slope of phenological traits on temperature',
   'Mean selection across phenological traits separately (presumably induced by temperature)',
   rep(' ', 2)), 3),

   Effect = rep(c('Intercept', 'Intercept', 'Fitness component',
   'Generation length'), 3))
 data_LRT_sens <- cbind(data_LRT_sens, all_LRT_sens)
 data_LRT_sens$'Random variance due to study ID' <-
                          get_stat_effsizes(list = model_list, stat = 'RandStudy')
 data_LRT_sens$'Random variance due to publication ID' <-
                          get_stat_effsizes(list = model_list, stat = 'RandPub')
 save_xlsx(table = data_LRT_sens,
           table_basename = table_basename,
           typeTab = 'LRT')
}

#' Produces a table with the heterogeneity metrics for the specified list of models,
#' this table is analogous to the Supplementary Table S7.
#'
#' @inheritParams  parameters_definition
#'
#' @return Saves a table analogous to the Supplementary Table S7 to the specified location.
#' @export

tab_heterog <- function(model_list, table_basename){
 all_heterogen <- get_stat_effsizes(model_list, stat = 'heterogen')
 colnames(all_heterogen) <- c('I2', 'H2', 'Q', 'p')
 for (i in 1:nrow(all_heterogen)){
   if (all_heterogen$p[i] < 0.0001){
     all_heterogen$'p value'[i] <- '<0.0001'
   } else {
   all_heterogen$'p value'[i] <- format(round(all_heterogen$p[i], 4),
                           nsmall = 4, scientific = FALSE)}
  }
  all_heterogen$p <- NULL

 data_heterogen <- data.frame(Data = c('PRCS', rep(' ', 7), 'PRC', rep(' ', 4),
                             'Sensitivity analysis, \n PRCS dataset', rep(' ', 5)),

                              Response = c('Slope of temperature on years',
                              'Slope of phenological traits on temperature',
                              'Slope of morphological traits on temperature',
                              'Mean selection across phenological traits (presumably induced by temperature)',
                              'Mean selection across morphological traits (presumably induced by temperature)',
                              'Slope of precipitation on years',
                              'Slope of phenological traits on precipitation',
                              'Mean selection across phenological traits (presumably induced by precipitation)',
                              'Slope of temperature on years',
                              'Slope of phenological traits on temperature',
                              'Slope of morphological traits on temperature',
                              'Slope of precipitation on years',
                              'Slope of phenological traits on precipitation',
                              'Slope of phenological traits on temperature after removing an outlier (Goodenough et al. 2011)',
                              'Mean selection across phenological traits (presumably induced by temperature) after removing an outlier (Goodenough et al. 2011)',
                              'Slope of phenological traits on temperature after removing the only study on mammal (Plard et al. 2014)',
                              'Mean selection across phenological traits (presumably induced by temperature) after removing the only study on mammal (Plard et al. 2014)',
                              'Slope of phenological traits on temperature after removing both the study on mammal (Plard et al. 2014) and an outlier (Goodenough et al. 2011)',
                              'Mean selection across phenological traits (presumably induced by temperature) after removing both the study on mammal (Plard et al. 2014) and an outlier (Goodenough et al. 2011)'))
 data_heterogen <- cbind(data_heterogen, all_heterogen)

 save_xlsx(table = data_heterogen,
           table_basename = table_basename,
           typeTab = 'heterog')
}
