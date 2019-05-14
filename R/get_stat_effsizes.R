#' Extracts statistics of interest or effect sizes for a specified list of random-effects meta-analytical models
#'
#' @inheritParams parameters_definition
#'
#' @return A dataframe with three columns: 'chi2_LR', 'df' and 'p_value' if the specified statistics is
#' 'LRT' and a numeric vector of extracted random variances or effect sizes or standard errors otherwise.
#' @export
#'
#' @examples
#' nb_cores <- min(c(parallel::detectCores(), 10))
#' mod_T_prcs <- fit_all(data = dat_Clim,
#'                       temperature = TRUE, precipitation = FALSE,
#'                       phenology = TRUE, morphology = TRUE,
#'                       condition = '1', nb_cores = nb_cores,
#'                       rand_trait = FALSE, fixed = NULL, digit = 3)
#' mod_Sel_T_phen <- fit_all(data = dat_Sel,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = TRUE, morphology = FALSE,
#'                           condition = '3', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL, digit = 3)
#' mod_list <- list(mod_T_prcs, mod_Sel_T_phen)
#'
#' test_LRT <- get_stat_effsizes(list = mod_list, stat = 'LRT')
#' test_LRT1 <- get_stat_effsizes(list = mod_list, stat = 'RandPub')
#'
get_stat_effsizes <- function(list, stat){
  if (stat == 'LRT'){
    dat <- do.call('rbind', lapply(list, FUN = function(x){x$meta_res$anova_global$basicLRT}))
    rownames(dat) <- 1:nrow(dat)
    return(dat)
  }
  if (stat == 'RandStudy'){
    return(as.numeric(do.call('rbind', lapply(list,
                                              FUN = function(x){x$meta_res$model$lambda['id']}))))
  }
  if (stat == 'RandPub'){
    return(as.numeric(do.call('rbind', lapply(list,
                                              FUN = function(x){x$meta_res$model$lambda['Study_Aut.']}))))
  }
  if (stat == 'globslope'){
    return(unlist(lapply(list, FUN = function(x){x$meta_res$global_slope})))
  }
  if (stat == 'globse'){
    return(unlist(lapply(list, FUN = function(x){x$meta_res$global_se})))
  }
  if (stat == 'heterogen'){
    return(as.data.frame(do.call('rbind', lapply(list, FUN =
                                                   function(x){x$meta_res$heterogen[c('I2', 'H2', 'Q', 'Qp')]})))
      )}
}
