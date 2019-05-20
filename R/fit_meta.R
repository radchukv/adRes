#' Fit a meta-analytical model to extract global effect sizes across studies
#'
#' \code{fit_meta} fits mixed-effects meta-analytical model to extract a global
#' effect size across the studies
#'
#' This function fits a mixed-effects meta-analytical model with
#' appropriate residual model structure to assess the significance of an overall
#' global effect size. Can be fit both with and without fixed effects. If fit
#' without any fixed effects, then the overall effect size across the studies is
#' obtained.
#'
#' @inheritParams parameters_definition
#' @export
#'
#' @examples
#' \dontrun{
#' nb_cores <- 2L ## increase the number for using more cores
#' dat_sel_phen <- prepare_data(data = dat_Sel,
#'                              temperature = TRUE, precipitation = FALSE,
#'                              phenology = TRUE, morphology = FALSE)
#' test_sel <- extract_effects_all_ids(data = dat_sel_phen,
#'                                     condition = '3', nb_cores = nb_cores)
#' test_meta_sel <- fit_meta(test_sel)
#' test_meta_sel_Fitn <- fit_meta(test_sel, fixed = 'Fitness_Categ')
#'
#'
#' dat_T_phen <- prepare_data(data = dat_Trait,
#'                            temperature = TRUE, precipitation = FALSE,
#'                            phenology = TRUE, morphology = FALSE)
#' test_T_phen <- extract_effects_all_ids(data = dat_T_phen,
#'                                       condition = '2', nb_cores = nb_cores)
#' test_meta_T_phen <- fit_meta(test_T_phen)
#' test_meta_T_phen_Taxon <- fit_meta(test_T_phen, fixed = 'Taxon')
#' }
#'

#'
#' @return A list of length eight, containing the results of
#' the meta-analysis: a global slope, its SE, a full model
#' fitted with REML, a null model fitted with ML,
#' a global model fitted with ML, results of LRT comparing the model
#' with the effect vs. the reduced one, the data used for the meta-analysis,
#' and heterogeneity metrics.
#'
fit_meta <- function(meta_data, rand_trait = FALSE,
                     fixed = NULL, digit = 3) {

  spaMM::spaMM.options(sparse_precision = FALSE)
  cat('\n ######################################################### \n',
      paste("Analysis of ", substitute(meta_data)),
      '\n #########################################################\n')

  # build formula
  ifelse(!is.null(fixed),
         formula <- paste0("slope ~ -1 + ", fixed, " + (1|id) + (1|Study_Authors)"),
         formula <- "slope ~ 1 + (1|id) + (1|Study_Authors)")
  if (rand_trait)   formula <- paste(formula, "+ (1|Trait_Categ_det)")

  # a work around the NaN in the Var...
  if (any(is.na(meta_data$SE_slope))) {
    warning("The object meta_data contains NA or NaN in predictors or response variable.
            Corresponding rows have been discarded")
    meta_data <- meta_data[!is.na(meta_data$SE_slope), ]
  }

  # global fit
  global_fit_REML <- spaMM::fitme(stats::as.formula(formula),
                                  fixed = list(phi = meta_data$SE_slope^2),
                                  data = meta_data,
                                  method = 'REML')


  # global slope +/- SE
  global_slope <- spaMM::fixef(global_fit_REML)
  names(global_slope) <- unlist(lapply(strsplit(names(global_slope), fixed), function(x) x[2]))

  ifelse(!is.null(fixed),
         global_se <- as.numeric(sqrt(diag(stats::vcov(global_fit_REML)))),
         global_se <- as.numeric(sqrt(stats::vcov(global_fit_REML)))) ## Not sure this is needed as diag should work here too (Alex)


  cat('\n########### Global slope ###########\n')
  print(paste0("slope = ", signif(global_slope, digit), "; se = ", signif(global_se, digit)))


  # build formula for ML -  some repetition, to have one estimate per fixed effect with REML and yet
  # to be able to run anova() on the models with fixed effects
  ifelse(!is.null(fixed),
         ML_formula <- paste0("slope ~", fixed, " + (1|id) + (1|Study_Authors)"),
         ML_formula <- "slope ~ 1 + (1|id) + (1|Study_Authors)")
  if (rand_trait) ML_formula   <- paste(ML_formula, "+ (1|Trait_Categ_det)")

  global_fit_ML <- spaMM::fitme(stats::as.formula(ML_formula),
                                fixed = list(phi = meta_data$SE_slope^2),
                                data = meta_data,
                                method = 'ML')

  ifelse(!is.null(fixed),
         nullformula <- "slope ~ 1 + (1|id) + (1|Study_Authors)",
         nullformula <- "slope ~ 0 + (1|id) + (1|Study_Authors)")
  if (rand_trait) nullformula   <- paste(nullformula, "+ (1|Trait_Categ_det)")

  global_nullfit_ML <- spaMM::fitme(stats::as.formula(nullformula),
                                    fixed = list(phi = meta_data$SE_slope^2),
                                    data = meta_data,
                                    method = 'ML')

  cat('\n########### Global ANOVA ###########\n')
  if (!is.null(fixed)) {
      cat(paste("Fixed effect of ", fixed), '\n')
  }

  anova_global <- spaMM::LRT(global_nullfit_ML, global_fit_ML)
  print(anova_global)

  global_simplefit_REML <- spaMM::fitme(slope ~ 1 + (1|id),
                                        fixed = list(phi = meta_data$SE_slope^2),
                                        data = meta_data,
                                        method = 'REML')

  #cat('\n########### Meta-analysis ###########\n')
  heterogen <- get_heterogen(model = global_simplefit_REML, digit = digit)

  return(list(global_slope = global_slope, global_se = global_se,
              model = global_fit_REML, model_null_ML = global_nullfit_ML,
              model_ML = global_fit_ML, anova_global = anova_global,
              meta_data = meta_data, heterogen = heterogen))
}
