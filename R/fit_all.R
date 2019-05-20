#' Get global effect size and its standard error as well as effect sizes
#' for all studies for a particular condition
#'
#' \code{fit_all} Subselects the data according to the specified requirements,
#' extracts the effect sizes per each study for a specied condition and fits a
#' meta-analytical model to calculate the global effect size and its standard error
#'
#' The data can be subselected per climatic variable and per trait category, as detailed in
#' \code{\link{prepare_data}}. Next, a condition tested should be specified as described in
#' \code{\link{extract_effects_all_ids}}. And finally, a meta-analytical model may
#' be fitted with an intercept only to assess the global effect size across the studies
#' or with a specified fixed effect.
#'
#' @inheritParams parameters_definition
#'
#' @export
#' @examples
#' nb_cores <- 2L ## increase the number for using more cores
#' meta_Sel_phen <- fit_all(data = dat_Sel, temperature = TRUE,
#'                          precipitation = FALSE,
#'                          phenology = TRUE, morphology = FALSE,
#'                          condition = '3', nb_cores = nb_cores,
#'                          rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_morph <- fit_all(data = dat_Sel,
#'                          temperature = TRUE, precipitation = FALSE,
#'                          phenology = FALSE, morphology = TRUE,
#'                          condition = '3', nb_cores = nb_cores,
#'                          rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_phen_Fitn <- fit_all(data = dat_Sel,
#'                               temperature = TRUE, precipitation = FALSE,
#'                               phenology = TRUE, morphology = FALSE,
#'                               condition = '3', nb_cores = nb_cores,
#'                               rand_trait = FALSE,
#'                               fixed = 'Fitness_Categ', digit = 3)
#'
#' @return A list with three elements. The first element is the data that was
#' subset according to the specified requirements. The second element is
#' a dataset of extracted effects sizes per study and their standard errors,
#' together with relevant meta-data per study. The third element is the list
#' of length eight, containing the results of the meta-analysis: a global slope,
#' its SE, a full model fitted with REML, a null model fitted with ML,
#' a global model fitted with ML, results of LRT comparing the model
#' with the effect vs. the reduced one, the data used for the meta-analysis,
#' and heterogeneity metrics.
#'
fit_all <- function(data, temperature = FALSE, precipitation = FALSE,
                     phenology = FALSE, morphology = FALSE,
                     condition, nb_cores, rand_trait = FALSE,
                     fixed = NULL, digit = 3){

  data_sub <- prepare_data(data = data, temperature = temperature,
                           precipitation = precipitation, phenology = phenology,
                           morphology = morphology)

  ef_sizes <- extract_effects_all_ids(data = data_sub, condition = condition, nb_cores = nb_cores)

  meta_res <- fit_meta(ef_sizes, fixed = fixed,
                       rand_trait = rand_trait, digit = digit)

  return(list(data = data_sub,
              ef_sizes = ef_sizes,
              meta_res = meta_res))

  }
