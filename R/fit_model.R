#' Fit the model for a given study id
#'
#' \code{fit_model} fits the model for a given study id. The function first fits
#' the model using REML with and without the AR1 auto-regressive term. It then
#' compares the AIC of these two models. The best model is then refitted using ML
#' and compared to a null model. All models are fitted using the package
#' \pkg{\link[spaMM]{spaMM}}.
#'
#' @inheritParams parameters_definition
#'
#' @return A list containing the best full fitted model and the result of
#'   \code{\link[spaMM]{anova.HLfit}} comparing this model and a null model.
#' @export
#'
#' @examples
#' dat_trials <- prepare_data(data = dat_Clim,
#'                            temperature = TRUE, precipitation = FALSE,
#'                            phenology = TRUE, morphology = TRUE)
#'
#' test <- fit_model(data = dat_trials,
#'                   formula_full = 'Clim ~ year',
#'                   formula_null = 'Clim ~ 1')
#'
fit_model <- function(data,
                      formula_full  = '~ 1',
                      formula_null  = '~ 1',
                      resid_formula = '~ 1',
                      prior_weights = NULL,
                      ...) {

  if (is.null(prior_weights)) {
    data$prior_weights <- rep(1, nrow(data))
  } else {
    data$prior_weights <- prior_weights
  }

  mod_full_fix <- spaMM::fitme(stats::as.formula(formula_full),
                               prior.weights = prior_weights,
                               resid.model = list(formula = stats::as.formula(resid_formula)),
                               data = data,
                               method = "REML", ...)

  formula_full_AR <- paste(formula_full, '+ AR1(1|year)')

  mod_full_AR <- spaMM::fitme(stats::as.formula(formula_full_AR),
                              prior.weights = prior_weights,
                              resid.model = list(formula = stats::as.formula(resid_formula)),
                              data = data,
                              method = "REML", ...)

  fix_best <- spaMM::AIC.HLfit(mod_full_fix)[[1]] < spaMM::AIC.HLfit(mod_full_AR)[[1]]

  out <- list()

  if (!fix_best) {
    formula_full <- paste(formula_full, '+ AR1(1|year)')
    formula_null <- paste(formula_null, '+ AR1(1|year)')
    out$model <- mod_full_AR
  } else {
    out$model <- mod_full_fix
  }

  mod_full <- spaMM::fitme(stats::as.formula(formula_full),
                           prior.weights = prior_weights,
                           resid.model = list(formula = stats::as.formula(resid_formula)),
                           data = data,
                           method = "ML", ...)

  mod_null <- spaMM::fitme(stats::as.formula(formula_null),
                           prior.weights = prior_weights,
                           resid.model = list(formula = stats::as.formula(resid_formula)),
                           data = data,
                           method = "ML", ...)

  out$anova <- spaMM::anova.HLfit(mod_full, mod_null)

  return(invisible(out))
}


#' Fit a single mixed-effects model to test a given condition for a given study id
#'
#' \code{fit_cond_id} fits a mixed-effects model for a single study id in the
#' dataset. This model takes into account temporal autocorrelation and includes
#' the desired fixed-effects predictors.
#'
#' @inheritParams parameters_definition
#'
#' @return The list of length 5. The first element is the best full
#' fitted model returned with the function \code{\link{fit_model}}.
#' The second element contains results of LRT. The third element
#' is a condition tested, the forth element is a study id, and the
#' fifth element is a dataset to what the study belongs (PRCS or PRC).
#' @export
#'
#' @seealso \code{\link{fit_model}}
#' @examples
#'
#' dat_trials <- prepare_data(data = dat_Clim,
#'                            temperature = TRUE, precipitation = FALSE,
#'                            phenology = TRUE, morphology = TRUE)
#' test <- fit_cond_id(data = dat_trials, id = '1', condition = '1')
#'
#' dat_temp_phen <- prepare_data(data = dat_Trait,
#'                            temperature = TRUE, precipitation = FALSE,
#'                            phenology = TRUE, morphology = FALSE)
#' test_trait <- fit_cond_id(data = dat_temp_phen, id = '1', condition = '2')
#' test_trait$model
#'
#'
fit_cond_id <- function(data, id, condition){
  study <- droplevels(data[data$id == id, ])

  if (condition == '1') {
    list_out <- fit_model(data = study,
                          formula_full = 'Clim ~ year',
                          formula_null = 'Clim ~ 1')

  } else if (condition == '2') {
    list_out <- fit_model(data = study,
                          formula_full = 'ScaledTrait ~ Clim + year',
                          formula_null = 'ScaledTrait ~ year',
                          prior_weights = 1/study$ScaledSE^2)

  } else if (condition == '2b') {
    list_out <- fit_model(data = study,
                          formula_full = 'ScaledTrait ~ Clim + year + ScaledPop',
                          formula_null = 'ScaledTrait ~ year + ScaledPop',
                          prior_weights = 1/study$ScaledSE^2,
                          resid_formula = '~ ScaledPop')

  } else if (condition  == '3') {
    list_out <- fit_model(data = study,
                          formula_full = 'Selection_mean ~ 1',
                          formula_null = 'Selection_mean ~ 0',
                          prior_weights = 1/study$Selection_SE^2)

  } else if (condition  == '3b') {
    list_out <- fit_model(data = study,
                          formula_full = 'Selection_mean ~ year',
                          formula_null = 'Selection_mean ~ 1',
                          prior_weights = 1/study$Selection_SE^2)
  } else stop('The condition you used is unknown!')

  list_out$condition <- condition
  list_out$id <- id

  if (length(unique(data$data)) == 2) {
    list_out$data <- 'PRC'} else {
      list_out$data <- 'PRCS'}

  return(list_out)
}
