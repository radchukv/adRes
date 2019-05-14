#' Extract the effect sizes and their standard errors from the model fitted to a single study
#'
#' \code{extract_effects} retrieves the effect size and SE for a specified study,
#' for a given condition.
#'
#' @inheritParams parameters_definition
#' @return A dataframe with the id of the study, the estimated effect sizes and their SE,
#' the estimated autocorrelation parameter and the results of the LRT. The dataframe also
#' contains all the meta-data supplied via 'data' argument.
#'
#' @export
#'
#' @examples
#' dat_clim <- prepare_data(data = dat_Clim, temperature = TRUE,
#'                          precipitation = FALSE,
#'                          phenology = TRUE, morphology = TRUE)
#'
#' fitted_mod <- fit_cond_id(data = dat_clim, id = '30', condition = '1')
#' test <- extract_effects(fitted_mod)
#'
#'
#' dat_sel_phen <- prepare_data(data = dat_Sel, temperature = TRUE,
#'                              precipitation = FALSE,
#'                              phenology = TRUE, morphology = FALSE)
#'
#' test_sel_phen <- extract_effects(fit_cond_id(data = dat_sel_phen,
#'                                  id  = '1', condition = '3'))
#'
#' dat_phen_prcs <- prepare_data(data = dat_Trait, temperature = TRUE,
#'                               precipitation = FALSE,
#'                               phenology = TRUE, morphology = FALSE)
#' mod_phen_abund <- fit_cond_id(data = dat_phen_prcs, id  = '1', condition = '2b')
#' test_tr_phen_abund <- extract_effects(mod_phen_abund)
#'
extract_effects <- function(list_fit){

  study <- list_fit$model$data

  if (list_fit$condition == '1') {
    slopes <- spaMM::fixef(list_fit$model)[[grep('year', names(spaMM::fixef(list_fit$model)))]]
    se_slopes <- sqrt(stats::vcov(list_fit$model)[grep('year', colnames(stats::vcov(list_fit$model))),
                                                  grep('year', colnames(stats::vcov(list_fit$model)))])

    Npoints <- length(!is.na(study$Clim))
  }

  if (list_fit$condition == '2') {
    slopes <- spaMM::fixef(list_fit$model)[['Clim']]
    se_slopes <-  sqrt(stats::vcov(list_fit$model)[grep('Clim', colnames(stats::vcov(list_fit$model))),
                                                   grep('Clim', colnames(stats::vcov(list_fit$model)))])

    Npoints <- length(!is.na(study$ScaledTrait))
  }

  if (list_fit$condition == '2b') {
    slope_year <- spaMM::fixef(list_fit$model)[['year']]
    slope_clim <- spaMM::fixef(list_fit$model)[['Clim']]
    slope_abund <- spaMM::fixef(list_fit$model)[['ScaledPop']]

    se_slope_year <- sqrt(stats::vcov(list_fit$model)[grep('year', colnames(stats::vcov(list_fit$model))),
                                                      grep('year', colnames(stats::vcov(list_fit$model)))])
    se_slope_clim <- sqrt(stats::vcov(list_fit$model)[grep('Clim', colnames(stats::vcov(list_fit$model))),
                                                      grep('Clim', colnames(stats::vcov(list_fit$model)))])
    se_slope_abund <- sqrt(stats::vcov(list_fit$model)[grep('ScaledPop', colnames(stats::vcov(list_fit$model))),
                                                       grep('ScaledPop', colnames(stats::vcov(list_fit$model)))])

    Npoints <- length(!is.na(study$ScaledPop))
  }

  if (list_fit$condition == '3') {

    ranges <- range(study$year)
    newdata <- data.frame(id = unique(study$id), year = seq(from = ranges[1], to = ranges[2]))
    point_predictions <- spaMM::predict.HLfit(list_fit$model, newdata = newdata, binding = NA)
    predvar <- spaMM::get_predVar(list_fit$model, newdata = newdata)

    if (any(predvar < 0)) {
      warning('predicted variance is negative; wrong fit must have occurred!')
    }
    # weighing the prediction variance
    wi <- (1/predvar)/sum(1/predvar)
    slopes <- sum(wi*point_predictions)  ## weighted mean
    covpred <- spaMM::get_predVar(list_fit$model, newdata = newdata,
                                  variances = list(predVar = TRUE, cov = TRUE))
    se_slopes <- sqrt(as.numeric(t(wi) %*% covpred %*% wi)) ## weighted SE

    Npoints <- length(!is.na(study$Selection_mean))

  }

  if (list_fit$condition == '3b') {
    slopes <- spaMM::fixef(list_fit$model)[['year']]
    se_slopes <- sqrt(stats::vcov(list_fit$model)[grep('year', colnames(stats::vcov(list_fit$model))),
                                                  grep('year', colnames(stats::vcov(list_fit$model)))])

    Npoints <- length(!is.na(study$Selection_mean))
  }

  AR1cor <- unlist(spaMM::get_ranPars(list_fit$model, which = 'corrPars'))[[1]]

  if (is.null(AR1cor)) {
    AR1cor <- NA
  }

  min_phi <- min(spaMM::get_residVar(list_fit$model))

  lambda <- list_fit$model$lambda[[1]]
  if (is.null(lambda)) {
    lambda <- NA
  }

  dat <- data.frame(id = unique(study$id), Study_Authors = unique(study$Study_Authors),
                    Species = unique(study$Species),
                    Trait_Categ_det = unique(study$Trait_Categ_det),
                    Trait_Categ = unique(study$Trait_Categ),
                    Trait_Cat = unique(study$Trait_Cat),
                    Fitness_Categ = unique(study$Fitness_Categ),
                    Country_code = unique(study$Country_code),
                    PaperID = unique(study$PaperID),
                    Taxon = unique(study$Taxon), Location = unique(study$Location),
                    CH = unique(study$CH), CH_Categ = unique(study$CH_Categ),
                    Trait = unique(study$Trait), Morph_type = unique(study$Morph_type),
                    Blood = unique(study$Blood), Npoints = Npoints,
                    data = unique(study$data), AR1cor = AR1cor,
                    LRT.p = list_fit$anova$basicLRT$p_value,
                    LRT.Chi2 = list_fit$anova$basicLRT$chi2_LR,
                    min_phi = min_phi, lambda = lambda, condition = list_fit$condition)

  if (!list_fit$condition == '2b'){
    dat$slope <- slopes
    dat$SE_slope <- se_slopes[[1]]

    if (list_fit$data == 'PRCS'){
      dat$Coord_Lat_deg <- study$Coord_Lat_deg[1]
      dat$Coord_Lat_min <- study$Coord_Lat_min[1]
      dat$Coord <- dat$Coord_Lat_deg*60 + dat$Coord_Lat_min
      dat$GenerationLength_yr <- unique(study$GenerationLength_yr)
    }

  } else {
    dat$slope_year <- slope_year
    dat$slope_clim <- slope_clim
    dat$slope_abund <- slope_abund
    dat$se_slope_year <- se_slope_year[[1]]
    dat$se_slope_clim <- se_slope_clim[[1]]
    dat$se_slope_abund <- se_slope_abund[[1]]
    dat$Coord_Lat_deg <- study$Coord_Lat_deg[1]
    dat$Coord_Lat_min <- study$Coord_Lat_min[1]
    dat$Coord <- dat$Coord_Lat_deg*60 + dat$Coord_Lat_min
    dat$GenerationLength_yr <- unique(study$GenerationLength_yr)

  }
  return(dat)
}
