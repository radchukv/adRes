#' Compute the Weighted Mean Selection (WMS) gradient
#'
#' \code{compute_WMS} computes the WMS gradient based on the fit of step 3 for a
#' given study. The WMS gradient is produced by predicting the gradient in each
#' year during the study period of ALL studies (accounting for the realization
#' of the yearly random effect). The predictions are then averaged accounting
#' for their uncertainty as measured by the prediction variance of the fit.
#'
#' @inheritParams parameters_definition
#'
#' @export
#'
#' @return A list with two elements: the Weighted Mean Selection (WMS)
#' gradient, and the weighted standard error associated with WMS (se).
#'
#' @note The weighted standard error is the square root of a prediction variance.
#' It represents the prediction uncertainty of a weighted average of yearly
#' selection gradients, taking into account the prediction covariance of yearly
#' selection gradients. The weights used for averaging are the inverse of the
#' yearly prediction variances, to minimize the variance of the average.
#'
#'
#'
#' @examples
#' data_for_Trait <- prepare_data(data = dat_Trait, temperature = TRUE,
#'                                phenology = TRUE, morphology = FALSE)
#' mod3 <- fit_cond_id(data = data_for_Trait, id = "82", condition = "3")
#' compute_WMS(mod_sel = mod3)
#'
compute_WMS <- function(mod_sel) {
  stopifnot(mod_sel$condition == "3")
  study <- mod_sel$model$data
  ranges <- range(study$year)
  newdata <- data.frame(id = unique(study$id),
                        year = seq(from = ranges[1], to = ranges[2]))
  point_predictions <- spaMM::predict.HLfit(mod_sel$model,
                                            newdata = newdata,
                                            binding = NA)
  predvar <- spaMM::get_predVar(mod_sel$model,
                                newdata = newdata)
  wi <- (1 / predvar) / sum(1 / predvar)
  WMS <- sum(wi * point_predictions)
  covpred <- spaMM::get_predVar(mod_sel$model,
                                newdata = newdata,
                                variances = list(predVar = TRUE, cov = TRUE))
  se_WMS <- sqrt(as.numeric(t(wi) %*% covpred %*% wi))
  return(list(WMS = WMS, se = se_WMS))
}



#' Compute the necessary information for testing adaptation
#'
#' \code{compute_adaptation} computes all necessary information, for all
#' studies, to compare the Weighted Mean Selection (WMS) gradient to the product
#' of the step 1 and 2.
#'
#' @inheritParams parameters_definition
#'
#' @return A \var{data.frame}.
#' @export
#'
#' @examples
#' data_for_Trait <- prepare_data(data = dat_Trait, temperature = TRUE,
#'                                phenology = TRUE, morphology = FALSE)
#' compute_adaptation(data = data_for_Trait[data_for_Trait$id %in% c("7"), ])
#'
compute_adaptation <- function(data) {
  data <- droplevels(data)
  ## internal function computing the product of step 1 and 2, and its uncertainty
  compute_prod12 <- function(id) {
    utils::capture.output(mod_temp <- fit_cond_id(data = data, id = id, condition = "1"))
    utils::capture.output(mod_trait <- fit_cond_id(data = data, id = id, condition = "2"))
    slope_year <- spaMM::fixef(mod_temp$model)[["year"]]
    slope_clim <- spaMM::fixef(mod_trait$model)[["Clim"]]
    slope <- slope_year * slope_clim
    var_year <- (stats::vcov(mod_temp$model)[2, "year"])
    var_clim <- stats::vcov(mod_trait$model)[2, "Clim"]
    var_prod <- slope_year^2 * var_clim + slope_clim^2 * var_year
    resu <- list(slope = slope, se_ind_slope = sqrt(var_prod))
    return(resu)
  }

  res <- list()
  for (level in levels(data$id)) {
    cat("Extracting info from study", level, "...", "\n")
    prod_info <- compute_prod12(level)
    utils::capture.output(mod_sel <- try(fit_cond_id(data = data, id = level, condition = "3")))
    if (inherits(mod_sel, "try-error")) {
      res[[level]] <- NULL
    } else {
      res[[level]] <- cbind(
        extract_effects(mod_sel),
        prod12 = prod_info$slope, se_prod12 = prod_info$se_ind_slope[[1]],
        WMS = compute_WMS(mod_sel)$WMS,
        se_WMS = compute_WMS(mod_sel)$se
      )
    }
  }
  cat("Done!\n")
  out <- as.data.frame(do.call(rbind, res))
  out$id <- factor(rownames(out))
  return(out)
}

