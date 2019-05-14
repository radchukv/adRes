#' Plot data and fitted model per each study id
#'
#' \code{plot_per_id} Plots raw data and a fitted model for a single study id in the dataset.
#' This model is fitted with \code{\link{fit_cond_id}}. Plot also visualizes the
#' p value for the fit of the model.
#'
#' This function fits a single model per study id. This model includes
#' predictors of interest and accounts for temporal autocorrelation. Predictors
#' of interest can be: \itemize{ \item a year (if assessing condition 1, change in climatic
#' factor), \item a climatic factor and a year (if assessing condition 2, change in
#' the trait with climate).}
#'
#' @inheritParams parameters_definition
#'
#' @export
#' @return A plot showing the raw data points and a fitted line.
#'
#' @examples
#' plot_per_id(data = dat_Clim, temperature = TRUE,
#'             precipitation = FALSE, phenology = TRUE,
#'             morphology = TRUE, id = '1', condition = '1')
#'
#' plot_per_id(data = dat_Trait, temperature = TRUE,
#'             precipitation = FALSE, phenology = TRUE,
#'             morphology = FALSE, id = '1', condition = '2')
#'
plot_per_id <- function(data, temperature = FALSE,
                        precipitation = FALSE,
                        phenology = FALSE, morphology = FALSE,
                        id, condition){

  if (! condition %in%  c('1', '2')){
    stop('specified condition is incorrect; for condition = 3
         use function plot_sel_per_id()')
  }

  # define x and y depending on the condition
  if (condition == '1'){
    xvar <- 'year'; yvar <- 'Clim'; zvar <- NULL
  }
  if (condition == '2'){
    xvar <- 'Clim'; yvar <- 'ScaledTrait'; zvar <- 'year'
  }

  # fit the model - because we can't extract it from any previously fitted object
  dat_sub <- prepare_data(data = data, temperature = temperature,
                          precipitation = precipitation,
                          phenology = phenology,
                          morphology = morphology)
  fitted_mod <- fit_cond_id(data = dat_sub, id = id, condition = condition)
  efsize <- extract_effects(fitted_mod)

  # datasubset for this id
  sub <- stats::na.omit(dat_sub[dat_sub$id == id,
                                c('Study_Authors', 'Species',
                                  'Ref_Suppl', xvar, yvar, zvar)])


  pv <- paste("=", formatC(efsize$LRT.p, digits = 3, format = "f"))
  if (pv == "= 0.000") pv <- "< 0.001"

  # plotting
  graphics::plot(sub[, xvar], sub[, yvar], xlab = '', ylab = '', tcl = -0.1,
                 mgp = c(3, 0.3, 0), pch = 19, xlim = NULL, ylim = NULL)

  if (condition == '2') {
    dat_pred <- data.frame(id = rep(unique(id), each = 10),
                           Clim = seq(from = range(sub$Clim)[1],
                                      to = range(sub$Clim)[2], length.out = 10),
                           year = rep(round(mean(sub$year)), length = 10))

    dat_pred$Pred <- as.numeric(spaMM::predict.HLfit(fitted_mod$model,
                                                     newdata = dat_pred))

    # plot predictions
    graphics::lines(unlist(dat_pred[xvar]), unlist(dat_pred['Pred']),
                    col = 2, lwd = 2)
  } else {
    graphics::abline(coef = spaMM::fixef(fitted_mod$model),
                     col = 2, lwd = 2)
  }

  graphics::mtext(paste0(unique(sub$Study_Authors), ' [',
                         unique(sub$Ref_Suppl), ']'),
                  side = 3, line = 0.6, outer = FALSE, cex = 0.7)
  graphics::mtext(unique(sub$Species), side = 3, line = -0.1,
                  outer = FALSE, cex = 0.7, font = 3)
  graphics::mtext(paste("pv", pv), side = 3, line = -1, outer = FALSE, cex = 0.7, col = 2)
}
