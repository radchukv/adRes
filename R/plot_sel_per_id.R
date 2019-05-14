#' Plot selection data and WMS per each study id
#'
#' \code{plot_sel_per_id} Plots raw selection data and an estimate of WMS for a single study id in the dataset.
#' Plot also visualizes the p value for the fit of the model.
#'
#' @inheritParams parameters_definition
#'
#' @export
#' @return A plot showing the raw selection data and an estimate of WMS.
#'
#' @examples
#' plot_sel_per_id(data = dat_Sel, temperature = TRUE,
#'                 precipitation = FALSE, phenology = TRUE,
#'                 morphology = FALSE, id = '1', condition = '3')
#'
plot_sel_per_id <- function(data, temperature = FALSE,
                            precipitation = FALSE,
                            phenology = FALSE,
                            morphology = FALSE,
                            id, condition){

  if ( condition !=  '3'){
    stop('specified condition is incorrect; for conditions other than 3
         use function plot_per_id()')
  }

  dat_sub <- prepare_data(data = data, temperature = temperature,
                          precipitation = precipitation,
                          phenology = phenology,
                          morphology = morphology)
  fitted_mod <- fit_cond_id(data = dat_sub, id = id, condition = condition)
  efsize <- extract_effects(fitted_mod)

  sub <- dat_sub[dat_sub$id == id, ]

  sub$lowCI <- sub$Selection_mean + stats::qnorm(0.025) * sub$Selection_SE
  sub$highCI <- sub$Selection_mean + stats::qnorm(0.975) * sub$Selection_SE

  graphics::plot(Selection_mean ~ year, data = sub,
                 type = 'p', pch = 19, xlab = '', ylab = '',
                 ylim = c(min(sub$lowCI), max(sub$highCI)))
  graphics::arrows(x0 = sub$year, x1 = sub$year,
                   y0 = sub$lowCI, y1 = sub$highCI,
                   code = 3, length = 0.03, angle = 90, col = 'black', lwd = 1)
  graphics::abline(a = efsize$slope, b = 0, col = 'red', lwd = 2)

  # rectangles
  graphics::rect(xleft = min(sub$year) - 2,
                 ybottom = efsize$slope + stats::qnorm(0.025) * efsize$SE_slope,
                 xright = max(sub$year) + 2,
                 ytop = efsize$slope + stats::qnorm(0.975) * efsize$SE_slope,
                 col = grDevices::rgb(255, 99, 71, 125, maxColorValue = 255),
                 border = grDevices::rgb(255, 99, 71, 125, maxColorValue = 255))
  graphics::mtext(paste0(unique(sub$Study_Authors), ' [',
                         unique(sub$Ref_Suppl), ']'), side = 3,
                  line = 0.6, outer = FALSE, cex = 0.7)
  graphics::mtext(unique(sub$Species), side = 3, line = -0.1,
                  font = 3, outer = FALSE, cex = 0.7)

}
