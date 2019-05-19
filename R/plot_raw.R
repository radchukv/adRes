#' Plot raw data and the fitted model for the selected set of studies
#'
#' \code{plot_raw} Plots raw data and fitted models for a
#' specified set of studies.
#'
#' @inheritParams parameters_definition
#'
#' @export
#' @return A plot showing the raw data points and fitted models for a
#' specified subset of studies.
#'
#' @examples
#' \dontrun{
#' pdf('./test_sel.pdf', height = 10, width = 7)
#' par(oma = c(2, 2, 0, 0), mar = c(1, 2, 3, 1))
#' plot_raw(data = dat_Sel, temperature = TRUE,
#'          precipitation = FALSE, phenology = TRUE,
#'          morphology = FALSE, condition = '3',
#'          id_to_do = c(1:19))
#' dev.off()
#'
#' pdf('./test_traits.pdf', height = 10, width = 7)
#' par(oma = c(2, 2, 0, 0), mar = c(1, 2, 3, 1))
#' plot_raw(data = dat_Trait, temperature = TRUE,
#'          precipitation = FALSE, phenology = TRUE,
#'          morphology = FALSE, condition = '2',
#'          id_to_do = c(1:20))
#' dev.off()
#'}
#'
plot_raw <- function(data, temperature = FALSE,
                     precipitation = FALSE,
                     phenology = FALSE, morphology = FALSE,
                     condition, id_to_do) {

  dat_sub <- prepare_data(data = data, temperature = temperature,
                          precipitation = precipitation,
                          phenology = phenology,
                          morphology = morphology)

  if((length(id_to_do) %% 4) != 0){
    mat <- matrix(c(1:length(id_to_do),
                    rep(0, 4 - (length(id_to_do) %% 4))),
                  ncol = 4, byrow = TRUE)
  } else {
      mat <- matrix(1:length(id_to_do), ncol = 4, byrow = TRUE)
  }

  graphics::layout(mat, widths =  rep.int(1, ncol(mat)),
         heights = rep.int(1, nrow(mat)), respect = TRUE)

  ifelse(condition == '3',
         lapply(unique(dat_sub$id)[id_to_do], function(x) {
    plot_sel_per_id(data = dat_sub, temperature = temperature,
                    precipitation = precipitation,
                    phenology = phenology, morphology = morphology,
                    id = as.character(x), condition = condition)}),
    lapply(unique(dat_sub$id)[id_to_do], function(x) {
      plot_per_id(data = dat_sub, temperature = temperature,
                  precipitation = precipitation,
                  phenology = phenology, morphology = morphology,
                  id = as.character(x), condition = condition)}))

  if (condition == '1' & temperature == TRUE){
    xlab <- 'Year'
    ylab <- expression(Temperature~(degree*C))
  }

  if (condition == '2' & phenology == TRUE & morphology != TRUE){
    xlab <- expression(Temperature~(degree*C))
    ylab <- 'Phenological trait value (z-scaled)'

  }

  if (condition == '2' & morphology == TRUE & phenology != TRUE){
    xlab <- expression(Temperature~(degree*C))
    ylab <- 'Phenological trait value (z-scaled)'
  }

  if (condition == '2' & morphology == TRUE & phenology != TRUE){
    xlab <- expression(Temperature~(degree*C))
    ylab <- 'Morphological trait value (z-scaled))'
  }

  if (condition == '3'){
    xlab <- 'Year'
    ylab <- 'Selection differential'
  }

  #if (nrow(mat) <= 5) {
  #  xline <- -3} else {
      xline <- 1#}
  graphics::mtext(xlab, side = 1, line = xline, outer = TRUE)
  graphics::mtext(ylab, side = 2, line = 0, outer = TRUE, las = 0)
}

