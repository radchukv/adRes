#' Produce a forest plot with effect sizes for each study id in PRCS dataset and global
#' effects for both PRCS and PRC datasets
#'
#' \code{plot_forest} Plots a forest plot displaying the effect sizes
#' for each study and global effects across the studies
#'
#' This function first fits separate mixed-effects models per study id. All the
#' models include predictors of interest. The possible types of labels are:
#' \itemize{
#' \item traits - the specific trait reported for each study;
#' \item fitness - the fitness measure used in each study;
#' \item country - a 2-letter country code;
#' \item authors - if there are more than two authors, the name of
#' the first one, otherwise the name of the first two.} If the label is set to
#' TRUE (default), it will be displayed on the plot.
#'
#' @inheritParams parameters_definition
#'
#' @export
#'
#' @return Plots a forest plot with effect sizes (and SEs) for each study,
#' and global effect size(s) on the bottom. The data used for the plot are
#' returned (invisibly).
#'
#' @examples
#' nb_cores <- 2L
#' meta_Trait_phen <- fit_all(data = dat_Trait, temperature = TRUE,
#'                            precipitation = FALSE, phenology = TRUE,
#'                            morphology = FALSE, condition = '2',
#'                            nb_cores = nb_cores, rand_trait = FALSE,
#'                            fixed = NULL, digit = 3)
#'
#' meta_Trait_morph <- fit_all(data = dat_Trait, temperature = TRUE,
#'                             precipitation = FALSE, phenology = FALSE,
#'                             morphology = TRUE, condition = '2',
#'                             nb_cores = nb_cores, rand_trait = FALSE,
#'                             fixed = NULL, digit = 3)
#'
#' meta_Trait_phen_Tax <- fit_all(data = dat_Trait, temperature = TRUE,
#'                                precipitation = FALSE, phenology = TRUE,
#'                                morphology = FALSE, condition = '2',
#'                                nb_cores = nb_cores, rand_trait = FALSE,
#'                                fixed = 'Taxon', digit = 3)
#'
#' test <- plot_forest(meta_obj1 = meta_Trait_phen$meta_res,
#'                     meta_obj2 = meta_Trait_morph$meta_res,
#'                     list_extra_meta_obj = list(meta_Trait_phen_Tax$meta_res),
#'                     sort = c("Species", "Study_Authors", "Fitness_Categ"),
#'                     increasing = TRUE,
#'                     labels = c(traits = TRUE,
#'                                fitness = TRUE,
#'                                country = TRUE,
#'                                authors = TRUE))
#'
plot_forest <- function(meta_obj1,
                        meta_obj2 = NULL,
                        list_extra_meta_obj = NULL,
                        sort = c("Species", "Study_Authors", "Fitness_Categ"),
                        increasing = TRUE,
                        labels = c(traits = TRUE,
                                   fitness = TRUE,
                                   country = TRUE,
                                   authors = TRUE),
                        mar = c(4, 10, 2, 2)) {

  ## prepare labels (note: labels are sorted by the function)
  labels_obj1 <- prepare_labels(meta_obj = meta_obj1,
                                labels = labels,
                                sort = sort,
                                increasing =  increasing)

  labels_obj2 <- prepare_labels(meta_obj = meta_obj2,
                                labels = labels,
                                sort = sort,
                                increasing = increasing)

  ## prepare the slopes and CI
  slopes_obj <- prepare_slopes(meta_obj1 = meta_obj1,
                               meta_obj2 = meta_obj2,
                               list_extra_meta_obj = list_extra_meta_obj,
                               labels_obj1 = labels_obj1,
                               labels_obj2 = labels_obj2)

  ## add the colours to the object containing slopes and CI
  slopes_obj <- prepare_colours(slopes_obj)

  ## prepare the coordinates of the things to plot (NOTE: retrieve cex and labels)
  data_plot <- prepare_data_plot(slopes_obj = slopes_obj)

  ## prepare the limits of the plot
  ylim_plot <- range(data_plot$y)
  xlim_max <-  ceiling(max(c(abs(data_plot$lwr), data_plot$upr), na.rm = TRUE))
  xlim_plot <- c(-xlim_max, xlim_max)

  ## prepare the x-axis label
  xlab_plot  <- prepare_xlabs(meta_obj = meta_obj1)
  xlab_plot2 <- prepare_xlabs(meta_obj = meta_obj2) ## just for checking

  if (!is.null(xlab_plot2) && xlab_plot != xlab_plot2) {
    stop('Conditions of meta_obj1 and meta_obj2 differ!')
  }

  ## start pdf if name of file defined
  # if (!is.null(pdf_basename)) {
  #   grDevices::pdf(file = paste0(pdf_basename, '.pdf'))
  # }

  graphics::par(mar = mar)
  ## drawing the empty plot
  graphics::plot(NULL,
                 xlim = xlim_plot,
                 ylim = ylim_plot,
                 ylab = '',
                 yaxt = 'n',
                 xlab = xlab_plot,
                 cex.axis = 1.2,
                 cex.lab = 1.3)

  ## adding middle lines
  graphics::abline(v = 0, lty = 3)
  graphics::abline(h = 0, lty = 1)

  ## adding data
  for (i in 1:nrow(data_plot)) {
    graphics::points(data_plot[i, 'y'] ~ data_plot[i, 'slope'],
                     pch = 4,
                     lwd = 1.4,
                     col = data_plot[i, 'colour'],
                     cex = data_plot[i, 'cex'])

    graphics::arrows(x0 = data_plot[i, 'lwr'], x1 = data_plot[i, 'upr'],
                     y0 = data_plot[i, 'y'],
                     y1 = data_plot[i, 'y'],
                     code = 3,
                     length = 0.01,
                     angle = 90,
                     lwd = 1.4,
                     col = data_plot[i, 'colour'])

    graphics::segments(x0 = min(xlim_plot) - 2,
                       x1 = data_plot[i, 'lwr'],
                       y0 = data_plot[i, 'y'],
                       y1 = data_plot[i, 'y'],
                       lty = 3,
                       col = data_plot[i, 'colour'])

    graphics::mtext(data_plot[i, 'label'], side = 2, line = 0.5, at = data_plot[i, 'y'],
                    las = 2, cex = 0.7, col = data_plot[i, 'colour'])
  }

  ## start pdf if name of file defined
  # if (!is.null(pdf_basename)) {
  #   grDevices::dev.off()
  #   message(paste0('a pdf named', paste0(pdf_basename, '.pdf'), ' has been created and saved!'))
  # }

  return(invisible(data_plot))
}


#' Produce a forest plot, which for each study shows the effect sizes for
#'  climatic factor, abundance and year
#'
#' @inheritParams parameters_definition
#'
#' @return Plots a forest plot that shows for each study the effect sizes
#' (and SEs) estimated for the climatic variable, abundance and year.
#' @export
#'

plot_abund_effects <- function(meta_data){
  ID_order <- rev(paste0("id", as.numeric(as.character(meta_data$id))[
    order(meta_data$Species, meta_data$Trait_Categ_det)]))
  ID_ord <- match(ID_order, paste0("id", meta_data$id))
  labls <- paste0(meta_data$Trait_Categ_det, ', ', meta_data$id, ', ', meta_data$Species)

  # if (!is.null(pdf_basename)) {
  #   grDevices::pdf(file = paste0(pdf_basename, '.pdf'))
  # }

  graphics::par(mar = c(4, 16, 2, 2))
  graphics::plot(meta_data$slope_clim[ID_ord], 1:length(meta_data$slope_clim),
       type = "o", pch = 19, col = "blue", ylab = '', yaxt = 'n',
       xlab = 'Effect sizes',
       xlim = c(min(c(range(meta_data$slope_clim), range(meta_data$slope_year),
                      range(meta_data$slope_abund))),
                max(c(range(meta_data$slope_clim), range(meta_data$slope_year),
                      range(meta_data$slope_abund)))))
  graphics::points(meta_data$slope_year[ID_ord],
         1:length(meta_data$slope_year), pch = 19, type = "o", col = "red")
  graphics::points(meta_data$slope_abund[ID_ord],
         1:length(meta_data$slope_abund), pch = 19, type = "o", col = "green")
  graphics::abline(v = 0, col = 'black', lwd = 1)
  graphics::mtext(labls[ID_ord],
        side = 2, line = 0.5, at = 1:length(meta_data$slope_clim), las = 2,
        cex = 0.8, col = 'black')
  graphics::legend(x = -1.3, y = 24, col = c('blue', 'red', 'green'), pch = 19,
         legend = c('Temperature', 'Year', 'Abundance'))
  # if (!is.null(pdf_basename)) {
  #   grDevices::dev.off()
  #   message(paste0('a pdf named', paste0(pdf_basename, '.pdf'), ' has been created and saved!'))
  # }
}
