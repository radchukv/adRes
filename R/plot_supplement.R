#' Plots a funnel plot for the inspection of the potential publication bias
#'
#' @inheritParams parameters_definition
#' @return Plots a funnel plot to assess the potential publication bias.
#' @export
#'
#' @examples
#' nb_cores <- 2L
#' mod_T_prcs <- fit_all(data = dat_Clim,
#'                       temperature = TRUE, precipitation = FALSE,
#'                       phenology = TRUE, morphology = TRUE,
#'                       condition = '1', nb_cores = nb_cores,
#'                       rand_trait = FALSE, fixed = NULL, digit = 3)
#' plot_funnel(meta_obj = mod_T_prcs)
#'
#' mod_phen_T_prcs <- fit_all(data = dat_Trait,
#'                            temperature = TRUE, precipitation = FALSE,
#'                            phenology = TRUE, morphology = FALSE,
#'                            condition = '2', nb_cores = nb_cores,
#'                            rand_trait = FALSE, fixed = NULL, digit = 3)
#' plot_funnel(meta_obj = mod_phen_T_prcs)
#'
plot_funnel <- function(meta_obj, model = 'rma'){
  meta_data <- meta_obj$ef_sizes
  glob <- meta_obj$meta_res$global_slope
  test_res <- metafor::regtest(x = meta_data$slope, vi = meta_data$SE_slope^2,
                               ret.fit = T, model =  model)


  ## specifying the main depending on type of the data
  if (length(unique(meta_data$Trait_Categ)) == 1){
    main = unique(meta_data$Trait_Categ)
    } else {
    main = ''}

  ## specifying the ylab depending on the condition
  if (meta_data$condition[1] == '1'){
    ylab = 'Effect of year on temperature'}
  if (meta_data$condition[1] == '2'){
    ylab = 'Effect of temperature on trait'}
  if (meta_data$condition[1] == '3'){
    ylab = 'Selection on trait'}

  graphics::plot(meta_data$slope ~ meta_data$Npoints, pch = 19, xlab = 'Sample size',
                 ylab = ylab, cex.lab = 1.5, xlim = c(0, max(meta_data$Npoints)))
  graphics::title(main, line = 2, cex.main = 1.5, font.main = 1)
  graphics::abline(h = 0, lty = 1, lwd = 2, col = 'darkgrey')
  graphics::abline(h = glob, lty = 2, lwd = 2, col = 'darkgrey')
  graphics::mtext(paste0('z value = ', round(test_res$zval, 3)),
                  side = 3, line = 0.7, adj = 1)
  if(round(test_res$pval, 3) > 0.001) {
    graphics::mtext(paste0('p value = ', round(test_res$pval, 3)),
                    side = 3, line = -0.1, adj = 1)
  } else {
    graphics::mtext(paste0('p value < 0.0001'),
                     side = 3, line = -0.1, adj = 1)
    }
}

#' Plots slopes of temperature vs. the duration of the studies
#' and vs. the first year in time series
#'
#' @inheritParams parameters_definition
#' @return Plots three plots: 1. the effect of year on temperature vs
#' the number of years in the time series; 2. the effect of year on
#' temperature vs the first year in the time series; and 3. the first
#' year in the time series vs the number of years.
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'  \dontrun{
#'      mod_T_prc <- fit_all(data = dat_Clim_prc,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = TRUE, morphology = TRUE,
#'                           condition = '1', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL, digit = digit)
#'      plot_eff_dur_firstY(data = dat_Clim_prc, meta_obj = mod_T_prc)
#'  }
plot_eff_dur_firstY <- function(data, meta_obj){
  dat_T <- prepare_data(data, temperature = TRUE,
                        precipitation = FALSE,
                        phenology = TRUE,
                        morphology = TRUE)


  ## getting the first years of the studies
  firstY_T <- dat_T %>%
    dplyr::group_by(., id) %>%
    dplyr::slice(1) %>%
    dplyr::rename(FirstYear = year) %>%
    dplyr::select(., FirstYear, id)


  T_firstY <- merge(meta_obj$ef_sizes, firstY_T, by = 'id')

  # models
  mod_FYear_T <- stats::lm(slope ~ FirstYear, data = T_firstY)
  summary(mod_FYear_T)

  mod_Dur_T <- stats::lm(slope ~ Npoints, data = T_firstY)
  summary(mod_Dur_T)

  corYearDur <- stats::cor.test(T_firstY$Npoints, T_firstY$FirstYear)

  plot_stat_mod <- function(mod){
    if(stats::anova(mod)$'Pr(>F)'[1] < 0.0001){
      graphics::mtext('p < 0.0001', side = 3, adj = 1, line = -1)
    } else {
      graphics::mtext(paste0('p = ', stats::anova(mod)$'Pr(>F)'[1]),
                      side = 3, adj = 1, line = -1)
    }
    graphics::mtext(paste0('slope = ', round(mod$coefficients[2], 3)),
                    side = 3, adj = 1, line = -2)
  }

  ## plot of effects vs. duration vs. first year
  # if (!is.null(pdf_basename)) {
  #   grDevices::pdf(file = paste0(pdf_basename, '.pdf'), width = 13)
  # }

  graphics::par(mfrow = c(1,3), mar = c(5, 6, 2, 2))

  graphics::plot(slope ~ Npoints, data = T_firstY,
                 xlab = 'Number of years',
                 ylab = bquote('Effect of year on temperature (' *degree*C~'/ year)'),
                 pch = 19, cex.lab = 1.5, cex.axis = 1.3)
  graphics::mtext('(rate of warming)', side = 2, line = 2)
  graphics::abline(mod_Dur_T, col = 'red', lwd = 2)
  graphics::mtext('a)', side = 3, adj = 0, cex = 1.4)
  plot_stat_mod(mod = mod_Dur_T)

  graphics::plot(slope ~ FirstYear, data = T_firstY,
                 xlab = 'First year in time series',
                 ylab = bquote('Effect of year on temperature (' *degree*C~'/ year)'),
                 pch = 19, cex.lab = 1.5, cex.axis = 1.3)
  graphics::mtext('(rate of warming)', side = 2, line = 2)
  graphics::abline(mod_FYear_T, col = 'red', lwd = 2)
  graphics::mtext('b)', side = 3, adj = 0, cex = 1.4)
  plot_stat_mod(mod = mod_FYear_T)


  graphics::plot(FirstYear ~ Npoints, data = T_firstY,
                 xlab = 'Number of years',
                 ylab = 'First year in time series',
                 pch = 19, cex.lab = 1.5, cex.axis = 1.3)
  graphics::mtext('c)', side = 3, adj = 0, cex = 1.4)
  if(corYearDur$p.value < 0.0001){
    graphics::mtext('p < 0.0001', side = 3, adj = 1, line = -1)
  }else{
    graphics::mtext(paste0('p = ', corYearDur$p.value),
          side = 3, adj = 1, line = -1)}
  graphics::mtext(paste0('Pearson r = ', round(corYearDur$estimate, 3)),
        side = 3, adj = 1, line = -2)

  # if (!is.null(pdf_basename)) {
  #   grDevices::dev.off()
  #   message(paste0('a pdf named', paste0(pdf_basename, '.pdf'),
  #                  ' has been created and saved!'))
  # }
}
utils::globalVariables(names = c('.', 'year', 'FirstYear', 'id'))


#
#' Merges all three slopes by the respective data attributes
#'
#' @inheritParams parameters_definition
#'
#' @return A dataframe with the products of slopes from conditions 1 and 2 and
#' slopes from condition 3.
#'
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' mod_T_prcs <- fit_all(data = dat_Clim,
#'                       temperature = TRUE, precipitation = FALSE,
#'                       phenology = TRUE, morphology = TRUE,
#'                       condition = '1', nb_cores = nb_cores,
#'                       rand_trait = FALSE, fixed = NULL, digit = digit)
#' mod_phen_T_prcs <- fit_all(data = dat_Trait, temperature = TRUE,
#'                             precipitation = FALSE, phenology = TRUE,
#'                             morphology = FALSE, condition = '2',
#'                             nb_cores = nb_cores, rand_trait = FALSE,
#'                             fixed = NULL, digit = digit)
#' mod_Sel_T_phen <- fit_all(data = dat_Sel,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = TRUE, morphology = FALSE,
#'                           condition = '3', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL,
#'                           digit = digit)
#' test <- merge_3slopes(meta_obj = mod_T_prcs,
#'                       meta_obj1 = mod_phen_T_prcs,
#'                       meta_obj2 = mod_Sel_T_phen)
#' }
#'
merge_3slopes <- function(meta_obj, meta_obj1, meta_obj2){
  Trait_1_2 <- merge(meta_obj$ef_sizes, meta_obj1$ef_sizes,
                     by= c('Study_Authors', 'Species', 'Location', 'CH'))

  Trait_12 <- Trait_1_2 %>%
    dplyr::select(., -c(Trait_Categ_det.y, Fitness_Categ.y, Country_code.y, Taxon.y, Trait.y)) %>%
    dplyr::rename(., Trait =  Trait.x)
  Trait_12_3 <- merge(Trait_12, meta_obj2$ef_sizes,
                      by = c('Study_Authors', 'Species', 'Location', 'Trait', 'CH'))

  Trait_12_3 <- Trait_12_3 %>%
    dplyr::mutate(., slope.x = slope.x*slope.y) %>%
    dplyr::select(., -slope.y) %>%
    dplyr::rename(., slope.y = slope) %>%
    dplyr::mutate(., slopes.all = slope.y*slope.x)
  return(Trait_12_3)
}
utils::globalVariables(names = c('.', 'Trait_Categ_det.y', 'Fitness_Categ.y',
                                 'Country_code.y', 'Taxon.y', 'Trait.y',
                                 'Trait.x', 'slope.x', 'slope.y', 'slope'))


#' A plot of the product of slopes from testing the two first conditions vs. the slopes from the thrid condition
#'
#' @inheritParams parameters_definition
#'
#' @return Plots a product of slopes from the first two conditions
#' (on x axis) vs the slopes from the third condition (on y axis).
#'
#' @export
#'
plot_slopes_products <- function(data, phenological = TRUE,
                                 xlim = c(-0.4, 0.2),
                                 ylim = c(-0.4, 0.2)){
  graphics::plot(slope.y ~ slope.x,
                 data = data,
                 pch = 19, xlim = xlim, ylim = ylim,
                 xlab = 'Trait change over time (SD per year)',
                 ylab = 'Weighted mean selection differential', cex.lab = 1.3, cex = 0.7)
  graphics::abline(h = 0, lty = 3)
  graphics::abline(v = 0, lty = 3)

  if (phenological) {
    graphics::mtext('A)', side = 3, line = 1.5, adj = 0, cex = 2)
  } else {
    graphics::mtext('B)', side = 3, line = 1.5, adj = 0, cex = 2)
  }
  graphics::text(x = xlim[1] + 0.05, y = ylim[1] + 0.05, labels = 'III', col = 'red')
  graphics::text(x = xlim[1] + 0.05, y = ylim[2] - 0.05, labels = 'II', col = 'red')
  graphics::text(x = xlim[2] - 0.05, y = ylim[2] - 0.05, labels = 'I', col = 'red')
  graphics::text(x = xlim[2] - 0.05, y = ylim[1] + 0.05, labels = 'IV', col = 'red')

}

