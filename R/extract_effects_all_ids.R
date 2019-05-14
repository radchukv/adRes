#' Fit model on each study, for a given condition, and extract the effect sizes and their standard errors
#'
#' \code{extract_effects_all_ids} retrieves the effect size and SE for each study
#' by calling the function \code{\link{extract_effects}} on each study.
#'
#' @inheritParams parameters_definition
#'
#' @return A dataframe containing all effect sizes and SE for the meta-analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' dat_clim <- prepare_data(data = dat_Clim, temperature = TRUE,
#'                          precipitation = FALSE,
#'                          phenology = TRUE, morphology = TRUE)
#' test <- extract_effects_all_ids(data = dat_clim,
#'                                 condition = "1", nb_cores = 4L)
#' head(test)
#'
#' dat_sel_phen <- prepare_data(data = dat_Sel, temperature = TRUE,
#'                              precipitation = FALSE,
#'                              phenology = TRUE, morphology = FALSE)
#' test_sel <- extract_effects_all_ids(data = dat_sel_phen,
#'                                     condition = "3", nb_cores = 4L)
#'
#'
#' dat_T_Trait <- prepare_data(data = dat_Trait, temperature = TRUE,
#'                             precipitation = FALSE,
#'                             phenology = TRUE, morphology = FALSE)
#' test_Trait <- extract_effects_all_ids(data = dat_T_Trait,
#'                                       condition = "2", nb_cores = 4L)
#' }
#'
extract_effects_all_ids <- function(data, condition, nb_cores) {
  ids <- unique(data$id)
  pb <- utils::txtProgressBar(min = 1,
                              max = length(ids),
                              style = 3)

  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  cl <- parallel::makeCluster(nb_cores)
  doSNOW::registerDoSNOW(cl)

  efs <- foreach::foreach(i = 1:length(ids), .options.snow = opts)

  all_efs <- foreach::`%dopar%`(efs, {
    id <- ids[i]
    extract_effects(fit_cond_id(data = data, id = id, condition = condition))
  })

  parallel::stopCluster(cl)

  res <- do.call('rbind', all_efs)

  ## otherwise the order of the levels is changed
  res$Trait_Cat <- factor(res$Trait_Cat, levels = levels(data$Trait_Cat))
  res$Morph_type <- factor(res$Morph_type, levels = levels(data$Morph_type))
  res$Fitness_Categ <- factor(res$Fitness_Categ, levels = levels(data$Fitness_Categ))
  res$Taxon <- factor(res$Taxon, levels = levels(data$Taxon))
  res$Blood <- factor(res$Blood, levels = levels(data$Blood))
  return(res)

}
utils::globalVariables("i")
