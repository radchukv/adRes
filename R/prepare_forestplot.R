#' Create the colours for the forest plots
#'
#' \code{prepare_colours} prepares the colours for the forest plots. This is
#' an internal function used by \code{\link{plot_forest}}.
#'
#' @inheritParams parameters_definition
#'
#' @return The list provided as an input with the colours added as a new row of
#' each data frame contained in this list.
#'
#' @seealso \code{\link{plot_forest}}.
#' @export
#'
#' @examples
#' \dontrun{
#' nb_cores <- min(c(parallel::detectCores(), 10))
#' meta_Sel_phen <- fit_all(data = dat_Sel,
#'                          temperature = TRUE, precipitation = FALSE,
#'                          phenology = TRUE, morphology = FALSE,
#'                          condition = '3', nb_cores = nb_cores,
#'                          rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_morph <- fit_all(data = dat_Sel,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = FALSE, morphology = TRUE,
#'                           condition = '3', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#'
#' test1 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res)
#'
#' test2 <- prepare_slopes(meta_obj2 = meta_Sel_phen$meta_res)
#'
#' test3 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                         meta_obj2 = meta_Sel_morph$meta_res)
#'
#' test_col_1 <- prepare_colours(test1)
#' test_col_2 <- prepare_colours(test2)
#' test_col_3 <- prepare_colours(test3)
#' }
#'
prepare_colours <- function(slopes_obj) {

  list_colours <- list(slopes_1 = 'black',
                       slopes_2 = 'darkgrey',
                       slope_PRCS = 'black',
                       slope_PRC = 'blue',
                       slope_PRCS_Phen = 'black',
                       slope_PRCS_Morph = 'darkgrey',
                       slope_PRC_Phen = 'darkblue',
                       slopes_PRC_Phen_Tax = 'darkblue',
                       slope_PRC_Morph = 'turquoise3',
                       slopes_PRC_Morph_Tax = 'turquoise3',
                       slopes_PRCS_Phen_Tax = 'darkblue',
                       slopes_PRCS_Morph_Tax = 'turquoise3',
                       slopes_Fitness_Temp = 'black',
                       slopes_Fitness_Precip = 'black')

  if (length(list_colours) != length(slopes_obj)) {
    stop('slopes_obj has a length not compatible with the function')
  }

  for (i in 1:length(list_colours)) {
    if (!is.null(slopes_obj[[i]])) {
      slopes_obj[[i]]$colour <- rep(list_colours[[i]],
                                    times = nrow(slopes_obj[[i]]))
    }
  }
  return(slopes_obj)
}


#' Create the x-axis labels for the forest plots
#'
#' \code{prepare_xlabs} prepares the x-axis label for the forest plots. This is
#' an internal function used by \code{\link{plot_forest}}.
#'
#' @inheritParams parameters_definition
#'
#' @return A vector of length one with the x-axis label. If \code{meta_obj} is
#' NULL, the function returns NULL.
#'
#' @seealso \code{\link{plot_forest}}.
#' @export
#'
prepare_xlabs <- function(meta_obj) {

  if (is.null(meta_obj)) {
    return(NULL)
  }

  condition    <- meta_obj$meta_data$condition[1]

  if (condition == '1') {
    if (meta_obj$meta_data$CH_Categ[1] == 'Temperature') {
      xlabs <- bquote('Effect of year on temperature (' *degree*C~'per year)')
    } else if (meta_obj$meta_data$CH_Categ[1] == 'Precipitation') {
      xlabs <- 'Effect of year on precipitation (mm per year)'
    }
  }

  if (condition == '2') {
    if (meta_obj$meta_data$CH_Categ[1] == 'Temperature') {
      xlabs <- bquote('Effect of temperature on trait (SD per'~degree*C*')')
    } else if (meta_obj$meta_data$CH_Categ[1] == 'Precipitation') {
      xlabs <- 'Effect of precipitation on trait (SD per mm)'
    }
  }

  if (condition == '3') {
    xlabs <- bquote('Selection on trait (SD'^-1*')')  # acounting for
  }

  if (condition == '3b') {
    xlabs <- bquote('Temporal trend in selection (year*SD)'^-1)
  }

  return(xlabs)
}


#' Create the labels for the forest plots
#'
#' \code{prepare_labels} prepares the labels for the forest plots. This is
#' an internal function used by \code{\link{plot_forest}}.
#'
#' @inheritParams parameters_definition
#'
#' @return A list with two elements: the labels and the order of the study.
#'
#' The possible types of labels are:
#' \itemize{
#' \item traits - the specific trait reported for each study;
#' \item fitness - the fitness measure used in each study;
#' \item country - a 2-letter country code;
#' \item authors - if there are more than two authors, the name of
#' the first one, otherwise the name of the first two.} If the label is set to
#' TRUE (default), it will be displayed on the plot.
#'
#' @seealso \code{\link{plot_forest}}
#' @export
#'
#' @examples
#' \dontrun{
#' dat_sel_phen <- prepare_data(data = dat_Sel,
#'                              temperature = TRUE, precipitation = FALSE,
#'                              phenology = TRUE, morphology = FALSE)
#' test_sel <- extract_effects_all_ids(data = dat_sel_phen,
#'                                     condition = '3', nb_cores = nb_cores)
#' test_meta_sel <- fit_meta(test_sel)
#' prepare_labels(meta_obj = test_meta_sel,
#'                labels = c(traits = TRUE,
#'                           fitness = TRUE,
#'                           country = TRUE,
#'                           authors = TRUE),
#'                sort = c("Species", "Study_Authors", "Fitness_Categ"),
#'                increasing = TRUE)
#' }
#'
#'
prepare_labels <- function(meta_obj = NULL, labels, sort, increasing) {

  if (is.null(meta_obj)) {
    return(list(labels = NULL, order = NULL))
  }

  if (any(!sort %in% colnames(meta_obj$meta_data))) {
    pb <- sort[!sort %in% colnames(meta_obj$meta_data)]
    if (length(sort) > 1) {
      sort <- sort[sort %in% colnames(meta_obj$meta_data)]
    } else {
      sort <- NULL
    }
    warning(paste('sort argument', pb, 'ignored!'))

  }

  fix <- function(x){
    if (is.factor(x)) return(as.character(x))
    return(x)
  }

  if (!is.null(sort)) {
    eval(parse(text = paste0("ranks <- order(", paste0("fix(meta_obj$meta_data$", sort, collapse = "), "), ")", ")")))
    orderedID <- c(paste0("id", as.character(meta_obj$meta_data$id[ranks])))
  } else {
    orderedID <- 1:nrow(meta_obj$meta_data)
  }

  if (!increasing) {
    orderedID <- rev(orderedID)
  }


  ordering_data <- match(orderedID, paste0("id", (meta_obj$meta_data$id)))


  labels_no_id <- rep("", length(meta_obj$meta_data$slope))

  if (labels["authors"]) {
    authors <- meta_obj$meta_data$Study_Authors[ordering_data]
    labels_no_id <- paste0(labels_no_id, authors, sep = ", ")
  }

  if (labels["traits"]) {
    traits <- gsub(pattern = '_', replacement = ' ', x = meta_obj$meta_data$Trait_Categ_det)[ordering_data]
    labels_no_id <- paste0(labels_no_id, traits,  sep = ", ")
  }

  if (labels["fitness"]) {
    fitness <- meta_obj$meta_data$Fitness_Categ[ordering_data]
    labels_no_id <- paste0(labels_no_id, fitness, sep = ", ")
  }

  if (labels["country"]) {
    country_codes <- meta_obj$meta_data$Country_code[ordering_data]
    labels_no_id <- paste0(labels_no_id, country_codes, sep = ", ")
  }
  length_char <- nchar(labels_no_id)
  labels_no_id <- substr(labels_no_id, start = 1, stop = length_char - 2)

  StudyID <- meta_obj$meta_data$PaperID[ordering_data]
  labels <- paste0(labels_no_id, ", ", StudyID)

  return(list(labels = labels, order = ordering_data))
}
utils::globalVariables('ranks')


#' Prepare the slopes for the forest plots
#'
#' \code{prepare_slopes} extracts the slopes needed for the forest plots from
#' the fitted model objects.
#'
#' @inheritParams parameters_definition
#'
#' @return A list containing the slopes and other information required
#' for the forest plot.
#'
#' @seealso \code{\link{plot_forest}}
#' @export
#'
#' @examples
#' \dontrun{
#' nb_cores <- 2L ## increase the number for using more cores
#' meta_Sel_phen <- fit_all(data = dat_Sel,
#'                          temperature = TRUE, precipitation = FALSE,
#'                          phenology = TRUE, morphology = FALSE,
#'                          condition = '3', nb_cores = nb_cores,
#'                          rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_morph <- fit_all(data = dat_Sel,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = FALSE, morphology = TRUE,
#'                           condition = '3', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_phen_Fitn <- fit_all(data = dat_Sel,
#'                               temperature = TRUE, precipitation = FALSE,
#'                               phenology = TRUE, morphology = FALSE,
#'                               condition = '3', nb_cores = nb_cores,
#'                               rand_trait = FALSE, fixed = 'Fitness_Categ',
#'                               digit = 3)
#'
#' test1 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res)
#'
#' test2 <- prepare_slopes(meta_obj2 = meta_Sel_phen$meta_res)
#'
#' test3 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                         meta_obj2 = meta_Sel_morph$meta_res)
#'
#' test4 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                         list_extra_meta_obj =
#'                         list(meta_Sel_phen_Fitn$meta_res))
#'
#' test5 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                         meta_obj2 = meta_Sel_morph$meta_res,
#'                         list_extra_meta_obj =
#'                         list(meta_Sel_phen_Fitn$meta_res))
#'
#' test6 <- prepare_slopes(meta_obj1 = meta_Sel_phen_Fitn$meta_res,
#'                         list_extra_meta_obj =
#'                         list(meta_Sel_morph$meta_res,
#'                         meta_Sel_phen$meta_res))
#' }
#'
#'

prepare_slopes <- function(meta_obj1 = NULL,
                           meta_obj2 = NULL,
                           list_extra_meta_obj = NULL,
                           labels_obj1 = NULL,
                           labels_obj2 = NULL) {

  if (! is.null(list_extra_meta_obj) && !is.list(list_extra_meta_obj)) {
    stop('list_extra_meta_obj must be a list!')
  }

  ## initialise empty list
  list_out <- list(slopes_1 = NULL,
                   slopes_2 = NULL,
                   slope_PRCS = NULL,
                   slope_PRC = NULL,
                   slope_PRCS_Phen = NULL,
                   slope_PRCS_Morph = NULL,
                   slope_PRC_Phen = NULL,
                   slopes_PRC_Phen_Tax = NULL,
                   slope_PRC_Morph = NULL,
                   slopes_PRC_Morph_Tax = NULL,
                   slopes_PRCS_Phen_Tax = NULL,
                   slopes_PRCS_Morph_Tax = NULL,
                   slopes_Fitness_Temp = NULL,
                   slopes_Fitness_Precip = NULL)

  ## definition of internal extractors
  extract_slopes <- function(meta_obj, labels_obj = NULL) {
    if (is.null(meta_obj)) {
      return(NULL)
    }
    d <- data.frame(slope = meta_obj$meta_data$slope,
                    lwr = meta_obj$meta_data$slope + stats::qnorm(0.025) * meta_obj$meta_data$SE_slope,
                    upr = meta_obj$meta_data$slope + stats::qnorm(0.975) * meta_obj$meta_data$SE_slope,
                    cex = meta_obj$meta_data$Npoints/(2*mean(meta_obj$meta_data$Npoints)) + 0.5,
                    PaperID = meta_obj$meta_data$PaperID)

    if (!is.null(labels_obj)) {
      d <- d[labels_obj$order, ]
      d$label <- labels_obj$labels
    }

    return(d)
  }

  extract_global_slopes <- function(meta_obj, label = 'label_to_do') {
    d <- data.frame(slope = meta_obj$global_slope,
               lwr = meta_obj$global_slope + stats::qnorm(0.025) * meta_obj$global_se,
               upr = meta_obj$global_slope + stats::qnorm(0.975) * meta_obj$global_se,
               cex = 1)
    if (is.character(label)) {
      d$label <- label
    } else if (is.logical(label) && label) {
      d$label <- names(meta_obj$global_slope)
    }
    return(d)
  }

  get_predictors <- function(meta_obj) {
    preds <- strsplit(strsplit(deparse(stats::formula(meta_obj$model)[[3]]), split = "(", fixed = TRUE)[[1]][1], split = '+', fixed = TRUE)[[1]]
    preds <- gsub(" ", "", preds)
    preds <- preds[preds != ""]
    return(preds[length(preds)])
  }

  ## function to check that dataset is PRCS (FALSE if NULL!)
  PRCS <- function(meta_obj) {
    if (is.null(meta_obj)) return(FALSE)
    if (length(unique(meta_obj$meta_data$data)) == 1) return(TRUE)
    return(FALSE)
  }

  ## function to check that dataset is PRC (FALSE if NULL!)
  PRC <- function(meta_obj) {
    if (is.null(meta_obj)) return(FALSE)
    if (length(unique(meta_obj$meta_data$data)) == 2) return(TRUE)
    return(FALSE)
  }

  ## slopes_1
  slopes1 <- extract_slopes(meta_obj1, labels_obj = labels_obj1)
  if (!is.null(slopes1)) {
    list_out[['slopes_1']] <- slopes1
  }

  ## slopes 2
  slopes2 <- extract_slopes(meta_obj2, labels_obj = labels_obj2)
  if (!is.null(slopes2)) {
    list_out[['slopes_2']] <- slopes2
  }

  ## slope_PRCS
  slope_PRCS <- function(meta_obj) {
    if (PRCS(meta_obj) && meta_obj$meta_data$condition[1] == 1) {
      return(list(extract_global_slopes(meta_obj, label = 'PRCS')))
    } else {
      return(list_out['slope_PRCS'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRCS'] <- slope_PRCS(extra_meta_obj)

  ## slope_PRC
  slope_PRC <- function(meta_obj) {
    if (PRC(meta_obj) && meta_obj$meta_data$condition[1] == 1) {
      return(list(extract_global_slopes(meta_obj, label = 'PRC')))
    } else {
      return(list_out['slope_PRC'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRC'] <- slope_PRC(extra_meta_obj)

  ## slope_PRCS_Phen
  slope_PRCS_Phen <- function(meta_obj) {
    if (PRCS(meta_obj) && unique(meta_obj$meta_data$Trait_Categ) == 'Phenological' &&
        meta_obj$meta_data$condition[1] != 1 &&
        get_predictors(meta_obj) == '1') {
      return(list(extract_global_slopes(meta_obj, label = 'Phenological')))
    } else {
      return(list_out['slope_PRCS_Phen'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRCS_Phen'] <- slope_PRCS_Phen(extra_meta_obj)


  ## slope_PRCS_Morph
  slope_PRCS_Morph <- function(meta_obj) {
    if (PRCS(meta_obj) && unique(meta_obj$meta_data$Trait_Categ) == 'Morphological' &&
        meta_obj$meta_data$condition[1] != 1 &&
        get_predictors(meta_obj) == '1') {
      return(list(extract_global_slopes(meta_obj, label = 'Morphological')))
    } else {
      return(list_out['slope_PRCS_Morph'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRCS_Morph'] <- slope_PRCS_Morph(extra_meta_obj)


  ## slope_PRC_Phen
  slope_PRC_Phen <- function(meta_obj) {
    if (PRC(meta_obj) && 'Phenological' %in% unique(meta_obj$meta_data$Trait_Categ) &&
        meta_obj$meta_data$condition[1] != 1 &&
        get_predictors(meta_obj) == '1') {
      return(list(extract_global_slopes(meta_obj, label = 'Phenological')))
    } else {
      return(list_out['slope_PRC_Phen'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRC_Phen'] <- slope_PRC_Phen(extra_meta_obj)


  ## slope_PRC_Morph
  slope_PRC_Morph <- function(meta_obj) {
    if (PRC(meta_obj) && 'Morphological' %in% unique(meta_obj$meta_data$Trait_Categ) &&
        meta_obj$meta_data$condition[1] != 1 &&
        get_predictors(meta_obj) == '1') {
      return(list(extract_global_slopes(meta_obj, label = 'Morphological')))
    } else {
      return(list_out['slope_PRC_Morph'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slope_PRC_Morph'] <- slope_PRC_Morph(extra_meta_obj)

  ## slopes_PRC_Phen_Tax
  slopes_PRC_Phen_Tax <- function(meta_obj) {
    if (PRC(meta_obj) && get_predictors(meta_obj) == 'Taxon'
        && unique(meta_obj$meta_data$Trait_Categ) == 'Phenological' &&
        meta_obj$meta_data$condition[1] != 1) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_PRC_Phen_Tax'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj)list_out['slopes_PRC_Phen_Tax'] <- slopes_PRC_Phen_Tax(extra_meta_obj)


  ## slopes_PRC_Morph_Tax
  slopes_PRC_Morph_Tax <- function(meta_obj) {
    if (PRC(meta_obj) && get_predictors(meta_obj) == 'Taxon'
        && unique(meta_obj$meta_data$Trait_Categ) == 'Morphological' &&
        meta_obj$meta_data$condition[1] != 1) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_PRC_Morph_Tax'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slopes_PRC_Morph_Tax'] <- slopes_PRC_Morph_Tax(extra_meta_obj)

  ## slopes_PRCS_Phen_Tax
  slopes_PRCS_Phen_Tax <- function(meta_obj) {
    if (PRCS(meta_obj) && get_predictors(meta_obj) == 'Taxon'
        && unique(meta_obj$meta_data$Trait_Categ) == 'Phenological' &&
        meta_obj$meta_data$condition[1] != 1) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_PRCS_Phen_Tax'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj)list_out['slopes_PRCS_Phen_Tax'] <- slopes_PRCS_Phen_Tax(extra_meta_obj)


  ## slopes_PRCS_Morph_Tax
  slopes_PRCS_Morph_Tax <- function(meta_obj) {
    if (PRCS(meta_obj) && get_predictors(meta_obj) == 'Taxon'
        && unique(meta_obj$meta_data$Trait_Categ) == 'Morphological' &&
        meta_obj$meta_data$condition[1] != 1) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_PRCS_Morph_Tax'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slopes_PRCS_Morph_Tax'] <- slopes_PRCS_Morph_Tax(extra_meta_obj)


  ## slopes_Fitness_Temp
  slopes_Fitness_Temp <- function(meta_obj) {
    if (!is.null(meta_obj) &&
        any(names(meta_obj$global_slope) %in% c('Recruitment', 'Survival', 'Reproduction')) &&
        unique(meta_obj$meta_data$CH_Categ == 'Temperature')) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_Fitness_Temp'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj) list_out['slopes_Fitness_Temp'] <- slopes_Fitness_Temp(extra_meta_obj)


  ## slopes_Fitness_Precip
  slopes_Fitness_Precip <- function(meta_obj) {
    if (!is.null(meta_obj) &&
        any(names(meta_obj$global_slope) %in% c('Recruitment', 'Survival', 'Reproduction')) &&
        unique(meta_obj$meta_data$CH_Categ == 'Precipitation')) {
      return(list(extract_global_slopes(meta_obj, label = TRUE)))
    } else {
      return(list_out['slopes_Fitness_Precip'])
    }
  }

  for(extra_meta_obj in list_extra_meta_obj)list_out['slopes_Fitness_Precip'] <- slopes_Fitness_Precip(extra_meta_obj)


  return(list_out)
}


#' Prepare the data for the forest plots
#'
#' \code{prepare_data_plot} prepares the data for the forest plots. This is
#' an internal function used by \code{\link{plot_forest}}.
#'
#' @inheritParams parameters_definition
#'
#' @return A dataframe with the information used for plotting.
#'
#' @seealso \code{\link{plot_forest}}.
#' @export
#'
#' @examples
#' \dontrun{
#' nb_cores <- min(c(parallel::detectCores(), 10))
#' meta_Sel_phen <- fit_all(data = dat_Sel,
#'                          temperature = TRUE, precipitation = FALSE,
#'                          phenology = TRUE, morphology = FALSE,
#'                          condition = '3', nb_cores = nb_cores,
#'                          rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' meta_Sel_morph <- fit_all(data = dat_Sel,
#'                           temperature = TRUE, precipitation = FALSE,
#'                           phenology = FALSE, morphology = TRUE,
#'                           condition = '3', nb_cores = nb_cores,
#'                           rand_trait = FALSE, fixed = NULL, digit = 3)
#'
#' lab1 <- prepare_labels(meta_obj = meta_Sel_phen$meta_res,
#'                        labels = c(traits = TRUE,
#'                                   fitness = TRUE,
#'                                   country = TRUE,
#'                                   authors = TRUE),
#'                       sort = c("Species"),
#'                       increasing = TRUE)
#'
#' lab2 <-  prepare_labels(meta_obj = meta_Sel_morph$meta_res,
#'                        labels = c(traits = TRUE,
#'                                   fitness = TRUE,
#'                                   country = TRUE,
#'                                   authors = TRUE),
#'                       sort = c("Species"),
#'                       increasing = TRUE)
#'
#'
#' test1 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                        labels_obj1 = lab1,
#'                        labels_obj2 = lab2)
#'
#' test2 <- prepare_slopes(meta_obj1 = meta_Sel_phen$meta_res,
#'                         meta_obj2 = meta_Sel_morph$meta_res,
#'                         labels_obj1 = lab1,
#'                         labels_obj2 = lab2,
#'                         list_extra_meta_obj = list(meta_Sel_phen$meta_res,
#'                                                   meta_Sel_morph$meta_res,
#'                                                   mod_Sel_T_phen_Fitn$meta_res))
#'
#' test_col_1 <- prepare_colours(test1)
#' test_col_2 <- prepare_colours(test2)
#' prep_data_test1 <- prepare_data_plot(test_col_1)
#' prep_data_test2 <- prepare_data_plot(test_col_2)
#' }
#'
prepare_data_plot <- function(slopes_obj) {

    get_obj <- function(slopes_obj, what) {
     out1 <- unlist(lapply(slopes_obj[1:2], function(x) x[, what]))
     out2 <- unlist(lapply(slopes_obj[3:length(slopes_obj)], function(x) x[, what]))
     if (length(out2) == 0) return(out1)
     if (length(out2) < 5) return(c(out1, NA, out2))
     return(c(out1, NA, out2[1:2], NA, out2[3:length(out2)]))
  }

  out <- data.frame(slope = get_obj(slopes_obj, 'slope'),
                    lwr = get_obj(slopes_obj, 'lwr'),
                    upr = get_obj(slopes_obj, 'upr'),
                    colour = get_obj(slopes_obj, 'colour'),
                    label = get_obj(slopes_obj, 'label'),
                    cex = get_obj(slopes_obj, 'cex'),
                    stringsAsFactors = FALSE)

  ymax <- NROW(slopes_obj[[1]]) + NROW(slopes_obj[[2]])
  ymin <- ymax - nrow(out) + 1
  out$y <- ymax:ymin

  return(out)
}
