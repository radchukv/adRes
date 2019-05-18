#' Define parameters for all functions
#'
#' @param condition A character specifying which condition is to be tested (for more details see
#' Radchuk et al. (in review)):
#' \itemize{\item '1' - Condition 1 (effect of year on climate);
#' \item '2' - Condition 2 (effect of climate on traits);
#' \item '3' - Condition 3 (testing whether weighted mean selection over years differs from 0);
#' \item '2b' - Condition 2, but also with abundance included as a predictor;
#' \item '3b' - testing whether selection changes over years.}
#' @param dat_nos Dataset without selection.
#' @param dat_prcs Dataset with selection - PRCS (Phenotypic Responses to
#'   Climate with Selection data).
#' @param data A dataframe containing per each study the time series of relevant variables
#' (i.e. yearly climate values, yearly trait values or yearly selection differentials)
#' to be analyzed.
#' @param digit An integer indicating how many digits to display on the screen.
#' @param first_column_name A string indicating the name of the first column which is taken from the row names of the table
#' @param fixed A string indicating the name of the fixed effect (called exactly
#'   as it is called in the dataframe \emph{meta_data}) if a meta-analytic model
#'   includes a fixed predictor, and NULL (default) otherwise.
#' @param formula_full A character string indicating the formula for the full
#'   model to be fitted.
#' @param formula_null A character string indicating the formula for the null
#'   model to be fitted.
#' @param id A character specifying the unique study id.
#' @param id_to_do A list of study ids for which to plot the raw data and fitted models.
#' @param increasing A Boolean specifying whether the slopes should be sorted
#' in increasing (TRUE) or decreasing (FALSE) order.
#' @param labels A vector specifying what should be used as labels for each study.
#' See Details for kinds of labels available.
#' @param labels_obj1 A list of labels to be used for slopes extracted from
#' meta_obj1 object. This list is returned by the function \code{\link{prepare_labels}}.
#' @param labels_obj2 A list of labels to be used for slopes extracted from
#' meta_obj2 object. This list is returned by the function \code{\link{prepare_labels}}
#' @param list A list of meta-analytical models from which statistics is to be extracted.
#' @param list_extra_meta_obj A list of meta-analytical model objects to be used for displaying the
#' global effects (either across all studies or in response to a fixed predictor).
#' @param list_fit A fitted object returned by the function \code{\link{fit_cond_id}}.
#' @param mar A vector specifying the plot margins, analogously to \code{\link[graphics]{par}}.
#' @param meta_data A dataframe with rows representing records per studies,
#'   including the effect sizes and required fixed and random effects.
#' @param meta_obj A fitted meta-analytical model object.
#' @param meta_obj1 A fitted meta-analytical model object whose global slopes and per-study effect
#' sizes will be displayed on the plot.
#' @param meta_obj2 An (optional) second fitted meta-analytical model object whose
#' global slopes and per-study effect sizes will be displayed on the plot.
#' @param model A meta-analytical model including studies as random effects.
#'   Heterogeneity metrics are to be computed for this model.
#' @param model_list A list of the models from which to extract the summary statistics for the table.
#' @param mod_sel An object produced by the function \code{\link{fit_cond_id}}
#' with the settings 'condition = 3'.
#' @param morphology A boolean indicating if the morphological data should be extracted (default is FALSE).
#' @param nb_cores An integer indicating how many cores should be used for assessing the LRT by
#'   bootstrap (see spaMM documentation for more details).
#' @param phenological A boolean indicating whether the plot for phenological traits should be produced (default is TRUE).
#' @param phenology A boolean indicating if the phenological data should be extracted (default is FALSE).
#' @param precipitation A boolean indicating if the precipitation data should be extracted (default is FALSE).
#' @param prior_weights A numeric vector of prior weights.
#' @param rand_trait A boolean indicating whether to include a trait type as a
#'   random effect in the model.
#' @param resid_formula A character string indicating the formula for the
#'   residual model to be fitted.
#' @param slopes_obj A list returned by \code{\link{prepare_slopes}} that contains
#' the lists with values for the slopes, their confidence intervals and labels.
#' @param sort A vector specifying the variables to use for sorting the labels.
#' @param stat A character specifying what should be extracted from the fitted models. Possible are:
#' LRT' for likelihood-ratio test statistics, 'RandStudy' for the random variance due to the study ID,
#' 'RandPub' for the random variance due to the publication ID, 'globslope' for global effect sizes,
#' 'globse' for the standard errors of global effect sizes.
#' @param table The dataframe to be exported as *.xlsx
#' @param temperature A boolean indicating if the temperature data should be extracted (default is FALSE).
#' @param traits A boolean indicating whether the phenotypic traits are
#'   used as response.
#' @param typeTab A character specifying what type of table to output: 'LRT' for a table
#' with likelihood-ratio statistics and random variances, 'efSizes' for a table with
#' estimated effect sizes and their standard errors, and 'heterog' for a table
#' of heterogeneity metrics.
#' @param xlim A vector specifying limits of x axis.
#' @param ylim A vector specifying limits of y axis.
#' @param ... Additional parameters for the function \code{\link[spaMM]{fitme}}.
#'
parameters_definition <- function(condition,
                                  dat_nos,
                                  dat_prcs,
                                  data,
                                  digit,
                                  first_column_name,
                                  fixed,
                                  formula_full,
                                  formula_null,
                                  id,
                                  id_to_do,
                                  increasing,
                                  labels,
                                  labels_obj1,
                                  labels_obj2,
                                  list,
                                  list_extra_meta_obj,
                                  list_fit,
                                  mar,
                                  meta_data,
                                  meta_obj,
                                  meta_obj1,
                                  meta_obj2,
                                  model,
                                  model_list,
                                  mod_sel,
                                  morphology,
                                  nb_cores,
                                  phenological,
                                  phenology,
                                  precipitation,
                                  prior_weights,
                                  rand_trait,
                                  resid_formula,
                                  slopes_obj,
                                  sort,
                                  stat,
                                  table,
                                  temperature,
                                  traits,
                                  typeTab,
                                  xlim,
                                  ylim,
                                  ...){
  return(invisible(NULL))
  }
