#' Merge two datasets into a single, PRC (Phenotypic Responses to Climate)
#' dataset
#'
#' \code{merge_two_data} Merges the dataset with selection (PRCS) and the one
#' that does not have selection data into a single dataset - PRC
#'
#' This function merges the dataset without selection with the one that
#' contains selection data, PRCS (Phenotypic Responses to Climate with
#' Selection data. The unique studies are retained in the resulting PRC
#' (Phenotypic Responses to Climate) dataset.
#'
#' @inheritParams parameters_definition
#'
#' @return A dataframe combining the studies for which selection data are
#'   available with those for which no selection data are available. This
#'   constitutes a PRC dataset ('Phenotypic Responses to Climate')

merge_two_data <- function(dat_nos, dat_prcs, traits = FALSE){
  if (traits){
    dat_sub_prcs <- dat_prcs[, !colnames(dat_prcs) %in%
                               c('Population_value', 'Selection_Par',
                                 'Selection_type', 'Selection_mean',
                                 'Selection_SE', 'Selection_Npoints',
                                 'Coord_Lat_deg', 'Coord_Lat_min',
                                 'ScaledPop', 'Ref_Suppl',
                                 'GenerationLength_yr')]
    } else {
    dat_sub_prcs <- dat_prcs[, !colnames(dat_prcs) %in%
                               c('Population_value', 'Selection_Par',
                                 'Selection_type', 'Selection_mean',
                                 'Selection_SE', 'Selection_Npoints',
                                 'Coord_Lat_deg', 'Coord_Lat_min',
                                 'Ref_Suppl', 'GenerationLength_yr')]
  }
dat_all <- rbind(dat_sub_prcs, dat_nos)
return(dat_all)
}
