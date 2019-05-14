#' Subsets the dataset according to the climatic variable and phenotypic category
#'
#' \code{prepare_data} subsets the dataset according to the specified conditions.
#'
#' @inheritParams parameters_definition
#'
#' @export
#'
#' @return A dataframe that is a subset of the dataframe supplied via
#' the argument 'data'. This dataframe is subset according to the specified
#' requirements.
#'
prepare_data <- function(data, temperature = FALSE, precipitation = FALSE,
                         phenology = FALSE, morphology = FALSE){
  if(temperature & precipitation){
    stop('please select temperature or precipiation, not both!')
  }

  levels_clim <- NA
  if (temperature) levels_clim <- 'Temperature'
  if (precipitation) levels_clim <- 'Precipitation'

  levels_trait <- NA
  if (phenology) levels_trait <- c(levels_trait, 'Phenological')
  if (morphology) levels_trait <- c(levels_trait, 'Morphological')

  return(droplevels(data[data$CH_Categ %in% levels_clim & data$Trait_Categ %in% levels_trait, ]))
}
