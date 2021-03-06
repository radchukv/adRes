#' Values of selection differentials for each year per study
#'
#' A dataset containing yearly selection values for each study.
#' Other attributes of the data represent study-specific meta-data.
#'
#' @name dat_Sel
#' @docType data
#' @format A data frame with 1296 rows and 41 columns:
#' \describe{
#' \item{Study_Authors}{First (if the paper was co-authored by more than 2 authors)
#' or first two authors of the paper}
#' \item{Journal}{Journal in which the study was published}
#' \item{Year_pub}{Publication year}
#' \item{Title}{Paper title}
#' \item{Duration}{Duration of the study as reported in the paper}
#' \item{Species}{Latin name of the study species}
#' \item{Taxon}{A taxon: Aves, Mammalia, Reptilia, Amphibia, Arachnida, Insecta}
#' \item{Location}{Study location as reported in the paper}
#' \item{Country}{A country where the study was conducted}
#' \item{Country_code}{A two-letter code of the country where the study took place}
#' \item{Trait}{Type of phenotypic trait as reported in the paper}
#' \item{Trait_Categ_det}{Trait as reported in the study}
#' \item{Trait_Categ}{Trait category: phenological or morphological}
#' \item{CH}{Type of climatic variable as reported in the paper}
#' \item{CH_Categ_det}{Categorization of climatic variables into groups including similar
#' climatic variables}
#' \item{CH_Categ}{Category of climatic variable: temperature or precipitation}
#' \item{CH_value}{Value of the climatic variable measured each year}
#' \item{Trait_MeanPopValue}{Mean population trait value per year}
#' \item{Trait_SE}{Standard error of the yearly mean population trait value}
#' \item{Trait_Npoints}{Number of data points available to measure mean yearly
#' population trait value (and SE)}
#' \item{Fitness}{Fitness measure as reported in the paper}
#' \item{Fitness_Categ}{Fitness category: recruitment, reproduction and adult survival}
#' \item{Population_Metrics}{A measure of population abundance as reported in the paper}
#' \item{Population_value}{Yearly values of population abundance}
#' \item{Selection_Par}{Type of selection measure: selection differential or selection gradient}
#' \item{Selection_type}{Via what component selection was measured: recruitment, reproduction, adult survival}
#' \item{Selection_mean}{Values of yearly selection differentials}
#' \item{Selection_SE}{Standard errors of yearly selection differentials}
#' \item{Selection_Npoints}{Number of data points available to measure yearly selection differential and its SE}
#' \item{Clim}{Values of climatic variables standardized so that they are comparable among studies}
#' \item{PaperID}{ID of the paper (as in Supplementary Table S3) from which the study was taken}
#' \item{Coord_Lat_deg}{Latitude (degrees) of the study location}
#' \item{Coord_Lat_min}{Latitude (minutes) of the study location}
#' \item{GenerationLength_yr}{Generation length of the species, in years (mainly extracted from
#' the BirdLife International database}
#' \item{Ref_Suppl}{Reference number for the figures in Supplementary Information of the MS}
#' \item{Morph_type}{Categorization of morphological traits into 3 groups, depending on
#' the type of measure: 'skeletal', 'Mass', and 'Both' (set NA for phenological traits)}
#' \item{Blood}{Whether the study species is endothermic ('Warm') or not ('Cold')}
#' \item{Trait_Cat}{Categorization of phenological traits into 3 groups:
#' 'arrival', 'breeding' and 'development', as in Cohen et al. 2018 (set NA for morphological traits)}
#' \item{data}{Dataset to which the study belongs: PRCS if selection data are available and PRC otherwise}
#' \item{id}{A unique study id}
#' \item{year}{Year when the observation was made}
#'
#' }
NULL
