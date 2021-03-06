% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_supplement.R
\name{plot_funnel}
\alias{plot_funnel}
\title{Plots a funnel plot for the inspection of the potential publication bias}
\usage{
plot_funnel(meta_obj, model = "rma")
}
\arguments{
\item{meta_obj}{A fitted meta-analytical model object.}

\item{model}{A meta-analytical model including studies as random effects.
Heterogeneity metrics are to be computed for this model.}
}
\value{
Plots a funnel plot to assess the potential publication bias.
}
\description{
Plots a funnel plot for the inspection of the potential publication bias
}
\examples{
nb_cores <- 2L
mod_T_prcs <- fit_all(data = dat_Clim,
                      temperature = TRUE, precipitation = FALSE,
                      phenology = TRUE, morphology = TRUE,
                      condition = '1', nb_cores = nb_cores,
                      rand_trait = FALSE, fixed = NULL, digit = 3)
plot_funnel(meta_obj = mod_T_prcs)

mod_phen_T_prcs <- fit_all(data = dat_Trait,
                           temperature = TRUE, precipitation = FALSE,
                           phenology = TRUE, morphology = FALSE,
                           condition = '2', nb_cores = nb_cores,
                           rand_trait = FALSE, fixed = NULL, digit = 3)
plot_funnel(meta_obj = mod_phen_T_prcs)

}
