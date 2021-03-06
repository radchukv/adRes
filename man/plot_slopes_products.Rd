% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_supplement.R
\name{plot_slopes_products}
\alias{plot_slopes_products}
\title{A plot of the product of slopes from testing the two first conditions vs. the slopes from the thrid condition}
\usage{
plot_slopes_products(data, phenological = TRUE, xlim = c(-0.4, 0.2),
  ylim = c(-0.4, 0.2))
}
\arguments{
\item{data}{A dataframe containing per each study the time series of relevant variables
(i.e. yearly climate values, yearly trait values or yearly selection differentials)
to be analyzed.}

\item{phenological}{A boolean indicating whether the plot for phenological traits should be produced (default is TRUE).}

\item{xlim}{A vector specifying limits of x axis.}

\item{ylim}{A vector specifying limits of y axis.}
}
\value{
Plots a product of slopes from the first two conditions
(on x axis) vs the slopes from the third condition (on y axis).
}
\description{
A plot of the product of slopes from testing the two first conditions vs. the slopes from the thrid condition
}
