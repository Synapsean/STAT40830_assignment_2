% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypackage.R
\name{plot_binned_summary}
\alias{plot_binned_summary}
\title{Plot binned_summary.indicator_data}
\usage{
plot_binned_summary(binned_data, title = NULL, ...)
}
\arguments{
\item{binned_data}{A \code{data.table} or \code{tibble} returned by \code{summary.indicator_data}.
It must contain columns like \code{country_name}, \code{year_bin_start}, \code{mean_value_bin},
and optionally \code{indicator_name} (if the summary was for a specific indicator or all).}

\item{title}{An optional character string for the plot title. If \code{NULL}, a default title is generated.}

\item{...}{Not used.}
}
\value{
A \code{ggplot} object.
}
\description{
Generates a time series plot of mean binned values from the output
of \code{summary.indicator_data}. This function visualises the aggregated data,
showing the mean value per year bin for each country. i.e. the mean "Arable land (hectares)" between '60-'64 etc
}
