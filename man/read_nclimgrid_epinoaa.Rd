% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_nclimgrid_epinoaa.R
\name{read_nclimgrid_epinoaa}
\alias{read_nclimgrid_epinoaa}
\title{Read and Filter nClimGrid Daily Data}
\usage{
read_nclimgrid_epinoaa(
  beginning_date = "1951-01-01",
  end_date = "2021-10-01",
  spatial_res = "cty",
  states = "all",
  counties = "all",
  tracts = "all"
)
}
\arguments{
\item{beginning_date}{The inclusive start date for the filtered dataset.  A string in the format: YYYY-MM-DD.  The earliest date is 1951-01-01.}

\item{end_date}{The inclusive end date for the filtered dataset.  A string in the format: YYYY-MM-DD. The latest date is usually a month prior to the current date.}

\item{spatial_res}{One of either 'ste' or 'cty' for county and state respectively. These correspond to spatial resolutions made available in the original nClimGrid dataset.}

\item{states}{One of either 'all', or a character vector of state abbreviations.}

\item{counties}{One of either 'all', or a character vector of county names.}

\item{tracts}{One of either 'all', or a character vector of census tract fips codes.}
}
\value{
A tibble of spatio-temporally filtered nClimGrid Daily data.
}
\description{
Read and Filter nClimGrid Daily Data
}
\examples{
\donttest{read_nclimgrid_epinoaa(beginning_date="1951-01-01",
end_date="1951-02-01",spatial_res="cty",states="NC",counties="NC: Wake County")}
}
