% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/produce_file_names.R
\name{produce_file_names}
\alias{produce_file_names}
\title{Produce File Names for nClimGrid Daily Dataset}
\usage{
produce_file_names(date, spatial_res, scaled = TRUE)
}
\arguments{
\item{date}{The date for which the file is requested.  A string in the format: YYYY-MM-DD.}

\item{spatial_res}{One of either 'ste' or 'cty'.  In the future this will support 'cen' for census tracts.}

\item{scaled}{Flag to denote if preliminary data are desired.  Note that setting this to TRUE will ONLY download preliminary data.}
}
\value{
A string of formatted nClimGrid daily file names.
}
\description{
Produce File Names for nClimGrid Daily Dataset
}
