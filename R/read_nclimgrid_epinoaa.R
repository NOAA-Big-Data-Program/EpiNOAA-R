#' Read and Filter nClimGrid Daily Data
#'
#' @param beginning_date The inclusive start date for the filtered dataset.  A string in the format: YYYY-MM-DD.  The earliest date is 1951-01-01.
#' @param end_date The inclusive end date for the filtered dataset.  A string in the format: YYYY-MM-DD. The latest date is usually a month prior to the current date.
#' @param spatial_res One of either 'ste' or 'cty' for county and state respectively. These correspond to spatial resolutions made available in the original nClimGrid dataset.
#' @param states One of either 'all', or a character vector of state abbreviations.
#' @param counties One of either 'all', or a character vector of county names.
#' @param tracts One of either 'all', or a character vector of census tract fips codes.
#'
#' @return
#'
#' A tibble of spatio-temporally filtered nClimGrid Daily data.
#'
#' @export
#'
#' @examples
#' \donttest{read_nclimgrid_epinoaa(beginning_date="1951-01-01",
#' end_date="1951-02-01",spatial_res="cty",states="NC",counties="NC: Wake County")}
read_nclimgrid_epinoaa <- function(
  beginning_date = '1951-01-01',
  end_date = '2021-10-01',
  spatial_res = 'cty',
  states = 'all',
  counties = 'all',
  tracts = 'all'
) {


  beginning_year <- lubridate::year(lubridate::ymd(beginning_date))
  end_year <- lubridate::year(lubridate::ymd(end_date))

  year_sequence <- as.character(beginning_year:end_year)

  # Connect to parquet dataset on AWS
  bucket = arrow::s3_bucket('noaa-nclimgrid-daily-pds')
  parquet_path <- str_glue("/EpiNOAA/v1-0-0/parquet/{spatial_res}/")
  nc_data <- arrow::open_dataset(bucket$path(parquet_path))

  # Filter for relevant dates
  nc_query <-  nc_data %>%
    filter(YEAR %in% year_sequence) %>%
    filter(date <= lubridate::ymd(end_date)) %>%
    filter(date >= lubridate::ymd(beginning_date))

  if (length(states) > 1) {
    nc_query <- nc_query %>%
      filter(postal_code %in% c(states))
  } else if (states != 'all') {
      nc_query <- nc_query %>%
        filter(postal_code %in% c(states))
  } else {
    nc_query <- nc_query
  }

  if (length(counties) > 1) {
    nc_query <- nc_query %>%
      filter(region_name %in% c(counties))
  } else if (counties != 'all') {
    nc_query <- nc_query %>%
      filter(region_name %in% c(counties))
  } else {
    nc_query <- nc_query
  }

  if (length(tracts) > 1) {
    nc_query <- nc_query %>%
      filter(fips %in% c(tracts))
  } else if (tracts != 'all') {
    nc_query <- nc_query %>%
      filter(fips %in% c(tracts))
  } else {
    nc_query <- nc_query
  }

  nclimgrid <-
    tryCatch(
      {
        message('Reading in nClimGrid files...')
        nc_query %>% collect()
      },
      error = function(cond) {
        message('The query for NClimGrid data did not complete.  Please check your inputs, internect connection, and see the error message below.  ')
        message(cond)
        return()
      }
    )

  return(nclimgrid)
}
