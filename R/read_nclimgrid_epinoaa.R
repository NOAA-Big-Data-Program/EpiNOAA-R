#' Read and Filter nClimGrid Daily Data
#'
#' @param beginning_date The inclusive start date for the filtered dataset.  A string in the format: YYYY-MM-DD.  The earliest date is 1951-01-01.
#' @param end_date The inclusive end date for the filtered dataset.  A string in the format: YYYY-MM-DD. The latest date is usually a month prior to the current date.
#' @param spatial_res One of either 'ste' or 'cty' for county and state respectively. These correspond to spatial resolutions made available in the original nClimGrid dataset.
#' @param scaled_only This defaults to true as this function is only configured to pull scaled data.  NOAA preliminary data can be found through the AWS bucket on the AWS Open Data Registry.
#' @param states One of either 'all', or a character vector of state abbreviations.
#' @param counties One of either 'all', or a character vector of county names.
#' @param tracts Defaults to all.  Census tracts will be added in a later version of this package.
#' @param workers  The number of workers to use for parallel processing.  Defaults to 2.  Be careful not to exceed capacity of your machine.
#'
#' @return
#'
#' A tibble of spatio-temporally filtered nClimGrid Daily data.
#'
#' @export
#'
#' @examples
#' \donttest{read_nclimgrid_epinoaa(beginning_date="1951-01-01",
#' end_date="1951-02-01",spatial_res="cty",states="NC",counties="Wake County", workers = 2)}
read_nclimgrid_epinoaa <- function(
  beginning_date = '1951-01-01',
  end_date = '2021-10-01',
  spatial_res = 'cty',
  scaled_only = TRUE,
  states = 'all',
  counties = 'all',
  tracts = 'all',
  workers = 2
) {
  sample_file <- produce_file_names(date = '2000-10-01',
                                    spatial_res,
                                    scaled = TRUE)
  sample_data <- arrow::read_parquet(sample_file)

  if (states == 'all') {
    selected_states <- unique(sample_data$state)
  } else {
    selected_states <- states
  }

  if ((counties == 'all') & (spatial_res != 'ste')) {
    selected_counties <-  unique(sample_data$county)
  } else {
    selected_counties <- counties
  }

  if ((tracts == 'all') & (spatial_res == 'cen')) {
    selected_tracts <- unique(sample_data$tract)
  } else {
    selected_tracts <- tracts
  }

  beginning <- lubridate::ymd(beginning_date)
  end <- lubridate::ymd(end_date)

  interval_sequence <- seq(beginning, end, by = 'month')

  files <- interval_sequence %>%
    purrr::map_chr( ~ produce_file_names(., spatial_res,
                                         scaled = scaled_only))

  future::plan(future::multisession, workers = workers)

  nclimgrid <-
    tryCatch(
      {
        message('Reading in nClimGrid files...')
        files %>%
          furrr::future_map_dfr( ~ arrow::read_parquet(.))
      },
      error = function(cond) {
        message('You seem to be requesting data that does not exist.
                It is likely that you are requesting either preliminary data
                that is not available or scaled data that is not available.
                This function will only return preliminary data when
                `scaled_only = FALSE` and only scaled data when `scaled_only = TRUE`.')
        message(cond)
        return()
      }
    )

  future::plan(future::sequential)

  day_filtering <- nclimgrid %>%
    dplyr::mutate(date = lubridate::mdy(date)) %>%
    dplyr::filter(date >= beginning,
           date <= end)

  if (spatial_res == 'cty') {
    filtered <- day_filtering %>%
      dplyr::filter(state %in% selected_states,
             county %in% selected_counties)
  } else if (spatial_res == 'ste') {
    filtered <- day_filtering %>%
      dplyr::filter(state %in% selected_states)
  } else if (spatial_res == 'cen') {
    filtered <- day_filtering %>%
      dplyr::filter(state %in% selected_states,
             county %in% selected_counties,
             tract %in% selected_tracts)
  } else {
    filtered <- NULL
    print('Spatial resolution (spatial_res) must be one of cty, or ste')
  }

  return(filtered)
}
