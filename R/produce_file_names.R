#' Produce File Names for nClimGrid Daily Dataset
#'
#' @param date The date for which the file is requested.  A string in the format: YYYY-MM-DD.
#' @param spatial_res One of either 'ste' or 'cty'.  In the future this will support 'cen' for census tracts.
#' @param scaled Flag to denote if preliminary data are desired.  Note that setting this to TRUE will ONLY download preliminary data.
#'
#' @return
#'   A string of formatted nClimGrid daily file names.
#' @export
#'
#' @examples
produce_file_names <- function(date, spatial_res, scaled = TRUE) {
  prefix <- 's3://noaa-nclimgrid-daily-pds/EpiNOAA/parquet/'
  month <-  lubridate::month(date) %>%
    stringr::str_pad(width = 2, pad = '0')
  year <- lubridate::year(date)

  if (!scaled) {
    scaled_flag <- 'prelim'
  } else if (scaled) {
    scaled_flag <- 'scaled'
  } else {
    stop("Scaled must be either TRUE or FALSE")
  }

  out <- paste0(
    prefix,
    year,
    month,
    '-',
    spatial_res,
    '-',
    scaled_flag,
    '.parquet'
  )

  return(out)
}
