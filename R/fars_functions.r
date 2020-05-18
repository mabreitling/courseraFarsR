#' Import FARS data from compressed csv file
#'
#' \code{fars_read()} imports data of the Fatality Analysis Reporting System
#' (FARS). The function makes use of the delivered compressed csv files. The
#' data is converted to and outputted as a tibble. Function throws an error if
#' file does not exist.
#'
#' @param filename Character string giving the path to the csv file to be read
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @return returns tidy tibble of accident data
#' @examples fars_read('accident_2013.csv.bz2')
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creating FARS filenames according to naming standard
#'
#' Helper function to stitch together FARS data filename for a given \code{year}.
#'
#' @param year year for which the filename should be constructed
#' @return character of filename
#'
#' @examples make_filename(2014)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Create month-year-combinations of every accident for given years
#'
#' \code{fars_read_years()} computes the month-year-combination for every
#' accident found in the files, which are specified by the given \code{years}.
#' Function constitutes a helper function for further summary statistics. Throws
#' an error if file for a given years does not exist.
#'
#' @param years year as numeric or multiple years as numeric vector
#' @importFrom dplyr mutate select
#'
#' @return returns list of month/year-combinations with seperate tibble list
#'   element for every year
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2014))
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' FARS summary by year and month
#'
#' \code{fars_summarize_years()} computes monthly summary statistics (counts of
#' accidents) for a given year or vector of \code{years}.
#'
#' @param filename path to the csv file to be read in
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return returns tidy tibble
#'
#' @examples fars_summarize_years(2013)
#' @examples fars_summarize_years(c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map FARS accident data for given state and year
#'
#' \code{fars_map_state()} makes use of several helper functions to map FARS
#' accident data. Respective data for the given year are plotted within a map of
#' the state specified by the state number \code{state.num}. Functions throws an
#' error if state number is not existent in data. Function does not return
#' anything.
#'
#' @param state.num numeric state identifier of state to be mapped
#' @param year for which the accident should be mapped
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
#'
#' @return None
#' @examples
#' fars_map_state(1, 2014)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


















