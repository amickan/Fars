#' Reads a delimited file into a tibble object.
#'
#' This function reads the contents of a compressed file in .bz2
#' format and returns them in a
#' \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble} object.
#'
#' @param filename The name of the compressed file you want to read in (a string value). 
#' You can provide the full file path rather than setting the working directory before running this function.
#' @return A tibble object with the contents of the filename.
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @section Warning:
#'    If the file does not exist, the function will show an error.
#' @examples
#' fars_read("YourComputer/FolderWithData/accident_2013.csv.bz2")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates a filename based on a specific year.
#'
#' It creates the filename string using the format of the US National Highway
#' Traffic Safety Administration in the Fatality Analysis Reporting System for
#' a specific year.
#'
#' @param year A numeric vector representing the years in a time period.
#' @return A character vector corresponding to the filenames for the specified time period
#' @export
#' @section Warning:
#'    If the \code{year} parameter cannot be coerced as an integer, an error will be thrown.
#' @examples
#' Creating the filename for year 2013:
#' make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads data from a compressed file based on a specific year, or a set of years.
#'
#' This function will read the data for a specific period, and it will
#' create a new variable (MONTH) in the dataset. Fatalities are then shown per year and month.
#'
#' @param years A numeric vector representing the years of a time period.
#' @return A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the data of car fatalities by month for each year in the
#'    specified period.
#' @export
#' @importFrom dplyr mutate select %>%
#' @section Warning:
#'    If a file does not exist for a specific year, the function will
#'    show a warning that the information was not read for that year.
#' @examples
#' # Data with the car fatalities in 2013.
#' fars_read_years(2013)
#' # Data with the car fatalities in 2013 and 2014.
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

#' Creates a summary table with the counts of car fatalities.
#'
#' This function creates a summary table with the counts of car fatalities reported
#' by the US National Highway Traffic Safety Administration for a specific period.
#' The data are shown seperately for each year in the period. 
#'
#' @inheritParams fars_read_years
#' @return A \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
#'    object with the counts of car fatalities by month and year.
#' @export
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @section Warning:
#'    If a file does not exist for a specific year, the function will
#'    throw a warning that the information cannot be accessed.
#' @examples
#' # Summary with the fatality counts in 2013.
#' fars_summarize_years(2013)
#' # Summary with the fatality counts in 2013 and 2014.
#' fars_summarize_years(c(2013, 2014))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots car fatalities in a specified US state and for a specific year.
#'
#' It plots the car fatalities that happened in a specified US state and in a
#' specific year. The map corresponds to the shape of the state, and shows where the accidents happened, geographically.
#'
#' @param state.num Number corresponding to US state. Needs to be an integer.
#' @inheritParams make_filename
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @section Warning:
#'    If the \code{state.num} parameter is not an existing state number, or the
#'    \code{year} parameter does not exist, the function will show an error.
#'    If there are no fatalities in the given year and the given state,
#'    the function will stop and state that there are no fatalities to show.
#' @examples
#' fars_map_state(12, 2015)
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
