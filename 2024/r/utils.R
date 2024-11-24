#' Utility functions

#' Get AOC input
#'
#' Download the input for the AOC from the web with a given AOC session identifier
#'
#' @param year The year for the AOC
#' @param day The target day as a number between 1 and 26
#'
#' @return The input file for the specified day/year combination as a string
get_aoc <- function(year = NULL, day = NULL) {
  year <- year %||% format(Sys.Date(), "%Y")
  day <- day %||% format(Sys.Date(), "%d")

  target_url <- paste0(
    "https://adventofcode.com/", year, "/day/", day, "/input"
  )

  sprintf(
    "Download input for day %d of AOC %d from %s",
    day, year, target_url
  )

  session_cookie <- readLines("../../.aoc_session")
  headers <- c("Cookie" = session_cookie)
  readLines(url(target_url, headers = headers))
}
