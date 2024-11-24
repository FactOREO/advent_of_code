#' Utility functions

#' Get AOC input
#'
#' Download the input for the AOC from the web with a given AOC session identifier
#'
#' @param year The year for the AOC
#' @param day The target day as a number between 1 and 26
#'
#' @return The input file for the specified day/year combination as a string
get_aoc <- function(year = NULL, day = NULL, session_cookie = NULL) {
  year <- as.integer(year) %||% format(Sys.Date(), "%Y")
  day <- as.integer(day) %||% format(Sys.Date(), "%d")

  if (is.null(session_cookie))
    stop("\n  Session Cookie cannot be NULL!")

  if (day < 0 || day > 26)
    stop("\n  Input day has to be an integer between 1 and 26!")

  target_url <- paste0(
    "https://adventofcode.com/", year, "/day/", day, "/input"
  )

  message(sprintf(
    "Download input for day %d of AOC %d from %s\n",
    day, year, target_url
  ))

  headers <- c("Cookie" = session_cookie)
  readLines(url(target_url, headers = headers))
}

#' Print Result
#'
#' Print the result of the AOC for a given day and year
#'
#' @param year The year for the AOC
#' @param day The target day as a number between 1 and 26
#'
#' @return Message with the solution
