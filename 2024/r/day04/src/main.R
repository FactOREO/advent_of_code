source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 4, session_cookie)
input <- readLines("../input/input.txt")

word_puzzle <- matrix(
  strsplit(input, "") |> unlist(),
  ncol=nchar(input[1]),
  nrow=length(input),
  byrow=TRUE
)

#' Recursively check for string XMAS in word puzzle
#'
#' This function checks either all possible surrounding tiles or only the ones in the correct direction for the next
#' letter in the word puzzle for the string "XMAS".
#'
#' @param row The current row
#' @param col The current column
#' @param mc Max col
#' @param mc Max row
#' @param curr_letter Current letter in the string XMAS
#' @param word_puzzle A matrix containing one of the letters X, M, A or S per entry
#' @param direction Integer between 1 and 9, indicating the index for the direction search or NULL if all tiles should
#'  be considered for the search
#'
#' @return TRUE or FALSE if the string has been completed beginning from the initial position
check_xmas <- function(row, col, mc, mr, curr_letter, word_puzzle, direction = NULL) {
  # Base Case
  if (curr_letter == "S") return(TRUE)
  next_letter <- switch(curr_letter, "X" = "M", "M" = "A", "A" = "S")
  new_positions <- integer(0)
  search_grid <- cbind(cbind(rep(-1L:1L, each = 3L), rep(-1L:1L, 3L)), 1:9)
  if (!is.null(direction)) {
    search_grid <- search_grid[direction, ]
  }
  check_neighbour <- function(pos, row, col, next_letter, word_puzzle) {
    nr <- row + pos[[1L]]; nc <- col + pos[[2L]]
    if (nc == 0L || nc > mc || nr == 0L || nr > mr || (pos[[1L]] == row && pos[[2L]] == col)) return(NULL)
    # Outside the bad cases, we can return nr, nc as matrix if we have a match
    if (word_puzzle[[nr, nc]] == next_letter)
      c(nr, nc, pos[[3L]])
  }
  # Go through all search_grid rows
  if (is.null(dim(search_grid))) {
    new_positions <- check_neighbour(search_grid, row, col, next_letter, word_puzzle)
    if (is.null(new_positions)) return(FALSE) else new_positions <- matrix(new_positions, ncol = 3L)
  } else {
    new_positions <- apply(search_grid, check_neighbour, MARGIN=1, row, col, next_letter, word_puzzle)
    if (is.null(new_positions)) return(FALSE) else new_positions <- do.call(rbind, args = new_positions)
  }
  # Recursive apply to the new_positions
  apply(new_positions, \(x) { check_xmas(x[[1L]], x[[2L]], mc, mr, next_letter, word_puzzle, x[[3L]])}, MARGIN=1L, simplify = FALSE)
}

start_points <- which(word_puzzle == "X", arr.ind=TRUE)

tictoc::tic()
p1 <- apply(start_points, \(x) {check_xmas(row = x[[1L]], col = x[[2L]], mc = ncol(word_puzzle), mr = nrow(word_puzzle), curr_letter = "X", word_puzzle = word_puzzle, direction = NULL)}, MARGIN = 1) |> unlist() |> sum()
tictoc::toc()

print_result(2024, 4, p1)
# 0.647 sec elapsed
# The result for day 4 of AOC 2024 is: 2483


# Part 2
# Modify check_xmas to search for diagonal M - S entries with start points equal to A

#' Check X-MAS cross in word puzzle
#'
#' Function to check if opposing diagonal entries relative to the start row and col contain "M" and "S" respectively
#' for both axes.
#'
#' @param row The current row
#' @param col The current column
#' @param mc Max col
#' @param mc Max row
#' @param word_puzzle A matrix containing one of the letters X, M, A or S per entry
#'
#' @return Bool TRUE if the cross can be completed, FALSE otherwise
check_x_mas <- function(row, col, mc, mr, word_puzzle) {
  # Only diagonals matter
  search_grid <- matrix(c(-1L, -1L, 1L, 1L, -1L, 1L, 1L, -1L), byrow = TRUE, ncol = 2)
  # Check for surrounding M and S
  check_neighbour <- function(pos, row, col, word_puzzle) {
    nr <- row + pos[[1L]]; nc <- col + pos[[2L]]
    if (nc == 0L || nc > mc || nr == 0L || nr > mr || (pos[[1L]] == row && pos[[2L]] == col)) return(NULL)
    # Outside the bad cases, we can return nr, nc as matrix if we have a match
    if (word_puzzle[[nr, nc]] == "M") c(pos[[1L]], pos[[2L]], 0L) else if (word_puzzle[[nr, nc]] == "S") c(pos[[1L]], pos[[2L]], 1L)
  }
  # Apply check_neighbour to all diagonal entries
  matches <- apply(
    search_grid,
    check_neighbour,
    MARGIN = 1,
    row = row, col = col, word_puzzle = word_puzzle
  ) |> unlist()
  if (!is.null(matches)) matches <- matrix(matches, ncol = 3, byrow = TRUE) else return(FALSE)
  # Check if we have 4 matches for a cross
  if (nrow(matches) != 4L)
    return(FALSE)# Check if the indices upper left - lower right and upper right - lower left are S and M
  if (matches[[1L, 3L]] != matches[[2L, 3L]] && matches[[3L, 3L]] != matches[[4L, 3L]])
    return(TRUE)
}

tictoc::tic()
start_points <- which(word_puzzle == "A", arr.ind=TRUE)
p2 <- apply(start_points, \(x) { check_x_mas(x[[1L]], x[[2L]], nrow(word_puzzle), ncol(word_puzzle), word_puzzle)}, MARGIN = 1) |> unlist() |> sum()
tictoc::toc()

print_result(2024, 4, p2)
# 0.227 sec elapsed
# The result for day 4 of AOC 2024 is: 1925
