source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 10, session_cookie)
input <- readLines("../input/input.txt")

trail_map <- do.call(rbind, lapply(strsplit(input, ""), as.integer))

check_trail_path <- function(row, col, mc, mr, curr_height, trail_map) {
  if (curr_height == 9L) return(c(row, col))
  next_step     <- curr_height + 1L
  search_grid   <- matrix(c(-1L, 0L, 0L, 1L, 1L, 0L, 0L, -1L), ncol = 2, byrow = TRUE)
  check_neighbour <- function(pos, row, col, next_step, trail_map) {
    nr <- row + pos[[1L]]
    nc <- col + pos[[2L]]
    if (nc == 0L || nc > mc || nr == 0L || nr > mr || (pos[[1L]] == row && pos[[2L]] == col)) return(NULL)
    if (trail_map[[nr, nc]] == next_step)
      c(nr, nc)
  }
  new_positions <- apply(search_grid, 1, check_neighbour, row, col, next_step, trail_map, simplify = FALSE)
  if (all(lapply(new_positions, is.null))) return() else new_positions <- do.call(rbind, args = new_positions)
  # Recursive apply to the new_positions
  apply(new_positions, \(x) { check_trail_path(x[[1L]], x[[2L]], mc, mr, next_step, trail_map)}, MARGIN=1L, simplify = FALSE)
}

start_positions <- which(trail_map == 0L, arr.ind = TRUE)

tictoc::tic()
mapply(\(row, col, mc, mr, curr_height, trail_map) {
  target_heights <- check_trail_path(row, col, mc, mr, curr_height, trail_map) |>
    unlist() |>
    matrix(byrow = TRUE, ncol = 2) |> # Matrix of all reachable paths per "0" beginning position
    unique() |>
    nrow()
  },
  row = start_positions[, 1L],
  col = start_positions[, 2],
  MoreArgs = list(
    mc = ncol(trail_map),
    mr = nrow(trail_map),
    curr_height = 0L,
    trail_map = trail_map
  )
) |>
  sum() |>
  print_result(2024, 10, solution = _)
tictoc::toc()

tictoc::tic()
mapply(\(row, col, mc, mr, curr_height, trail_map) {
  target_heights <- check_trail_path(row, col, mc, mr, curr_height, trail_map) |>
    unlist() |>
    matrix(byrow = TRUE, ncol = 2) |>
    # No unique end points required, only distinct pathways
    nrow()
  },
  row = start_positions[, 1L],
  col = start_positions[, 2],
  MoreArgs = list(
    mc = ncol(trail_map),
    mr = nrow(trail_map),
    curr_height = 0L,
    trail_map = trail_map
  )
) |>
  sum() |>
  print_result(2024, 10, solution = _)
tictoc::toc()


# The result for day 10 of AOC 2024 is: 698

# 0.467 sec elapsed
# The result for day 10 of AOC 2024 is: 1436

# 0.444 sec elapsed
