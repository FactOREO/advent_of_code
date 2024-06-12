#!/usr/bin/Rscript
### Strategy
# remotes::install_github("machow/astar-r")
# Needs download of datastructures from CRAN archive
args <- commandArgs(trailingOnly = TRUE)

# Given a "." in the input, find a way with exactly n steps to this field if possible.
# Go through all tiles reachable within a manhattan distance of 64 to avoid checking irrelevant fields.
# Component A: Find the shortest distance
# Component B: If the remainder is even, the field is reachable, else not.
is_reachable <- function(map, start, end, steps) {
  # Every field with manhattan distance > 64 is also infeasible
  if (manhattan_distance(start[[1]], start[[2]], end[[1]], end[[2]]) > steps + 5) return(FALSE)
  if (map[[start[[1L]], start[[2L]]]] == 1L) return(FALSE)
  if (map[[end[[1L]], end[[2L]]]] == 1L) return(FALSE)
  mg <- astar::MazeGazer$new(map)
  print(sprintf("Find path from (%s) to (%s)",
                paste(start, collapse = ","),
                paste(end, collapse = ",")))
  opt_path_length <- tryCatch(
    expr = length(mg$run(as.numeric(start), as.numeric(end))) - 1L,
    error = function(e) { return(1L) }
  )
  rm(mg)
  print(sprintf("Opt path length: %d", opt_path_length))
  if (opt_path_length > steps || opt_path_length == 1L) return(FALSE)
  if (opt_path_length == 0L || (steps - opt_path_length) %% 2 == 0L) return(TRUE)
  FALSE
}

manhattan_distance <- function(x1, y1, x2, y2) {
  abs(x1 - x2) + abs(y1 - y2)
}

main <- function(file, init_steps) {
  # Prepare
  input <- readLines(file) |> strsplit("")
  input <- matrix(unlist(input), nrow = length(input), byrow = TRUE)
  start_row <- which(apply(input, 1, \(x) { pos <- which(x == "S"); pos }) != 0L)
  start_col <- which(apply(input, 2, \(x) { pos <- which(x == "S"); pos }) != 0L)
  input[which(input %in% c("S", "."))] <- 0
  input[which(input == "#")] <- 1
  input <- matrix(as.integer(input), ncol = ncol(input))
  # Apply Part 1
  pos_mat <- which(input == 0L, arr.ind = TRUE)
  cl <- parallel::makeCluster(parallel::detectCores() - 4)
  parallel::clusterExport(cl = cl, varlist = c("input", "start_col", "start_row", "init_steps"), envir = environment())
  parallel::clusterExport(cl = cl, varlist = c("is_reachable", "manhattan_distance"))
  res <- parallel::parApply(cl, pos_mat, 1, \(x) is_reachable(input, c(start_row, start_col), c(x[[1]], x[[2]]), init_steps))
  # res <- apply(pos_mat, 1, \(x) is_reachable(input, c(start_row, start_col), c(x[[1]], x[[2]]), init_steps))
  parallel::stopCluster(cl)
  print(sprintf("Part 1: %d", sum(res)))
}

main(file = args[[1L]], init_steps = as.numeric(args[[2L]]))

## OLD APPROACH - NOT FEASIBLE FOR LARGE INPUT
# Given a number of steps, find all "." in the input which can be reached by constructing pathways.
# Only "." can be chosen as target for the next move, "#" indicate stones.
walk_through_garden <- function(garden, position, steps_left = 1L, steps_done = 0L, seen = list()) {
  # print(sprintf("Current position: %s | Left steps: %d", paste(position, collapse = "x"), steps_left))
  if (steps_left == 0L) {
    len_seen <- length(seen)
    seen[[len_seen + 1L]] <- position
    return(seen)
  }
  # Check possible directions
  directions <- check_directions(garden, position)
  # print(sprintf("Found possible directions: %s", paste(directions, collapse = "|")))
  # Invoke function again with 1 step less for every direction
  for (direction in directions) {
    # print(sprintf("Invoke walkthrough for direction %s", direction))
    # Get the new position
    if (direction == "E") new_position <- c(position[[1L]], position[[2L]] + 1L)
    if (direction == "S") new_position <- c(position[[1L]] + 1L, position[[2L]])
    if (direction == "W") new_position <- c(position[[1L]], position[[2L]] - 1L)
    if (direction == "N") new_position <- c(position[[1L]] - 1L, position[[2L]])
    # Invoke the function
    seen <- walk_through_garden(garden,
                                new_position,
                                steps_left - 1L,
                                steps_done + 1L,
                                seen)
  }
  return(seen)
}

# Return vector of possible directions
check_directions <- function(garden, position) {
  directions <- vector("character")
  if (position[[1L]] - 1L > 0L && garden[[position[[1L]] - 1L, position[[2L]]]] != "#") directions <- c(directions, "N")
  if (position[[1L]] + 1L <= nrow(garden) && garden[[position[[1L]] + 1L, position[[2L]]]] != "#") directions <- c(directions, "S")
  if (position[[2L]] - 1L > 0L && garden[[position[[1L]], position[[2L]] - 1L]] != "#") directions <- c(directions, "W")
  if (position[[2L]] + 1L <= ncol(garden) && garden[[position[[1L]], position[[2L]] + 1L]] != "#") directions <- c(directions, "E")
  directions
}

# All reachable positions with every amount of steps until the given number of steps
# seen_positions <- walk_through_garden(input, c(start_row, start_col), init_steps)
# print(sprintf("Reachable position: %d", length(unique(seen_positions))))
