source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 15, session_cookie)
input <- readLines("../input/input.txt")
warehouse <- do.call(rbind, strsplit(input[1:(which(input == "") - 1)], ""))
directions <- strsplit(paste(input[(which(input == "") + 1):length(input)], sep = "", collapse = ""), "")[[1]]

#' Get next position
get_next_position <- function(x, y, direction, warehouse) {
  switch(direction,
    ">" = c(x, y + 1),
    "<" = c(x, y - 1),
    "^" = c(x - 1, y),
    "v" = c(x + 1, y)
  )
}

#' Check if position is feasible
position_is_feasible <- function(position, warehouse) {
  any(position == 1 | position == nrow(warehouse) | position == ncol(warehouse))
}

#' Get tile value
get_tile_value <- function(x, y, warehouse) {
  warehouse[[x, y]]
}

#' Get entries till wall in direction
get_entries_till_wall <- function(x, y, direction, warehouse) {
  switch(direction,
    ">" = warehouse[x, (y + 1):ncol(warehouse)],
    "<" = warehouse[x, (y - 1):1],
    "^" = warehouse[(x - 1):1, y],
    "v" = warehouse[(x + 1):nrow(warehouse), y]
  )
}

#' Reposition entries in warehouse
reposition_warehouse <- function(x, y, direction, warehouse, free_space_idx, entries) {
  switch(direction,
    ">" = {
      for (i in seq.int(1, free_space_idx)) {
        warehouse[[x, y + (free_space_idx - i) + 1]] <- entries[i]
      }
      warehouse
    },
    "<" = {
      for (i in seq.int(1, free_space_idx)) {
        warehouse[[x, y - (free_space_idx - i) - 1]] <- entries[i]
      }
      warehouse
    },
    "^" = {
      for (i in seq.int(1, free_space_idx)) {
        warehouse[[x - (free_space_idx - i) - 1, y]] <- entries[i]
      }
      warehouse
    },
    "v" = {
      for (i in seq.int(1, free_space_idx)) {
        warehouse[[x + (free_space_idx - i) + 1, y]] <- entries[i]
      }
      warehouse
    }
  )
}

# Extend the logic from move_robot_in_warehouse to handle a larger chest
move_robot_in_warehouse <- function(robot, direction, warehouse) {
  next_position <-  get_next_position(robot[[1]], robot[[2]], direction, warehouse)
  if (! position_is_feasible(next_position, warehouse))
    return(warehouse)
  next_tile <- get_tile_value(next_position[[1]], next_position[[2]], warehouse)
  if (next_tile == "#")
    return(warehouse)
  if (next_tile == "#") {
    warehouse[[robot[[1]], robot[[2]]]] <- "."
    warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
    return(warehouse)
  }
  entries <- get_entries_till_wall(robot[[1]], robot[[2]], direction, warehouse)
  free_space <- which(entries == ".")
  first_wall <- which(entries == "#")[[1]]

  if (length(free_space) == 0 || first_wall < free_space[[1]])
    return(warehouse)

  # Move everything up to the next wall one into direction
  warehouse <- reposition_warehouse(
    robot[[1]], robot[[2]], direction, warehouse, free_space[[1]], entries
  )
  # Actually move the robot
  warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
  warehouse[[robot[[1]], robot[[2]]]] <- "."
  return(warehouse)
}

tictoc::tic()
robot <- which(warehouse == "@", arr.ind = TRUE)
for (i in seq_along(directions)) {
  warehouse <- move_robot_in_warehouse(robot, directions[[i]], warehouse)
  robot <- which(warehouse == "@", arr.ind = TRUE)
}
chests <- which(warehouse == "O", arr.ind = TRUE)
p1 <- sum((chests[, 1] - 1) * 100 + chests[, 2] -  1)
tictoc::toc()
print_result(2024, 15, p1)

# 0.549 sec elapsed
# The result for day 15 of AOC 2024 is: 1509863

# Expand the map to be twice as big
large_warehouse <- do.call(rbind, apply(warehouse, 1, \(r) {
  out <- vector("character", length = 2 * length(r))
  for (i in seq_along(r)) {
    out[(2 * i - 1):(2 * i)] <- {
      if (r[[i]] == "#") {
        rep(r[[i]], 2)
      } else if (r[[i]] == "O") {
        c("[", "]")
      } else if (r[[i]] == "@") {
        c("@", ".")
      } else {
        rep(".", 2)
      }
    }
  }
  out
}, simplify = FALSE))
