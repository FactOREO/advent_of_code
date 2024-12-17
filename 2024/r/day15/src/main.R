options(expressions = 500000)

source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 15, session_cookie)
input <- readLines("../input/input.txt")

warehouse <- do.call(rbind, strsplit(input[1:(which(input == "") - 1)], ""))

directions <- strsplit(paste(input[(which(input == "") + 1):length(input)], sep = "", collapse = ""), "")[[1]]

#' Move the robot by one field in direction
move_robot_in_warehouse <- function(robot, direction, warehouse) {
  next_position <- switch(direction,
    ">" = c(robot[[1]], robot[[2]] + 1),
    "<" = c(robot[[1]], robot[[2]] - 1),
    "^" = c(robot[[1]] - 1, robot[[2]]),
    "v" = c(robot[[1]] + 1, robot[[2]])
  )
  if (any(
    next_position == 1 |
      next_position == nrow(warehouse) |
      next_position == ncol(warehouse)
  )) return(warehouse)
  next_tile <- warehouse[[next_position[[1]], next_position[[2]]]]
  switch(next_tile,
    "#" = return(warehouse),
    "." = {
      warehouse[[robot[[1]], robot[[2]]]] <- "."
      warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
      return(warehouse)
    },
    "O" = {
      entries <- switch(direction,
        ">" = warehouse[robot[[1]], (robot[[2]] + 1):ncol(warehouse)],
        "<" = warehouse[robot[[1]], (robot[[2]] - 1):1],
        "^" = warehouse[(robot[[1]] - 1):1, robot[[2]]],
        "v" = warehouse[(robot[[1]] + 1):nrow(warehouse), robot[[2]]]
      )
      free_space <- which(entries == ".")
      first_wall <- which(entries == "#")[[1]]
      if (length(free_space) == 0 || first_wall < free_space[[1]])
        return(warehouse)
      # Move everything up to the next wall one into direction
      warehouse <- switch(direction,
        ">" = {
          for (i in seq.int(1, free_space[[1]])) {
            warehouse[[robot[[1]], robot[[2]] + (free_space[[1]] - i) + 1]] <- entries[i]
          }
          warehouse
        },
        "<" = {
          for (i in seq.int(1, free_space[[1]])) {
            warehouse[[robot[[1]], robot[[2]] - (free_space[[1]] - i) - 1]] <- entries[i]
          }
          warehouse
        },
        "^" = {
          for (i in seq.int(1, free_space[[1]])) {
            warehouse[[robot[[1]] - (free_space[[1]] - i) - 1, robot[[2]]]] <- entries[i]
          }
          warehouse
        },
        "v" = {
          for (i in seq.int(1, free_space[[1]])) {
            warehouse[[robot[[1]] + (free_space[[1]] - i) + 1, robot[[2]]]] <- entries[i]
          }
          warehouse
        }
      )
      warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
      warehouse[[robot[[1]], robot[[2]]]] <- "."
      return(warehouse)
    }
  )
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

# 0.668 sec elapsed
# The result for day 15 of AOC 2024 is: 1509863
