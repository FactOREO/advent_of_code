source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 14, session_cookie)

input <- readLines("../input/input.txt")

robots <- input |>
  strsplit(" ") |>
  lapply(\(r) {
    r <- gsub("[pv]=", "", x = r) |>
      strsplit(",") |>
      lapply(as.integer)
    pos <- r[[1]] + 1L
    vel <- r[[2]]
    c(pos, vel)
    }
  )

MAP_WIDTH <- 101
MAP_HEIGHT <- 103
crossing <- c(ceiling(MAP_WIDTH / 2), ceiling(MAP_HEIGHT / 2))

#' Move a robot by it's velocity N times
#'
#' Naive solution to move a robot N times in the given direction,
#' shifting across walls if any is hit.
move_robot_n <- function(pos, velocity, map_boundaries = c(11, 7), n = 0, N = 100) {
  if (n == N) return(pos)
  return(move_robot_n(move_robot(pos, velocity, map_boundaries),velocity, map_boundaries, n+1, N))
}

#' Move a robot by it's velocity once
move_robot <- function(pos, velocity, map_boundaries) {
  new_X <- pos[[1L]] + velocity[[1L]]
  new_Y <- pos[[2L]] + velocity[[2L]]

  # Check if boundaries are hit
  if (new_X > map_boundaries[[1L]]) {
    new_X <- new_X - map_boundaries[[1L]]
  } else if (new_X < 1) {
    new_X <- new_X + map_boundaries[[1L]]
  }
  if (new_Y > map_boundaries[[2L]]) {
    new_Y <- new_Y - map_boundaries[[2L]]
  } else if (new_Y < 1) {
    new_Y <- new_Y + map_boundaries[[2L]]
  }
  c(new_X, new_Y)
}

# Get the final positions of each robot
tictoc::tic()
final_positions <- do.call(rbind, lapply(robots, \(robot) {
  move_robot_n(robot[1:2], robot[3:4], c(MAP_WIDTH, MAP_HEIGHT), 0, 100)
}))

robots_Q1 <- length(which(final_positions[, 1L] < crossing[[1L]] & final_positions[, 2L] < crossing[[2L]], arr.ind = TRUE))
robots_Q2 <- length(which(final_positions[, 1L] > crossing[[1L]] & final_positions[, 2L] < crossing[[2L]], arr.ind = TRUE))
robots_Q3 <- length(which(final_positions[, 1L] < crossing[[1L]] & final_positions[, 2L] > crossing[[2L]], arr.ind = TRUE))
robots_Q4 <- length(which(final_positions[, 1L] > crossing[[1L]] & final_positions[, 2L] > crossing[[2L]], arr.ind = TRUE))
p1 <- prod(robots_Q1, robots_Q2, robots_Q3, robots_Q4)
tictoc::toc()
print_result(2024, 14, p1)


check_subsequent_integers <- function(data, group_col, value_col, min_length) {
  groups <- split(data[, value_col], data[, group_col])
  # Only with enough entries
  groups <- groups[which(sapply(groups, length) >= min_length)]

  # Function to check for subsequent integers in a single group
  has_subsequent <- function(values, min_length) {
    sorted_values <- sort(unique(values))
    if (length(sorted_values) < min_length) return(FALSE)
    sum(diff(sorted_values) == 1L) >= min_length
  }

  # Check for each group if the condition is met
  any(sapply(groups, has_subsequent, min_length = min_length))
}

robots_mat <- do.call(rbind, robots)

tictoc::tic()
for (i in seq.int(1, 7350)) {
  # Save the positions at iteration k as new start points for iteration (k + 1)
  robots_mat[, 1:2] <- do.call(rbind, apply(robots_mat, 1, \(robot) {
    move_robot(c(robot[1], robot[2]), c(robot[3], robot[4]), c(MAP_WIDTH, MAP_HEIGHT))
  }, simplify = FALSE))
  # Check for horizontal line
  if (check_subsequent_integers(robots_mat, 1, 2, 30) || check_subsequent_integers(robots_mat, 2, 1, 30)) {
    p2 <- i
    out_mat <- matrix(0L, ncol = MAP_WIDTH, nrow = MAP_HEIGHT)
    for (j in seq.default(1, nrow(robots_mat))) out_mat[[robots_mat[[j, 2L]], robots_mat[[j, 1L]]]] <- 1L
    png(paste0("../output/image_", i, ".png"), width = 1200, height = 1200)
    image(
      seq.int(1, ncol(out_mat)), seq.int(1, nrow(out_mat)), t(apply(out_mat, 2, rev)), col = c("white", "black"), axes = FALSE, xlab = "", ylab = ""
    )
    title(paste("After", i, "moves"))
    dev.off()
    break
  }
}
tictoc::toc()
print_result(2024, 14, p2)

# 0.113 sec elapsed
# The result for day 14 of AOC 2024 is: 229632480

# 19.611 sec elapsed
# The result for day 14 of AOC 2024 is: 7051
