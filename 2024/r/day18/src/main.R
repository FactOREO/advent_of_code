source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 18, session_cookie)

# Use the BFS from day 16
tile_is_feasible <- function(x, y, map) {
  x > 0 && x <= nrow(map) && y > 0 && y <= ncol(map) && map[[x, y]] != "#"
}

bfs <- function(start, goal, map) {
  # Possible Directions
  direction_map <- list(
    North = c(-1, 0), East = c(0, 1), South = c(1, 0), West = c(0, -1)
  )

  # Initialize the queue with the start position and an empty seen matrix
  queue <- list(list(
    row = start[[1]],
    col = start[[2]],
    seen = matrix(c(start[[1]], start[[2]]), ncol = 2, byrow = TRUE)
  ))

  # Visited Tiles go here
  visited <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
  visited[[start[[1]], start[[2]]]] <- TRUE

  # List of possible solutions
  solutions <- NULL

  while (length(queue) > 0) {
    # Dequeue the first element
    current <- queue[[1]]
    queue <- queue[-1]

    # Current Values
    current_row <- current[["row"]]
    current_col <- current[["col"]]
    current_seen <- current[["seen"]]

    # Check if we reached the goal
    if (map[[current_row, current_col]] == goal) {
      solutions <- append(solutions, list(list(seen = current_seen)))
    }

    # Explore all possible directions
    for (direction in names(direction_map)) {
      x <- current_row + direction_map[[direction]][[1]]
      y <- current_col + direction_map[[direction]][[2]]

      # Check if the new position is valid
      if (! tile_is_feasible(x, y, map))
        next

      if (!visited[[x, y]]) {
        visited[[x, y]] <- TRUE
        # Add the new state to the queue
        queue <- append(queue, list(list(
          row = x,
          col = y,
          seen = do.call(rbind, list(current_seen, c(x, y)))
        )))
      }
    }
  }
  solutions
}

# Construct the Map with obstacles
construct_obstacles_map <- function(map, falling_bytes, n_falling = 12) {
  if (n_falling > nrow(falling_bytes)) n_falling <- nrow(falling_bytes)
  for (i in seq.default(1, n_falling)) {
    map[[falling_bytes[[i, 1]] + 1, falling_bytes[[i, 2]] + 1]] <- "#"
  }
  map[[1, 1]] <- "S"
  map[[nrow(map), ncol(map)]] <- "E"
  map
}

# Use binary search for part 2
# We have 3450 falling bytes instructions and need to find the two bytes, where first instruction
# is still possible, but second is not
binary_search <- function(base_map, falling_bytes) {
  len_bytes <- nrow(falling_bytes)
  i <- 0
  min_idx <- 1
  while (min_idx <= len_bytes && i <= 10) {
    i <- i + 1
    idx <- floor((min_idx + len_bytes) / 2)
    s1 <- bfs(c(1, 1), "E", construct_obstacles_map(base_map, falling_bytes, idx))
    s2 <- bfs(c(1, 1), "E", construct_obstacles_map(base_map, falling_bytes, idx + 1))
    if (!is.null(s1) && is.null(s2)) {
      return(falling_bytes[idx + 1, ])
    } else if (is.null(s1)) {
      # No solution found for idx -> solution is smaller -> ignore right half
      len_bytes <- idx - 1
    } else {
      # Solution found for idx -> solution is greater -> ignore left half
      min_idx <- idx + 1
    }
  }
}

### Test Cases
testthat::test_that("Example", {
  map <- matrix(c(
    "S", ".", ".", ".", ".", ".", "#",
    ".", ".", ".", ".", ".", "#", ".",
    ".", "#", ".", ".", "#", ".", "#",
    "#", ".", ".", "#", ".", ".", ".",
    ".", ".", "#", ".", ".", "#", ".",
    ".", "#", ".", ".", "#", ".", ".",
    ".", ".", ".", "#", ".", ".", "E"
  ), ncol = 7, byrow = TRUE)
  testthat::expect_equal(
    bfs(c(1, 1), "E", map)[[1]]$seen |> nrow() - 1,
    22
  )
})

testthat::test_that("Example Part 2", {
  base_map <- matrix(".", nrow = 7, ncol = 7)
  falling_bytes <- matrix(c(
    5, 4, 4, 2, 4, 5, 3, 0, 2, 1, 6, 3, 2, 4, 1, 5, 0, 6, 3, 3,
    2, 6, 5, 1, 1, 2, 5, 5, 2, 5, 6, 5, 1, 4, 0, 4, 6, 4, 1, 1,
    6, 1, 1, 0, 0, 5, 1, 6, 2, 0
  ), ncol = 2, byrow = TRUE)
  testthat::expect_equal(
    binary_search(base_map, falling_bytes),
    c(6, 1)
  )
})

# ENV Variables for map height and width
MAP_HEIGHT <- 71
MAP_WIDTH <- 71

base_map <- matrix(".", nrow = MAP_HEIGHT, ncol = MAP_WIDTH)
input <- readLines("../input/input.txt")
falling_bytes <- do.call(rbind, lapply(input, \(i) strsplit(i, split = ",")[[1]] |> as.integer()))

map <- construct_obstacles_map(base_map, falling_bytes, n_falling = 1024)

tictoc::tic()
p1 <- bfs(c(1, 1), "E", map)[[1]]$seen |> nrow() - 1
tictoc::toc()

print_result(2024, 18, p1)

tictoc::tic()
p2 <- binary_search(base_map, falling_bytes) |> paste(collapse = ",")
tictoc::toc()

print_result(2024, 18, p2)

# 0.082 sec elapsed
# The result for day 18 of AOC 2024 is: 294

# 0.196 sec elapsed
# The result for day 18 of AOC 2024 is: 31,22
