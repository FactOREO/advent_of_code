source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 16, session_cookie)

get_tile_value <- function(x, y, map) {
  map[[x, y]]
}

tile_is_feasible <- function(x, y, map) {
  map[[x, y]] != "#" && x > 0 && x <= nrow(map) && y > 0 && y <= ncol(map)
}

get_direction_to_tile <- function(x, y, t1, t2) {
  if (x == t1 && y < t2) {
    "East"
  } else if (x == t1 && y > t2) {
    "West"
  } else if (x < t1 && y == t2) {
    "South"
  } else {
    "North"
  }
}

calc_score_multiplier <- function(current_direction, target_direction, directions) {
  curr_idx <- which(directions == current_direction)
  trgt_idx <- which(directions == target_direction)
  min(abs(curr_idx - trgt_idx), 4 - abs(curr_idx - trgt_idx))
}

bfs_reindeer_maze <- function(start, goal, map, find_all_best_paths = FALSE) {
  # Helper variables for score-costs and directions
  direction_map <- list(
    North = c(-1, 0), East = c(0, 1), South = c(1, 0), West = c(0, -1)
  )
  # directions <- c("North", "East", "South", "West")
  directions <- c("East", "South", "West", "North")

  # Initialize the queue with the start position, score, and direction
  queue <- list(list(
    row = start[["current_row"]],
    col = start[["current_col"]],
    score = 0,
    direction = start[["current_direction"]],
    seen = matrix(c(start[["current_row"]], start[["current_col"]]), ncol = 2, byrow = TRUE)
  ))
  visited <- array(FALSE, dim = c(nrow(map), ncol(map), length(directions)))
  scores  <- array(Inf, dim = c(nrow(map), ncol(map), length(directions)))

  # Mark the initial state as visited
  start_dir_idx <- which(directions == start[["current_direction"]])
  visited[start[["current_row"]], start[["current_col"]], start_dir_idx] <- TRUE
  scores[start[["current_row"]], start[["current_col"]], start_dir_idx] <- 0

  # List of possible solutions
  solutions <- list()

  # Move through the queue
  while (length(queue) > 0) {
    # Dequeue the first element
    current <- queue[[1]]
    queue <- queue[-1]

    current_row <- current[["row"]]
    current_col <- current[["col"]]
    current_score <- current[["score"]]
    current_direction <- current[["direction"]]
    current_seen <- current[["seen"]]

    # Check if we reached the goal
    if (map[[current_row, current_col]] == goal) {
      solutions <- append(solutions, list(list(score = current_score, seen = current_seen)))
    }

    # Explore all possible directions
    for (direction in directions) {
      x <- current_row + direction_map[[direction]][[1]]
      y <- current_col + direction_map[[direction]][[2]]

      # Check if the new position is valid
      if (! tile_is_feasible(x, y, map))
        next

      # Calculate the score increment
      new_score <- current_score + 1000 * calc_score_multiplier(current_direction, direction, directions) + 1

      # Mark the direction-visited state
      dir_idx <- which(directions == direction)

      # We want the lowest score for an already seen tile, hence we should check if the
      # position was already seen and if so, only proceed if a lower score is recorded
      if (find_all_best_paths) {
        if (new_score <= scores[x, y, dir_idx]) {
          # Update the score
          scores[x, y, dir_idx] <- new_score

          # Add the new state to the queue
          queue <- append(queue, list(list(
            row = x,
            col = y,
            score = new_score,
            direction = direction,
            seen = do.call(rbind, list(current_seen, c(x, y)))
          )))
        }
        next
      }
      if (new_score < scores[[x, y, dir_idx]]) {
        # Update the score
        scores[x, y, dir_idx] <- new_score

        # Add the new state to the queue
        queue <- append(queue, list(list(
          row = x,
          col = y,
          score = new_score,
          direction = direction,
          seen = do.call(rbind, list(current_seen, c(x, y)))
        )))
      }
    }
  }
  # Return the path(s) with the lowest solution
  final_scores <- lapply(solutions, \(s) s$score) |> unlist()
  solutions[final_scores == min(final_scores)]
}

### Actual problem solving
input <- readLines("../input/input.txt")

map <- do.call(rbind, strsplit(input, ""))
reindeer <- which(map == "S", arr.ind = TRUE)

start_position <- list(
  current_row = reindeer[[1]],
  current_col = reindeer[[2]],
  current_score = 0,
  current_direction = "East"
)

tictoc::tic()
solutions <- bfs_reindeer_maze(start_position, "E", map)
print_result(2024, 16, solutions[[1]]$score)
tictoc::toc()

tictoc::tic()
solutions <- bfs_reindeer_maze(start_position, "E", map, TRUE)
# Get all unique tiles and count
p2 <- do.call(rbind, lapply(solutions, \(s) s$seen)) |> unique() |> nrow()
print_result(2024, 16, p2)
tictoc::toc()

# The result for day 16 of AOC 2024 is: 72428
# 2.354 sec elapsed

# The result for day 16 of AOC 2024 is: 456
# 14.166 sec elapsed

# Print the found optimal path
char_to_num <- c("#" = 1, "." = 2, "S" = 3, "E" = 4)
color_palette <- c("black", "white", "red", "blue")
for (i in seq.int(1, nrow(solutions[[1]]$seen))) map[[solutions[[1]]$seen[[i, 1]], solutions[[1]]$seen[[i, 2]]]] <- "S"
img_matrix <- matrix(char_to_num[map], nrow = nrow(map))
png("../output/maze_plot.png", 800, 800)
image(t(apply(img_matrix, 2, rev)), col = color_palette, axes = FALSE)
dev.off()
