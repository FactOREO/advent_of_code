# Strategy: Given a position and direction, determine what the next tile is. Based on this information, decide what
#           should happen next. If there is air, pass through. If there is a reflector, reflect to the correct
#           direction. If a splitter is hit, invoke the same function again twice with the correct directions.
#           In this case exit the current function with all hit tiles.
guide_beam <- function(cave, visited, position, direction, ctr = 1L) {
  ### PRE CHECK
  # Check the tile at the input position and determine the correct direction to guide
  # Reflector should reflect with respect to the given input direction, splitter should default to "down"
  if (found_splitter(tile = cave[[position[[1L]], position[[2L]]]], direction = direction)) {
    directions <- get_directions(tile = cave[[position[[1L]], position[[2L]]]], direction = direction, splitter = TRUE)
    for (new_direction in directions) {
      # print(sprintf("Try to move %s from position (%-5s)", new_direction, paste(position, collapse = ", ")))
      if (get_tile(cave, move(position = position, direction = new_direction)) != "X") {
        visited <- guide_beam(cave, visited, position, new_direction, ctr = ctr + 1L)
      } else {
        # print(sprintf("[[%d]] | Exit function because new direction from split is a wall in direction %s", ctr, new_direction))
        next
      }
    }
  } else {
    direction <- get_directions(tile = cave[[position[[1L]], position[[2L]]]], direction = direction, splitter = FALSE)
  }
  ### Recursive Logic
  while (TRUE) {
    # print(sprintf("[[%d]] | Cur position: (%-5s) | Tile: '%-1s' | Direction: %s", ctr, paste(position, collapse = ", "), get_tile(cave, position), direction))
    # Update the visited matrix first
    if (is.null(visited[[position[[1L]], position[[2L]]]])) {
      # If at the given position there is no direction, this tile was not visited beforehand
      # -> add current direction as first entry
      # -> proceed
      visited[[position[[1L]], position[[2L]]]] <- list(direction)
    } else if (!is.null(visited[[position[[1L]], position[[2L]]]]) && direction %in% visited[[position[[1L]], position[[2L]]]]) {
      # If the current direction was already present at the current tile, we were here before
      # -> exit the loop
      # print(sprintf("[[%d]] | Exit function because of visited position (%-5s) from direction %s", ctr, paste(position, collapse = ", "), direction))
      break
    } else {
      # If there are directions present but not the current one
      # -> add the current direction at the current position
      # -> proceed
      visited[[position[[1L]], position[[2L]]]] <- c(visited[[position[[1L]], position[[2L]]]], direction)
    }
    if (found_splitter(get_tile(cave, position), direction)) {
      ### Check if we are on a splitter
      # If a splitter was encountered, call new beams
      directions <- get_directions(get_tile(cave, position), direction = direction, splitter = TRUE)
      # print(sprintf("[[%d]] | Detected splitter '%s' with new directions: [%s]", ctr, get_tile(cave, position), paste(directions, collapse = ",")))
      # Iteratively go through the new beams
      for (new_direction in directions) {
        # print(sprintf("Try to move %s from position (%-5s)", new_direction, paste(position, collapse = ", ")))
        # Only call the next beam, if we don't hit a wall
        if (get_tile(cave, move(position = position, direction = new_direction)) != "X") {
          visited <- guide_beam(cave, visited, position, new_direction, ctr = ctr + 1L)
        } else {
          # If we hit a wall, go to the next direction (if any is left)
          # print(sprintf("[[%d]] | Exit function because new direction from split is a wall in direction %s", ctr, new_direction))
          next
        }
      }
    } else if (check_next_tile(cave, position, direction) == "X") {
      ### Check if the next tile is a wall, if we are not on a splitter
      # print(sprintf("[[%d]] | Exit function because next tile is a wall in direction %s", ctr, direction))
      break
    } else {
      # If we can pass through, update the position and the direction
      position <- move(position, direction)
      direction <- get_directions(tile = get_tile(cave, position), direction = direction)
      # print(sprintf("[[%d]] | New position: (%-5s) | Tile: '%-1s' | Direction: %s", ctr, paste(position, collapse = ", "), get_tile(cave, position), direction))
    }
  }
  return(visited)
}

# Function to determine if the next tile would be a wall.
# Returns the next tile character or an X in case of a wall.
check_next_tile <- function(cave, position, direction) {
  switch(direction,
    "left"  = if (position[[2L]] - 1L != 0L) cave[[position[[1L]], position[[2L]] - 1L]] else "X",
    "right" = if (position[[2L]] + 1L <= ncol(cave)) cave[[position[[1L]], position[[2L]] + 1L]] else "X",
    "up"  = if (position[[1L]] - 1L != 0L) cave[[position[[1L]] - 1L, position[[2L]]]] else "X",
    "down"  = if (position[[1L]] + 1L <= nrow(cave)) cave[[position[[1L]] + 1L, position[[2L]]]] else "X"
  )
}

# Function to determine the current cave tile.
# Returns the tile at the given position or "X" if a wall is hit.
get_tile <- function(cave, position) {
  tryCatch(cave[[position[[1L]], position[[2L]]]], error = \(cond) "X")
}

# Function to find out, if based on the current tile and direction a splitter is hit.
# Returns a boolean.
found_splitter <- function(tile, direction) {
  if (tile == "|" && direction %in% c("left", "right")) {
    return(TRUE)
  } else if (tile == "-" && direction %in% c("up", "down")) {
    return(TRUE)
  }
  FALSE
}

move <- function(position, direction) {
  switch(direction,
    "left"  = c(position[[1L]], position[[2L]] - 1L),
    "right" = c(position[[1L]], position[[2L]] + 1L),
    "up"  = c(position[[1L]] - 1L, position[[2L]]),
    "down"  = c(position[[1L]] + 1L, position[[2L]]),
  )
}

# Function to get the next direction(s) based on the current tile and direction.
# Returns either a scalar or a vector of length two, if a splitter (dependent on the current direction) was hit.
get_directions <- function(tile, direction, splitter = FALSE) {
  switch(tile,
    "|" = if (splitter) c("up", "down") else direction,
    "-" = if (splitter) c("left", "right") else direction,
    "\\" = switch(direction,
      "left" = "up", "right" = "down", "up" = "left", "down" = "right"
    ),
    "/" = switch(direction,
      "left" = "down", "right" = "up", "up" = "right", "down" = "left"
    ),
    direction
  )
}

# Function for debugging purposes, converts NULL to 0 and all others to the length of the entry.
convert_null_matrix <- function(matrix_with_nulls) {
  matrix(lapply(matrix_with_nulls, \(x) if (is.null(x)) return(0L) else return(length(x))),
     nrow = nrow(matrix_with_nulls), ncol = ncol(matrix_with_nulls))
}

# Take a cave as input and return all possible start positions in the cave as well as their directions.
get_start_positions <- function(cave) {
  num_rows <- nrow(cave)
  num_cols <- ncol(cave)
  start_positions <- list()
  start_positions$right <- cbind(seq_len(num_rows), rep(1, num_rows))
  start_positions$left  <- cbind(seq_len(num_rows), rep(num_cols, num_rows))
  start_positions$down  <- cbind(rep(1, num_cols), seq_len(num_cols))
  start_positions$up    <- cbind(rep(num_rows, num_cols), seq_len(num_cols))
  return(start_positions)
}

# Take a cave (matrix with characters '.', '\', '|', '-' and '/'), a list with start positions where the names of the
# list elements correspond to the starting direction ('left', 'right', 'up' or 'down') and a initial matrix to keep
# track of visited tiles.
# Returns all result matrices from the start_positions list.
apply_guide_beam <- function(cave, start_positions, visited) {
  lapply(names(start_positions), function(direction) {
    sapply(seq_len(nrow(start_positions[[direction]])), function(i) {
      guide_beam(cave = cave, visited = visited, position = start_positions[[direction]][i, ], direction = direction)
    }, simplify = FALSE)
  })
}

solve_part1 <- function(cave, visited) {
  position <- c(1L, 1L)
  direction <- "right"
  start_time <- Sys.time()
  result <- guide_beam(cave, position = position, direction = direction, visited = visited)
  end_time <- Sys.time()
  print(sprintf("Part 1: %d | Solved in %.2f seconds",
                sum(lengths(result) > 0), difftime(end_time, start_time, units = "secs")))
}

solve_part2 <- function(cave, visited) {
  # Get a list with entries = matrices with positions. Each matrix entry corresponds to one direction.
  start_positions <- get_start_positions(cave)
  start_time <- Sys.time()
  result <- apply_guide_beam(cave = cave, start_positions = start_positions, visited = visited)
  # Calculate the sum of energized tiles in all result lists.
  energized_tiles <- lapply(result, function(result_list) {
    lapply(result_list, function(result) {
      sum(lengths(result) > 0)
    })
  })
  end_time <- Sys.time()
  print(sprintf("Part 2: %d | Solved in %.2f seconds",
                max(unlist(energized_tiles)), difftime(end_time, start_time, units = "secs")))
}

# Function to solve both parts of the task.
main <- function(file) {
  ### initialize variables and input
  input <- readLines(file)
  cave <- matrix(unlist(strsplit(input, "")), nrow = length(input), byrow = TRUE)
  visited <- matrix(vector("list", length = 4L), nrow = nrow(cave), ncol = ncol(cave))
  ### PART 1
  solve_part1(cave, visited)
  ### PART 2
  solve_part2(cave, visited)
}

main("./input.txt")
# [1] "Part 1: 6740 | Solved in 0.47 seconds"
# [1] "Part 2: 7041 | Solved in 105.30 seconds"
