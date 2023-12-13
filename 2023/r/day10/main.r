library("data.table")
args <- commandArgs(trailingOnly = TRUE)

input <- fread(args[[1]], header = FALSE, colClasses = "character", sep = "A")
input[, (paste0("pos", seq.int(1L, nchar(input[["V1"]][[1L]])))) := tstrsplit(V1, "")]
input[, V1 := NULL]

labyrinth <- array(transpose(input))

# find the starting position
x <- which(unlist(lapply(labyrinth, function(x) any(x == "S"))))
y <- which(labyrinth[[x]] == "S")
around <- vector("character", length = 4L)
names(around) <- c("left", "right", "above", "below")
x_max <- length(labyrinth)
y_max <- length(labyrinth[[x]])
direction <- ""
pipes <- 0L
# For part 2: positions at any given point
positions <- data.table()

while (TRUE) {
  # Start at the current position (x,y) and check all four tiles around
  # Idea:
  # If we are around the full loop, there is an odd or even number of pipes passed
  # If it is an even number there would be 2 tiles evenly far away
  # If it is an odd number like in the example, there is one tile evenly far away
  # In both cases we can use ceiling(pipes / 2) to calculate the answer
  # ===
  # Get all pipes around the current position
  current_pipe <- labyrinth[[x]][[y]]
  positions <- rbindlist(list(positions, list(x, y, current_pipe)), use.names = FALSE)

  # print(sprintf("Current position: (%s, %s) at %s", x, y, current_pipe))
  if ((x - 1) == 0 || direction == "below") {
    around[["above"]] <- ";"
  } else {
    around[["above"]] <- labyrinth[[x - 1]][[y]]
  }
  if ((x + 1) > x_max || direction == "above") {
    around[["below"]] <- ";"
  } else {
    around[["below"]] <- labyrinth[[x + 1]][[y]]
  }
  if ((y - 1) == 0 || direction == "right") {
    around[["left"]] <- ";"
  } else {
    around[["left"]] <- labyrinth[[x]][[y - 1]]
  }
  if ((y + 1) > y_max || direction == "left") {
    around[["right"]] <- ";"
  } else {
    around[["right"]] <- labyrinth[[x]][[y + 1]]
  }
  # Matching characters:
  #   - | above or below
  #   - F above or left
  #   - J below or right
  #   - - right or left
  #   - 7 above or right
  #   - L below or left
  # Additionally: Keep track from which direction we came to avoid moving backwards
  # Move one step into one direction
  if (around[["below"]] %in% c("J", "|", "L") && !(current_pipe %in% c("L", "J", "-"))) {
    x <- x + 1L
    direction <- "below"
  } else if (around[["above"]] %in% c("7", "|", "F") && !(current_pipe %in% c("-", "F", "7"))) {
    x <- x - 1L
    direction <- "above"
  } else if (around[["right"]] %in% c("-", "7", "J") && !(current_pipe %in% c("7", "J", "|"))) {
    y <- y + 1
    direction <- "right"
  } else if (around[["left"]] %in% c("L", "-", "F") && !(current_pipe %in% c("|", "F", "L"))) {
    y <- y - 1
    direction <- "left"
  } else {
    print(sprintf("The farest pipe in the loop is %d away", ceiling(pipes / 2)))
    break
  }
  pipes <- pipes + 1L
}

# Part 2: Find all encapsulated tiles (pipes or ground)
# Apply Pick's formula and the shoelace formula to find the area
# https://www.reddit.com/r/adventofcode/comments/18f1sgh/comment/kcugm6t/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

# Pick's formula: i = A - b / 2 - h + 1
#                 ^   ^   ^       ^
#                 |   |   |       |
#     number of tiles enclosed  holes in the polygon (here: 0)
#                     |   |
#                 area of the polygon
#                         |
#                     perimeter of the polygon

# Shoelace formula
# A = 1 / 2 \cdot sum_{i = 1}^{n} (y_i + y_{i + 1}) \cdot (x_i - x_{i + 1})
# ^
# |
# Area of the polygon

# Since n is the number of points, and conveniently P_{n + 1} == P_{1}, append first entry
x <- c(positions[[1]], positions[[1]][[1]])
y <- c(positions[[2]], positions[[2]][[1]])
max_x <- length(x)
max_y <- length(y)

A <- abs(0.5 * sum((y[1:max_y - 1] + y[2:max_y]) * (x[1:max_x - 1] - x[2:max_x])))
b <- ceiling(pipes / 2) * 2
i <- A - b / 2 - 0 + 1
print(sprintf("A: %s|b: %s|i: %s", A, b, i))

