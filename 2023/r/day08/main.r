library("data.table")

# Function which starts at a specific node and follows the given sequence of instructions
walk_through_network <- function(network, position, sequence = c("L", "R")) {
  initial_sequence <- sequence
  steps <- 1L
  while (!endsWith(position, "Z")) {
    if (steps > length(sequence)) sequence <- c(sequence, initial_sequence)
    position <- network[node == position][[sequence[[steps]]]]
    steps <- steps + 1L
  }
  print(sprintf("Finished at position: %s", position))
  return(steps - 1L)
}

# Greatest common denominator
gcd <- function(a, b) {
  while (a != b) {
    if (a > b) a <- a - b else b <- b - a
  }
  a
}

# Least common multiple
lcm <- function(a, b) {
  abs(a * b) / gcd(a, b)
}

main <- function(file, part1, part2) {
  # Node network
  data <- fread(cmd = paste0("grep -F '=' ", file), header = FALSE, sep = "=", col.names = c("node", "instructions"))
  data[, (c("L", "R")) := tstrsplit(instructions, ",")]
  data[, (c("L", "R")) := lapply(.SD, gsub, pattern = "\\s|\\(|\\)", replacement = ""), .SDcols = c("L", "R")]
  # Sequence to move
  sequence <- readLines(file, n = 1) |> strsplit("")
  sequence <- sequence[[1]]
  ### PART 1
  # Move through the network
  if (part1) {
    steps <- walk_through_network(network = data, position = "AAA", sequence = sequence)
    print(sprintf("Part 1: Found \"ZZZ\" after %d steps", steps))
  }
  ### PART 2
  # Get all the nodes which end with "A"
  if (part2) {
    # Walk simoultaneously through all of the nodes which end with "A" and find the common number where all arive at
    # a position which ends with "Z"
    # ===
    # It is constructed in a way to use the LCM intead of actually calculating a "general" solution
    data[, start_point := endsWith(node, "A")][]
    start_points <- data[(start_point)][["node"]]
    print(sprintf("Start with positions: %s", paste(start_points, collapse = "|")))
    path_ways <- lapply(start_points, walk_through_network, network = data, sequence = sequence)
    print(sprintf("Part 2: It will take %s steps", Reduce(lcm, unlist(path_ways))))
  }
}

main("./input.txt", part1 = TRUE, part2 = TRUE)
