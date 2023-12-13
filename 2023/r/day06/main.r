library("data.table")

data <- fread("./input.txt", header = FALSE)
data[, V1 := NULL]
data <- transpose(data)
setnames(data, c("time", "distance"))

# Function to get the number of ways we could win
get_number_of_wins <- Vectorize(function(race_time, race_distance) {
  # Get a vector of all possible "hold the button X milliseconds"
  # Calculate the achieved distance with HOLD * (TIME - HOLD)
  # Compare with the distance value
  # Return sum of TRUEs
  holds <- seq.default(0, race_time)
  distances <- holds * (race_time - holds)
  sum(distances > race_distance)
}, vectorize.args = c("race_time", "race_distance"))

data[, wins := get_number_of_wins(time, distance)]
print(sprintf("Part 1: %f", prod(data$wins)))

# Part 2: There aren't multiple races, it's just one
# Use a bit of shell to pre-process the input quickly
data <- fread(cmd = "sed 's/[^[:digit:]]//g' ./input.txt", header = FALSE, colClasses = "numeric")
data <- transpose(data)
setnames(data, c("time", "distance"))
data[, wins := get_number_of_wins(time, distance)]
print(sprintf("Part 2: %f", prod(data$wins)))
