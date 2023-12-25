solve_part1 <- function(map) {

}

solve_part2 <- function() {

}

main <- function(file) {
  input <- apply(do.call(rbind, strsplit(readLines(file), "")), 1, as.integer)
  input
}

map <- main("./input_test.txt")
