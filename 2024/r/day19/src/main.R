source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 19, session_cookie)

#' Check how often a Design can be combined from all towels
check_design <- function(design, towels) {
  check_design_memoize <- memoise::memoise(function(design) {
    if (design == "") {
      return(1)
    } else {
      # Sum up the results for all prefixes that match
      return(sum(sapply(towels, function(towel) {
        if (startsWith(design, towel)) {
          return(check_design_memoize(substring(design, nchar(towel) + 1)))
        } else {
          return(0)
        }
      })))
    }
  })
  return(check_design_memoize(design))
}

### Test cases
testthat::test_that("Example Case", {
  towels <- c("r", "wr", "b", "g", "bwu", "rb", "gb", "br")
  designs <- c("brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb")
  testthat::expect_equal(
    sapply(designs, check_design, towels = towels) |> sapply(\(i) i > 0) |> sum(),
    6
  )
})

testthat::test_that("Example Case", {
  towels <- c("r", "wr", "b", "g", "bwu", "rb", "gb", "br")
  designs <- c("brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb")
  testthat::expect_equal(
    sapply(designs, check_design, towels = towels) |> sum(),
    16
  )
})

### Actual Problem Solving
input <- readLines("../input/input.txt")
towels <- strsplit(input[[1]], ", ")[[1]]
designs <- input[3:length(input)]

tictoc::tic()
possible_designs <- sapply(designs, check_design, towels = towels)

p1 <- sapply(possible_designs, \(i) i > 0) |> sum()
print_result(2024, 19, p1)

p2 <- sum(possible_designs)
print_result(2024, 19, p2)
tictoc::toc()

# The result for day 19 of AOC 2024 is: 255
# The result for day 19 of AOC 2024 is: 621820080273474
# 16.245 sec elapsed
