input <- scan("./input_test.txt", what = "list")

find_arrangements <- function(springs, damaged_groups) {
  #   ?###???????? 3,2,1
  #   .###.##.#...
  #   .###.##..#..
  #   .###.##...#.
  #   .###.##....#
  #   .###..##.#..
  #   .###..##..#.
  #   .###..##...#
  #   .###...##.#.
  #   .###...##..#
  #   .###....##.#
  # ===
  # Idea: Split the given string into all substrings given via available dots.
  #       Than split by the underscore to obtain the current number of defect springs
  #       per group.
  v <- gsub("\\.{1,}", "_", springs)
  v
}
