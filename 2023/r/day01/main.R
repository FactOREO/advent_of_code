extract_digits <- function(string, convert_spelled = FALSE) {
  digits_list <- list(
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9
  )
  out_digits  <- lapply(digits_list, \(x) stringr::str_locate_all(string, as.character(x))[[1]][, 1] |> as.vector())
  names(out_digits) <- names(digits_list)
  if (convert_spelled) {
    out_spelled <- lapply(names(digits_list), \(x) stringr::str_locate_all(string, x)[[1]][, 1] |> as.vector())
    names(out_spelled) <- names(digits_list)
    out <- setNames(mapply(c, out_spelled[names(digits_list)], out_digits[names(digits_list)]), names(digits_list))
  } else {
    out <- out_digits
  }
  out <- data.table::as.data.table(data.table::transpose(out))
  out[, c("letter", "digit") := .(names(digits_list), digits_list)]
  out <- data.table::melt.data.table(out, id.vars = c("letter", "digit")) |>
    collapse::na_omit() |>
    collapse::roworder(value)
  paste0(out[["digit"]], collapse = "", sep = "")
}

get_last_and_first_digit <- function(string) {
  str_len <- nchar(string)
  if (str_len == 1) {
    paste0(string, string)
  } else {
    paste0(substr(string, 1, 1), substr(string, str_len, str_len))
  }
}

data <- read.delim("./input_part1.txt", header = FALSE)
data[["numbers"]] <- lapply(data[["V1"]], extract_digits, TRUE)
data[["two_digit"]] <- lapply(data[["numbers"]], get_last_and_first_digit)
sprintf("The result is %d", sum(as.numeric(data[["two_digit"]])))

# [1] "The result is 54578"
