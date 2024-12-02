path <- "day-01/input.txt"

fetch_column <- function(path, col_number) {
  data <- readr::read_lines(path) |>
    stringr::str_split("\\s+") |>
    purrr::map_chr(col_number)

  return(data)
}

one <- fetch_column(path, 1) |> as.numeric()
two <- fetch_column(path, 2) |> as.numeric()
result_vector <- numeric()

for (i in seq_along(one)) {
  ith_count <- two[two == one[i]] |> length()

  result_vector <- c(result_vector, ith_count * one[i])
}

result <- sum(result_vector)

glue::glue("answer: {result}")
