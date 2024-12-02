path <- "day-01/input.txt"

fetch_column <- function(path, col_number) {
  data <- readr::read_lines(path) |>
    stringr::str_split("\\s+") |>
    purrr::map_chr(col_number)

  return(data)
}
one <- fetch_column(path, 1) |> as.numeric() |> sort()
two <- fetch_column(path, 2) |> as.numeric() |> sort()

three <- abs(one - two)

result <- sum(three)

glue::glue("answer: {result}")
