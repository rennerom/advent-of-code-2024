path <- "day-03/input.txt"

data <- 
  readr::read_lines(path) |>
  paste0(collapse = "") # make sure it's a single string

data |> 
  # luckily these patterns aren't in the rental shop's computers memory!
  stringr::str_replace_all("don't", "STOP_STOP") |> # this has to go first
  stringr::str_replace_all("do", "START_START") |>
  stringr::str_extract_all("^.*?(?=STOP_STOP)|(?<=START_START).*?(?=STOP_STOP)|(?<=START_START).*?$") |> 
  paste0(collapse = "") |>  # make sure it's a single string again
  stringr::str_extract_all("mul\\(\\d{1,3},\\d{1,3}\\)") |> # basic regex patern
  purrr::flatten() |> # give each match its own spot in the list
  purrr::map(
    ~stringr::str_extract_all(.x, "\\d{1,3}") |>  # get just the digits pairs
      purrr::flatten() |> # convert each match to a list of paired digits
      purrr::map_dbl(~as.numeric(.x)) |>
      purrr::reduce(`*`) # times 'em
  ) |> 
  purrr::reduce(`+`) -> result # add 'em up

glue::glue("answer: {result}")
