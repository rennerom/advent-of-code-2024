box::use(R/aoc [fetch_raw_input_data])

input <- fetch_raw_input_data(2024, 25) |>
  stringr::str_split("\n") |>
  unlist() |>
  purrr::reduce(
    .init = list(list()),
    ~ if (.y == "") {
      append(.x, list(list()))
    } else {
      # append to existing, don't create a new list
      .x[[length(.x)]] <- append(.x[[length(.x)]], .y)
      .x
    }
  ) |>
  purrr::discard(~ length(.x) == 0)

keys <- list()
locks <- list()

for (i in seq_along(input)) {
  code <- input[[i]] |> 
    purrr::map(~ stringr::str_split(.x, "")[[1]]) |> 
    purrr::transpose() |>
    purrr::map(~ sum(.x == "#") - 1)
  if (stringr::str_detect(input[[i]][1], "^#+$")) {
    locks <- append(list(code), locks)
  } else {
    keys <- append(list(code), keys)
  }
}

result <- tidyr::expand_grid(keys, locks) |> 
  dplyr::mutate(
    fit = purrr::pmap_lgl(
      list(keys, locks),
      ~all(purrr::map2_lgl(.x, .y, ~ .x + .y <= 5))
    )
  ) |> 
  dplyr::pull(fit) |> sum()

glue::glue("answer: {result}")
