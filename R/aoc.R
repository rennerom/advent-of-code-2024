#' Save input data for Advent of Code
#' 
#' @description 
#' Saves the raw input data for a given year and day from the Advent of Code website.
#' 
#' @param year An integer representing the year of the Advent of Code challenge.
#' @param day An integer representing the day of the Advent of Code challenge.
#' @param cookie A character string representing the session cookie for the Advent of Code website.
#'   If not provided, the function will look for a session cookie in the .Renviron file under the key "cookie".
#' @param file_path A character string representing the file path to save the input data.
#'    Default is "input.txt".
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists.
#'    Default is FALSE.
#' @return NULL
#' @examples
#' # Save input data for day 1 of 2021 using a session cookie stored in an .Renviron file
#' save_raw_input_data(2021, 1)
#' 
#' # Save input data for day 1 of 2021 using a session cookie passed as an argument
#' save_raw_input_data(2021, 1, cookie = "your_session_cookie")
#' 
#' # Save input data for day 1 of 2021 and save it to a custom file path
#' save_raw_input_data(2021, 1, file_path = "some/path/input.txt")
#' 
#' # Save input data for day 1 of 2021 and overwrite the existing file
#' save_raw_input_data(2021, 1, overwrite = TRUE)
#' 
#' @export
save_raw_input_data <- function(year, day, cookie, file_path = "input.txt", overwrite = FALSE) {

  content <- fetch_raw_imput_data(year, day, cookie)

  if (file.exists(file_path) && !overwrite) {
    cli::cli_alert(c(
      "x" = "File already exists.",
      "!" = "Use the {.arg overwrite = TRUE} argument to overwrite the file."
    ))
  }
  readr::write_lines(content, file_path)
}

#' Fetch raw input data for Advent of Code
#' 
#' @description
#' Fetches the raw input data for a given year and day from the Advent of Code website.
#' 
#' @param year An integer representing the year of the Advent of Code challenge.
#' @param day An integer representing the day of the Advent of Code challenge.
#' @param cookie A character string representing the session cookie for the Advent of Code website.
#'  If not provided, the function will look for a session cookie in the .Renviron file under the key "cookie".
#' @return A character string representing the raw input data.
#' @examples
#' # Fetch input data for day 1 of 2021 using a session cookie stored in an .Renviron file
#' fetch_raw_imput_data(2021, 1)
#' 
#' # Fetch input data for day 1 of 2021 using a session cookie passed as an argument
#' fetch_raw_imput_data(2021, 1, cookie = "your_session_cookie")
#' 
#' @export
fetch_raw_input_data <- function(year, day, cookie) {
  # abort if year or day is not supplied
  if (missing(year) || missing(day)) {
    cli::cli_abort(c(
      "{.var year} and {.var day} must be supplied as integers"
    ))
  }

  if (missing(cookie)) {
    if (!"cookie" %in% names(Sys.getenv())) {
      cli::cli_abort(c(
        "{.var cookie} must be supplied as a non empty string",
        "x" = "you can pass it as an argument or set it in an .Renviron file"
      ))
    } else {
      cookie <- Sys.getenv("cookie")
    }
  }

  base_url <- "https://adventofcode.com/{year}/day/{day}/input"
  url <- stringr::str_glue(base_url, year = year, day = day)

  req <- httr2::request(url) |>
    httr2::req_cookies_set(session =  cookie)

  resp <- httr2::req_perform(req)

  if (resp$status_code == 200) {
    content <- httr2::resp_body_string(resp)
  } else {
    cli::cli_abort(c(
      "x" = "Failed to fetch input data.",
      "!" = "HTTP status code: {resp$status_code} {httr2::resp_status_desc(resp)}.",
      "i" = "Check your session cookie or ensure the URL is correct."
    ))
  }
  return(content)
}

#' Print the answer to an Advent of Code challenge
#' 
#' @description
#' Pipe in the answer to an Advent of Code challenge and print it with a message.
#' 
#' @param x The answer to the Advent of Code challenge.
#' @return NULL
#' @examples
#' # Print the answer to an Advent of Code challenge
#' answer <- 42
#' answer |> print_answer()
#' 
#' @export
print_answer <- function(x, text = "answer") {
  stringr::str_glue("{text}: {x}") |> print()
}