path <- "day-04/input.txt"

word_search <- do.call(rbind, strsplit(readr::read_lines(path), ""))

# get row, column, and diagonal strings
extract_r_c_x <- function(mat) {
  n <- nrow(mat)
  m <- ncol(mat)
  rcx <- list()
  
  # top-left to bottom-right diagonals
  for (k in 1:(n + m - 1)) {
    chars <- c()
    for (i in 1:n) {
      j <- k - i + 1 # too many indices for one matrix jeeze
      if (j >= 1 && j <= m) { # remember to check our boundaries first
        chars <- c(chars, mat[i, j])
      }
    }
    if (length(chars) > 0) {
      rcx <- append(rcx, list(paste(chars, collapse = "")))
    }
  }
  
  # top-right to bottom-left
  for (k in 1:(n + m - 1)) {
    chars <- c()
    for (i in 1:n) {
      j <- i + k - n
      if (j >= 1 && j <= m) {
        chars <- c(chars, mat[i, j])
      }
    }
    if (length(chars) > 0) {
      rcx <- append(rcx, list(paste(chars, collapse = "")))
    }
  }

  # rows
  for (i in 1:n) {
    chars <- c()
    for (j in 1:m) {
      chars <- c(chars, mat[i, j])
    }
    rcx <- append(rcx, list(paste(chars, collapse = "")))
  }

  # columns
  for (j in 1:m) {
    chars <- c()
    for (i in 1:n) {
      chars <- c(chars, mat[i, j])
    }
    rcx <- append(rcx, list(paste(chars, collapse = "")))
  }
  
  return(rcx)
}

# Extract diagonals from the word_search matrix
rcx <- extract_r_c_x(word_search)

xmas_cnt <- rcx |> 
  purrr::map(~ stringr::str_count(., "XMAS")) |> purrr::reduce(`+`)

samx_cnt <- rcx |> 
  purrr::map(~ stringr::str_count(., "SAMX")) |> purrr::reduce(`+`)

result <- xmas_cnt + samx_cnt

glue::glue("answer: {result}")
