path <- "day-04/input.txt"

word_search <- do.call(rbind, strsplit(readr::read_lines(path), ""))

nrows <- nrow(word_search)
ncols <- ncol(word_search)

empty_matrix <- matrix(".", nrows, ncols)
x_mas <- 0

for (i in 2:(nrows-1)) { # start at 2 to stay in bounds
  for (j in 2:(ncols-1)) {
    if(word_search[i, j] == "A") {
      # Check for top-left to bottom-right diagonal MAS or SAM
      if(word_search[i-1, j-1] %in% c("M", "S") && word_search[i+1, j+1] %in% c("M", "S") && word_search[i-1, j-1] != word_search[i+1, j+1]) {
        # Check for top-right to bottom-left diagonal MAS or SAM
        if(word_search[i-1, j+1] %in% c("M", "S") && word_search[i+1, j-1] %in% c("M", "S") && word_search[i-1, j+1] != word_search[i+1, j-1]) {
          
          # debug
          empty_matrix[i-1, j-1] <- word_search[i-1, j-1]
          empty_matrix[i, j] <- word_search[i, j]
          empty_matrix[i+1, j+1] <- word_search[i+1, j+1]
          empty_matrix[i-1, j+1] <- word_search[i-1, j+1]
          empty_matrix[i+1, j-1] <- word_search[i+1, j-1]

          x_mas <- x_mas + 1
        }
      }
    }
  }
}

glue::glue("answer: {x_mas}")
