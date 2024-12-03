#' Deparse and join all lines into a single line
#'
#' Calling `deparse` and joining all the returned lines into a
#' single line, separated by whitespace, and then cleaning up all the
#' duplicated whitespace (except for excessive whitespace in strings
#' between single or double quotes).
#' 
#' Credits to the logger package for the source of this function
#' 
#' @param x object to `deparse`
#' @return string
#' @export
deparse_to_one_line <- function(x) {
  gsub('\\s+(?=(?:[^\\\'"]*[\\\'"][^\\\'"]*[\\\'"])*[^\\\'"]*$)', " ",
    paste(deparse(x), collapse = " "),
    perl = TRUE
  )
}