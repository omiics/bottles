#' @export
save_bottle <- function(bottle, filename) UseMethod("save_bottle")

#' @export
save_bottle.bottle <- function(bottle, filename) {
  saveRDS(bottle, file = filename)
}

#' @export
load_bottle <- function(filename) {
  object <- readRDS(filename)
  class(object) <- "bottle"
  object
}