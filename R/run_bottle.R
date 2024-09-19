#' @export
run_bottle <- function(bottle) UseMethod("run_bottle")

#' @export
run_bottle.bottle <- function(bottle) {
    
  # Attach any required packages for the bottle
  if (!is.null(bottle$packages)){
    
    for (package in bottle$packages) {
      
      # Skip if package already is loaded (Avoids unloading after the bottle is run)
      if (package %in% (.packages())) next
      
      # Library based approach
      suppressPackageStartupMessages(
        get("library")(package, character.only=TRUE)
      )
      
      on.exit(detach(paste0("package:", package), character.only = TRUE))
      
    }
    
  }
  
  # Run the code and capture errors in an elegant way
  tryCatch({
    if (class(bottle$env) == "list") {
      eval(parse(text = bottle$code), envir = bottle$env)
    } else if (class(bottle$env) == "environment") {
      eval(parse(text = bottle$code), envir = new.env(parent=bottle$env))
    } else {
      cli::cli_abort(
        c(
          "x" = "Unable to run the code and the stored environment is of an unexpected type",
          "i" = "Class is {class(bottle$env)} and must be either a list or an environment!"
        )
      )
    }
  }, error = function(cond) {
    
    cli::cli_abort(
      c("Evaluation of bottle failed!",
        "i" = "Execution of the bottle has failed as there might be an error or an object referenced is missing!",
        "x" = "Original execution error:",
        "x" = "\t{cond$message}")
    )
    
  })
  
}