#' @export
run_bottle <- function(bottle) UseMethod("run_bottle")

#' @export
run_bottle.bottle <- function(bottle) {
  
  # If there are packages add them all together
  run_eval <- function(code, env){
    eval(parse(text = code), envir = env)
  }
  
  # Attach any required packages for the bottle
  if (!is.null(bottle$packages)){
    
    for (package in bottle$packages) {
      
      # Experiment with using box?
      
      # Library based approach
      suppressPackageStartupMessages(
        get("library")(package, character.only=TRUE)
      )
      
      on.exit(detach(paste0("package:", package), character.only = TRUE))
      
    }
    
  }
  
  # Run the code and capture errors in an elegant way
  tryCatch({
    eval(parse(text = bottle$code), envir = bottle$env)
  }, error = function(cond) {
    
    cli::cli_abort(
      c("Evaluation of bottle failed!",
        "i" = "Execution of the bottle has failed as there might be an error or an object referenced is missing!",
        "x" = "Original execution error:",
        "x" = "\t{cond$message}")
    )
    
  })
  
}