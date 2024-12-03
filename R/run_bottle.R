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

  # Extend the bottle code
  code <- bottle$code

  # Add tryCatch and provide a more detailed error message
  code <- c(
    "tryCatch({
",
    code,
"}, error = function(cond) {
  call <- bottles::deparse_to_one_line(cond$call)
  call_chars <- as.character(cond$call)
  if (call_chars[1] == 'eval') {
    call_text <- ''
  } else {
    call_text <- glue::glue('|=| Call: {call} ')
  }
  packages <- .packages()
  environment_data <- ls(rlang::env_parent())
  cli::cli_abort(
    'Error: {cond$message} {call_text}|=| Environment: {environment_data} |=| Packages: {packages}'
  )
})"
  )
 
  # Run the code and capture errors in an elegant way
  tryCatch({
    if (class(bottle$env) == "list") {
      eval(parse(text = code), envir = bottle$env)
    } else if (class(bottle$env) == "environment") {
      eval(parse(text = code), envir = new.env(parent=bottle$env))
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
        "x" = "{cond$message}")
    )
    
  })
  
}