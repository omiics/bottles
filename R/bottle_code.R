#' Bottle code
#' 
#' @description
#' Takes the provided code and packages it into a small bottle environment.
#'
#' @param expr            Code block that should be bottled
#' @param packages        Specify which packages should be used to run the code
#' @param env             Environment that the bottle should be based on. Default is parent environment
#' @param debug           Run a detailed debug mode to see what steps the function is taking
#' @param deparse         Should the expr variable be deparsed. Default TRUE. Set to FALSE if you are providing already deparsed code.
#' @param crate_storage   Used internally by Crate. Applies the effects of the shared, unshare and ignore functions 
#' 
#' @return bottle with code inside
#' @export
#' 
bottle_code <- function(expr, packages = NULL, env = parent.frame(), debug = FALSE, deparse = TRUE, crate_storage = FALSE) {
  
  if (debug) cli::cli_alert_info("Running bottle_code debug mode!")
  
  bottle_data <- list()
  
  if (deparse) {
    bottle_data$code <- deparse(substitute(expr))
  } else {
    bottle_data$code <- expr
  }

  if (debug) cli::cli_alert_info("Detecting keywords")
  
  keywords_output <- bottle_keywords(bottle_data$code, crate_storage = crate_storage)
  keywords <- keywords_output$keywords

  bottle_data$unshare <- keywords_output$unshare
  
  if (debug) cli::cli_alert_info("Found keywords: {.vals {keywords}}")
  
  active_variables <- keywords[keywords %in% names(env)]
  
  if (debug) cli::cli_alert_info("Active variables: {.vals {active_variables}}")
  
  remaining_keywords <- keywords[!(keywords %in% active_variables)]
  
  if (debug) cli::cli_alert_info("Remaining keywords: {.vals {remaining_keywords}}")
  
  # Fetch any values from the environment and make a mini environment to include the data
  ## mget returns a list and not an environment
  bottle_data$env <- mget(active_variables, envir = env)
  
  # Add any libraries that are needed to generate the code here
  if (!is.null(packages)) {
    
    bottle_data$packages <- packages
    
  } else {
    if (debug) cli::cli_alert_info("Auto detecting keywords")
    
    # Auto detect what packages are used in the code
    packages <- c()
    
    base_packages <- c("base", "stats", "graphics", "grDevices", "utils", "methods", "datasets")
    
    for (kw in remaining_keywords) {
      
      if (debug) cli::cli_alert_info("Checking keyword: {kw}")
      if (debug) cli::cli_alert_info("Current packages: {.vals {packages}}")
      
      pkg_name <- NULL
      
      tryCatch({
          pkg_name <- environmentName(environment(get(kw)))
      },
        error = function(cond) {}
      )
      
      if (debug) cli::cli_alert_info("Comes from package: {pkg_name}")
      
      if (is.null(pkg_name)) next
      if (pkg_name %in% base_packages) next
      if (pkg_name == "") next
      
      if (debug) cli::cli_alert_info("Passed checks will add package name to packages")
      
      packages <- c(packages, pkg_name)
    }
    
    if (debug) cli::cli_alert_info("Reducing to unique package names")
    
    packages <- unique(packages)
    
    if (debug) cli::cli_alert_info("Final detected packages: {.vals {keywords}}")
    
    bottle_data$packages <- packages
    
  }
  
  class(bottle_data) <- "bottle"
  
  bottle_data
}

#' Find all keywords for bottle
#' 
#' @description
#' Look for all of the keywords from the code block and return all of the unique entries
#' 
#' @param expr_str       String with the code expression to be analysed
#' @param crate_storage  
#' 
#' @return Vector of keywords detected in the code
bottle_keywords <- function(expr_str, crate_storage = FALSE) {
  # old regex: "\\w+" (Matches any word)
  # good regex: "(?<!::)\\b\\w+\\b(?!::\\w+| =|=)" (Matches any variables/function names that are not prefixed by or followed by a namespace ::)
  # better regex: "(?<!::|\\$)\\b\\w+\\b(?!::\\w+| =|=)" (Includes fix for ignoring list variables )
  # most detailed regex: "(?<!::|:::|\\$|\"|\'|\\%)\\b\\w+\\b(?!::\\w+| *=|\"|\'|\\%| *<-)" (Cleans up the detection even more)

  # (["'])(.*?[^\\])\1 ##

  # Preprocess expression
  ## Remove all comments
  ## Remove all strings from consideration

  expr_str <- stringr::str_remove_all(expr_str, "([\"\'])(.*?[^\\\\])\\1") # Match any strings and remove them
  expr_str <- stringr::str_remove_all(expr_str, stringr::regex("#.*$", multiline = TRUE)) # Match and delete anything that comes after a comment

  values <- unlist(stringr::str_extract_all(expr_str, "(?<!::|:::|\\$|\"|\'|\\%|~)\\b\\w+\\b(?!::\\w+| *=|\"|\'|\\%| *<-)"))
  values <- unique(values)

  unshare_values <- NULL

  if (crate_storage) {

    # Search for tagging function
    ## shared() - Should be skipped, since it already shared in the environment
    shared_values <- unlist(stringr::str_extract_all(expr_str, "(?<=shared\\()\\b\\w+\\b(?=\\))"))

    # Filter any share values
    values <- values[!(values %in% shared_values)]

    ## ignore() - Variable should just be skipped
    ignored_values <- unlist(stringr::str_extract_all(expr_str, "(?<=ignore\\()\\b\\w+\\b(?=\\))"))

    # Filter any share values
    values <- values[!(values %in% ignored_values)]

    ## unshare()
    unshare_values <- unlist(stringr::str_extract_all(expr_str, "(?<=unshare\\()\\b\\w+\\b(?=\\))"))

    ## Should not filter these
  }

  # Remove any integers
  suppressWarnings(values <- values[is.na(as.numeric(values))])
  
  # Ignore certain keywords ("if", "for")
  R_keywords <- c("if", "for", "while", "c", "TRUE", "FALSE")
  values <- values[!(values %in% R_keywords)]
  
  list(
    keywords = values,
    unshare = unshare_values
  )
}

#' Mark a variable as being shared in a bottle code
#' 
#' @description
#' Helps the keyword detection to remove the keyword so it is not added to the variable again 
#'
#' @param var   Variable to be marked
#' 
#' @return return the var without any modifications
shared <- function(var) {
  var
}

#' Mark a variable as explicitly that is should not be shared
#' 
#' @description
#' Helps keyword detection to list a keyword to be 
#' 
#' @param var   Variable to be marked
#' 
#' @return return the var without any modifications
unshare <- function(var) {
  var
}

#' Mark a variable to be ignored by the 
#' 
ignore <- function(var) {
  var
}