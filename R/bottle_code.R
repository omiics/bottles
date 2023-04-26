#' Bottle code
#' 
#' @description
#' Takes the provided code and packages it into a small bottle environment.
#'
#' @param expr      code block that should be bottled
#' @param packages  Specify which packages should be used to run the code
#' @param env       Environment that the bottle should be based on. Default is parent environment
#' @param debug     Run a detailed debug mode to see what steps the function is taking
#' 
#' @return bottle with code inside
#' @export
#' 
bottle_code <- function(expr, packages = NULL, env = parent.frame(), debug=FALSE) {
  
  if (debug) cli::cli_alert_info("Running bottle_code debug mode!")
  
  bottle_data <- list()
  
  bottle_data$code <- deparse(substitute(expr))
  
  if (debug) cli::cli_alert_info("Detecting keywords")
  
  keywords <- bottle_keywords(bottle_data$code)
  
  if (debug) cli::cli_alert_info("Found keywords: {.vals {keywords}}")
  
  active_variables <- keywords[keywords %in% names(env)]
  
  if (debug) cli::cli_alert_info("Active variables: {.vals {active_variables}}")
  
  remaining_keywords <- keywords[!(keywords %in% active_variables)]
  
  if (debug) cli::cli_alert_info("Remaining keywords: {.vals {remaining_keywords}}")
  
  
  # Fetch any values from the environment and make a mini environment to include the data
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
#' @param expr_str
#'
bottle_keywords <- function(expr_str) {
  # old regex: "\\w+" (Matches any word)
  # good regex: "(?<!::)\\b\\w+\\b(?!::\\w+| =|=)" (Matches any variables/function names that are not prefixed by or followed by a namespace ::)
  # better regex: "(?<!::|\\$)\\b\\w+\\b(?!::\\w+| =|=)" (Includes fix for ignoring list variables )
  # most detailed regex: "(?<!::|:::|\$|"|')\b\w+\b(?!::\w+| =|=|"|'| <-|<-)" ()
  
  values <- unlist(stringr::str_extract_all(expr_str, "(?<!::|\\$)\\b\\w+\\b(?!::\\w+| =|=)"))
  values <- unique(values)
  
  # Remove any integers
  values <- values[is.na(as.numeric(values))]
  
  # Ignore certain keywords ("if", "for")
  R_keywords <- c("if", "for", "while", "c")
  values <- values[!(values %in% R_keywords)]
  
  values
}