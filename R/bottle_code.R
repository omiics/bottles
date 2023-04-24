#' Bottle code
#' 
#' @description
#' Takes the provided code and packages it into a small bottle environment.
#'
#' @param expr      code block that should be bottled
#' @param packages  Specify which packages should be used to run the code
#' @param env       Environment that the bottle should be based on. Default is parent environment.
#' 
#' @return bottle with code inside
#' @export
#' 
bottle_code <- function(expr, packages = NULL, env = parent.frame()) {
  bottle_data <- list()
  
  bottle_data$code <- deparse(substitute(expr))
  
  keywords <- bottle_keywords(bottle_data$code)
  active_variables <- keywords[keywords %in% names(env)]
  remaining_keywords <- keywords[!(keywords %in% active_variables)]
  
  # Fetch any values from the environment and make a mini environment to include the data
  bottle_data$env <- mget(active_variables, envir = env)
  
  # Add any libraries that are needed to generate the code here
  if (!is.null(packages)) {
    
    bottle_data$packages <- packages
    
  } else {
    # Auto detect what packages are used in the code
    packages <- c()
    
    base_packages <- c("base", "stats", "graphics", "grDevices", "utils", "methods", "datasets")
    
    for (kw in remaining_keywords) {
      
      pkg_name <- NULL
      
      tryCatch({
          pkg_name <- environmentName(environment(get(kw)))
      },
        error = function(cond) {}
      )
      
      if (is.null(pkg_name)) next
      if (pkg_name %in% base_packages) next
      if (pkg_name == "") next
      
      packages <- c(packages, pkg_name)
    }
    
    packages <- unique(packages)
    
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
  
  # Ignore certain keywords ("if", "for")
  
}