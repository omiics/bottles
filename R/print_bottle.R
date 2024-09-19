#' Print 
#' 
#' 
#' @export
print.bottle <- function(x){
  
  bottle_header <- "
                 [ ]
               .-' '-.
R Code Bottle  :-...-:
           v1  |     |
               |     |
               `-...-'"
  
  cat(bottle_header, "\n")
  
  cli::cli_h1("Bottled code:")
  cli::cli_code(lines = x$code)
  
  if (!is.null(x$packages)){
    cli::cli_h1("Bottled packages:")
    packages <- cli::cli_vec(x$packages, style = list(
      "vec-sep" = ", ", "vec-last" = " & "
      ))
    cli::cli_text("{packages}")
  }
  
  cli::cli_h1("Bottled environment:")
  if (class(x$env) == "list") {
    variables <- cli::cli_vec(ls(x$env), style = list(
      "vec-sep" = ", ", "vec-last" = " & "
    ))
    cli::cli_text("{variables}")
  } else if (class(x$env) == "environment") {
    env_vars <- c(ls(x$env), ls(parent.env(x$env)))
    variables <- cli::cli_vec(env_vars, style = list(
      "vec-sep" = ", ", "vec-last" = " & "
    ))
    cli::cli_text("{variables}")
  }
   
}