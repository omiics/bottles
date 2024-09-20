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
  variables <- cli::cli_vec(ls_bottle(x), style = list(
    "vec-sep" = ", ", "vec-last" = " & "
  ))
  cli::cli_text("{variables}")
   
}