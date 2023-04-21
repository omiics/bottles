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
  
  cat(bottle_header, "\n\n")
  
  cat("Bottled code:\n")
  for (line in x$code) {
    cat("\t", line,"\n")
  }
  
  if (!is.null(x$packages)){
    cat("\nBottled packages:\n\n")
    cat("\t", x$packages,"\n")
  }
  
  cat("\nBottled environment:\n")
  
  cat("\n")
  cat("\t", names(x$env))
  
}