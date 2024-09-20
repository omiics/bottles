#' @description
#' List all of the items available in the environment
#' Considers both the standalone bottle case and the bottles from crates with shared environments
#' 
#' @param bottle   Input bottle to look up environment variables
#' 
#' @export
ls_bottle <- function(bottle) UseMethod("ls_bottle")

#' @export
ls_bottle.bottle <- function(bottle) {

    if (class(bottle$env) == "list") {
        return(ls(bottle$env))
    } else if (class(bottle$env) == "environment") {
        return(c(
            ls(bottle$env), ls(parent.env(bottle$env))
        ))
    } else {
        cli::cli_abort(
            c(
                "x" = "Bottle is malformed. The environment must be either a list or a environment!"
            )
        )
    }

}
