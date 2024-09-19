#' @title Crate - Collection of bottles
#' 
#' @description 
#' A crate is a collection of bottles which ensures that all of the bottles
#' share the same env. The aim is for cases where you generate multiple bottles with repeated.\cr\cr
#' 
#' A crate provides multiple interfaces
#' 
#' It is very important to consider that all of the bottles share the same namespace!
#' This means that if you have variables that overlab in the bottles, they will all have the same value.
#' Aim to store top level data in the object, and do any specific filtering within the bottle!
#' 
#' @importFrom R6 R6Class
#' @importFrom rlang new_environment
Crate <- R6Class("Crate",
    public = list(

        #' @description
        #' Constructor function
        #' 
        #' @param packages   Add some packages that should be loaded with any bottle
        #' 
        #' @return [Crate]
        initialize = function(filename = NULL, packages = NULL) {

            if (is.null(filename)) {
                private$data_env <- new.env()
                private$bottle_store <- new.env()
                private$packages <- packages
            } else {
                private$load_from_file(filename)
            }


        },

        #' @description
        #' Bottle some code.
        #' 
        #' @usage Crate$bottle(id)({ 1 + 1 })
        #' 
        #' @param ...           Names for the id of the bottle
        #' @param search_env    Environment to search for variables and data
        #' 
        #' @return Function that takes in an expression to bottle
        bottle = function(..., search_env = parent.frame()) {

            id <- private$create_id(...)

            function(expr, warn = TRUE) {

                code <- deparse(substitute(expr))

                bottle <- bottle_code(code, env = search_env, deparse = FALSE, crate_storage = TRUE)

                private$add(bottle, id, warn = warn)

            }

        },

        #' @description
        #' Get a bottle from the crate
        #' 
        #' @param ... Id of the bottle
        #' 
        #' @return Bottle if present otherwise NULL
        get_bottle = function(...) {

            id <- private$create_id(...)

            private$bottle_store[[id]]

        },

        #' @description
        #' Run bottle with a specific id from the crate
        #' 
        #' @param ... Id of the bottle
        #' 
        #' @return Result of the bottle
        run_bottle = function(...) {

            id <- private$create_id(...)

            run_bottle(private$bottle_store[[id]])

        },

        #' @description
        #' Print get an overview of
        #' 
        #' @param x    Object
        #' @param ...  Ignored parameters
        print = function(x, ...) {

            header <- "
   .+--------+
 .' |      .'|
+---+----+'  |  Crate of Bottles
|   |    |   |                v1
|  ,+----+---+ 
|.'      | .'  
+--------+'    
"

            cat(header)

            cli::cli_h1("Number of Bottles:")
            cli::cli_text("{length(private$bottle_store)} Bottles")

            if (!is.null(private$packages)) {
                cli::cli_h1("Bottled packages:")
                packages <- cli::cli_vec(private$packages, style = list(
                    "vec-sep" = ", ", "vec-last" = " & "
                ))
                cli::cli_text("{packages}")
            }

            cli::cli_h1("Shared data environment:")
            variables <- cli::cli_vec(names(private$data_env), style = list(
                "vec-sep" = ", ", "vec-last" = " & "
            ))
            cli::cli_text("{variables}")

        },

        #' @description
        #' Save the crate as an .rda file.
        #' 
        #' @param filename  rda file that should store the data
        save = function(filename) {
            
            # Figure out how to store this as an .rda file

            # New env
            storage_env <- new.env(parent = emptyenv())

            # Arrange the data
            storage_env[["data_env"]] <- private$data_env
            storage_env[["bottle_store"]] <- private$bottle_store
            storage_env[["packages"]] <- private$packages

            # Save to file
            save(list = ls(storage_env), file = filename, envir=storage_env)

        },

        add_to_env = function(var, env = parent.frame()) {

            varname <- deparse(substitute(expr))

            if (!(varname %in% names(env))) {
                cli::cli_abort(
                    c(
                        "x" = "{.var {varname}} does not exist in the environment"
                    )
                )
            }

            private$data_env[[varname]] <- var

        },

        get_from_env = function(var) {
            varname <- deparse(substitute(var))

            private$data_env[[varname]]
        }

    ),
    private = list(
        # Shared env for bottles
        data_env = NULL,
        # Env for storing the bottles
        bottle_store = NULL,
        # Shared packages
        packages = NULL,
        
        create_id = function(...) {
            paste(..., sep = "<=>")
        },

        deconstruct_id = function(id) {
            unlist(stringr::str_split(id, fixed("<=>")))
        },

        merge_packages = function(packages) {
            private$packages <- unique(c(private$packages, packages))
        },

        move_to_data_env = function(source_env, unshare, warn = TRUE) {
            list_data_env <- ls(private$data_env)
            list_env <- ls(source_env)
            for(v in list_env) {
                # Skip if value is listed to be unshared
                if (v %in% unshare) next
                if (v %in% list_data_env) {
                    cli::cli_warn(
                        c(
                            "!" = "Variable {.var {v}} is already present in the shared environment!",
                            "x" = "Might cause serious conflicts if the variables are supposed to be different!",
                            "i" = "Use top-level data instead, and filter inside the bottle instead!"
                        )
                    )
                    next
                }
                private$data_env[[v]] <- source_env[[v]]

                # Delete the old value
                rm(list = v, envir = source_env)
            }
        },

        add = function(bottle, id, warn = TRUE) {

            # Merge package information
            private$merge_packages(bottle$packages)

            ## Convert the bottle env from a list to an env
            bottle$env <- rlang::as_environment(bottle$env)

            # Extract variables from the bottle env into shared env
            ## Will delete anything moved
            private$move_to_data_env(bottle$env, bottle$unshare, warn = warn)

            # Update the bottle environment
            parent.env(bottle$env) <- private$data_env

            # Store the bottle
            private$bottle_store[[id]] <- bottle
            
        },

        load_from_file = function(filename) {

            unload_env <- new.env()

            load(filename, envir = unload_env)

            private$data_env <- unload_env[["data_env"]]
            private$bottle_store <- unload_env[["bottle_store"]]
            private$packages <- unload_env[["packages"]]

        }

    )
)