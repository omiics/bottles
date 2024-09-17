setup_clean_test_env <- function(env = parent.frame()) {
    # Create a temporary directory
    tmpdir <- tempdir()
    dir <- fs::path_join(c(tmpdir, "test_bottle"))
    fs::dir_create(dir)

    # Cleanup the directory once the test finishes
    withr::defer(fs::dir_delete(dir), envir = env)

    # Change work directory to the tmp file
    current_wd <- getwd()
    setwd(dir)
    # Change back the working directory once the test is over
    withr::defer(setwd(current_wd), envir = env)
}

setup_old_test_bottle <- function(env = parent.frame()) {

    # Create a temporary directory
    tmpdir <- tempdir()
    dir <- fs::path_join(c(tmpdir, "test_bottle"))
    fs::dir_create(dir)

    # Cleanup the directory once the test finishes
    withr::defer(fs::dir_delete(dir), envir = env)

    # Copy relevant data
    fs::file_copy(
        system.file("old_test_bottle.rds", package = "bottles"),
        fs::path_join(c(dir, "old_test_bottle.rds"))
    )

    # Change work directory to the tmp file
    current_wd <- getwd()
    setwd(dir)
    # Change back the working directory once the test is over
    withr::defer(setwd(current_wd), envir = env)

}