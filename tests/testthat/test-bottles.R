test_that("Test keyword detection", {

    # Test standard tidyverse code
    keywords <- bottle_keywords('{
        # Filtering the input data
        table_information <- input_table %>%
            filter(Size < !!size_filter) %>%
            filter(Category %in% !!categories)

        # Generating a bottle
        table_information %>% ggplot(aes(x = Size, y = Category)) +
            geom_point() + theme_bw() # Using a default theme
    }')

    expect_equal(
        keywords$keywords, 
        c(
            "input_table", "filter", "Size", "size_filter", "Category", "categories", 
            "table_information", "ggplot", "aes", "geom_point", "theme_bw"
        )
    )

    # Code having R6 classes
    keywords <- bottle_keywords('{

        to_filter <- c("A", "B", "C")
        colors <- c("#ADFBAF", "#ABDFCA", "#252627")

        # Call internal function
        Data$filter(to_filter)

        Data$prepare_data() %>% ggplot(aes(x = x, y = y)) +
            geom_point() + theme_bw()

    }')

    expect_equal(
        keywords$keywords,
        c("Data", "to_filter", "ggplot", "aes", "x", "y", "geom_point", "theme_bw")
    )

    keywords <- bottle_keywords('{

        # Create a table
        x <- tibble::tribble(
            ~x, ~y, ~z,
             1,  2,  3,
             2,  3,  4,
             3,  4,  5
        )

        x$x <- x$x*2

        x

    }')

    expect_equal(
        keywords$keywords,
        c("x")
    )

    keywords <- bottle_keywords('{

        # Comment that should be skipped
        zap <- c("zip", "zap", "zippy" ,"zappy")

        # Data is shared and should not be included
        shared(big_dataset) %>% 
            filter(!(name %in% !!ignore(zap))) %>%
            filter(sum > !!unshare(filter_value))

    }', crate_storage = TRUE)

    expect_equal(
        keywords$keywords,
        c(
            "shared", "filter", "name", "ignore",
            "sum", "unshare", "filter_value"
        )
    )

    expect_equal(
        keywords$unshare,
        c("filter_value")
    )

})

test_that("Test an old test bottle", {

    setup_old_test_bottle()

    bottle <- load_bottle("old_test_bottle.rds")

    # Snapshot test of the print output
    expect_snapshot(
        print(bottle)
    )

    # Run the bottle
    p <- run_bottle(bottle)

    expect_s3_class(p, "ggplot")
    

})

test_that("Creation, running and saving bottles works", {

    setup_clean_test_env()

    # Load the library
    library(tibble)

    # Library will be detected 
    bottle <- bottle_code({

        # Create a table
        x <- tribble(
            ~x, ~y, ~z,
             1,  2,  3,
             2,  3,  4,
             3,  4,  5
        )

        x$x <- x$x*2

        x

    })

    value <- run_bottle(bottle)

    expect_s3_class(value, c("tbl"))

    expect_equal(value$x, c(2,4,6))

    # Detach the package again
    detach("package:tibble", character.only = TRUE)

    # Call again without package in namespace
    value <- run_bottle(bottle)

    expect_s3_class(value, c("tbl"))

    expect_equal(value$x, c(2,4,6))

    expect_snapshot(bottle_code({

        # Create a table
        x <- tribble(
            ~x, ~y, ~z,
             1,  2,  3,
             2,  3,  4,
             3,  4,  5
        )

        x$x <- x$x*2

        x

    }, debug = TRUE))

    save_bottle(bottle, "test_bottle.rds")

    bottle_2 <- load_bottle("test_bottle.rds")

    value <- run_bottle(bottle_2)

    expect_s3_class(value, c("tbl"))

    expect_equal(value$x, c(2,4,6))

})

test_that("Test error in bottle", {

    # Note invalid syntax not possible
    # The code block {} will always be evaluated for syntax errors

    force_low_value <- function(value) {
        if (value > 10) {
            cli::cli_abort(
                c(
                    "x" = "Value must be lower than 10!",
                    "i" = "Value provided was {value}"
                )
            )
        }
        value
    }

    bottle <- bottle_code({

        force_low_value(40)

    })

    expect_error(run_bottle(bottle), "Value must be lower than 10!")

})

test_that("Test malformed bottle errors", {

    bottle <- bottle_code({

        1 + 1
    
    })

    # Make the environment malformed
    bottle$env <- c(1,2,3,4)

    expect_error(
        run_bottle(bottle),
        "Unable to run the code and the stored environment is of an unexpected type"
    )

    expect_error(
        ls_bottle(bottle),
        "Bottle is malformed. The environment must be either a list or a environment!"
    )

})