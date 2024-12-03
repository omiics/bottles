test_that("Crate initialization", {

    crate <- Crate$new()

    crate$bottle("test", "bottle")({
        1 + 1
    })

    expect_s3_class(crate$get_bottle("test", "bottle"), "bottle")

    expect_equal(crate$run_bottle("test", "bottle"), 2)

})

test_that("Crate shared env clean build", {

    library(tidyverse)

    crate <- Crate$new()

    example_data <- tibble(
        x = 1:20,
        y = 1:20,
        z = 1:20
    )

    crate$bottle("X", "FilterAbove10", search_env = environment())({

        example_data %>% filter(x > 10)

    })

    # Test warning for shared code
    expect_warning(crate$bottle("X", "FilterAbove5", search_env = environment())({

        example_data %>% filter(x > 5)

    }), "Variable `example_data` is already present in the shared environment!")

    # Test no warning using the shared function marker
    expect_no_warning(
        crate$bottle("X", "FilterAbove15", search_env = environment())({

        # Use shared to say example_data already exists
        shared(example_data) %>% filter(x > 15)

    }))

    expect_equal(
        crate$run_bottle("X", "FilterAbove10")$x,
        11:20
    )
    expect_equal(
        crate$run_bottle_dyn(c("X", "FilterAbove10"))$x,
        11:20
    )

    expect_equal(
        crate$run_bottle("X", "FilterAbove5")$x,
        6:20
    )
    expect_equal(
        crate$run_bottle_dyn(c("X", "FilterAbove5"))$x,
        6:20
    )

    expect_equal(
        crate$run_bottle("X", "FilterAbove15")$x,
        16:20
    )
    expect_equal(
        crate$run_bottle_dyn(c("X", "FilterAbove15"))$x,
        16:20
    )

    # Snapshot test of the print function
    expect_snapshot(
        print(crate)
    )

    expect_equal(
        ls(crate$get_bottle("X", "FilterAbove15")$env),
        character(0)
    )

    filter_value <- 15

    crate$bottle("Y", "FilterAbove15", search_env = environment())({

        # Label as shared
        shared(example_data) %>% 
            # Do not share the specific
            filter(y > !!unshare(filter_value))

    })

    expect_equal(
        crate$run_bottle("Y", "FilterAbove15")$y,
        16:20
    )

    expect_equal(
        ls(crate$get_bottle("Y", "FilterAbove15")$env),
        c("filter_value")
    )

})

test_that("Crate save and load works as expected", {

    # Setup at temporary directory with nothing in it
    setup_clean_test_env()

    # Initialize the Crate
    crate <- Crate$new()

    Conditions <- c("A", "B", "C")
    Comparisons <- list(
        c("A", "B"),
        c("A", "C"),
        c("B", "C")
    )
    n <- 1000

    test_dataset <- tibble(
        Condition = sample(Conditions, n, replace = TRUE),
        Effect = rgamma(n, shape = 2.5, scale = 0.7),
        Age = round(rnorm(n, mean = 40, sd = 15), 0)
    )

    # Add the dataset into the crate
    crate$add_data(test_dataset)

    for (comp in Comparisons) {

        # Crate
        crate$bottle(comp[1], comp[2], search_env = environment())({

            # Generate a plot from the comparison
            shared(test_dataset) %>%
                filter(Condition %in% unshare(comp)) %>%
                ggplot(aes(x = Age, y = Effect, color = Condition)) +
                    geom_point() + theme_bw()

        })

    }

    expect_equal(
        crate$ls_data(),
        c("test_dataset")
    )

    for (comp in Comparisons) {
        expect_equal(
            get("comp", envir=crate$get_bottle(comp[1], comp[2])$env),
            comp
        )
    }

    crate$save("crate.rda")

    crate_new <- Crate$new(filename = "crate.rda")

    expect_equal(
        crate_new$ls_data(),
        c("test_dataset")
    )

    for (comp in Comparisons) {

        expect_equal(
            ls_bottle(crate_new$get_bottle(comp[1], comp[2])),
            c("comp", "test_dataset")
        )

        expect_equal(
            get("comp", envir=crate_new$get_bottle(comp[1], comp[2])$env),
            comp
        )

    }


})


test_that("Adding, getting, removing and listing environments works in Crates", {

    crate <- Crate$new()

    A <- 1000
    B <- 2000
    C <- 3000

    crate$add_data(A)

    expect_setequal(crate$ls_data(), c("A"))
    expect_equal(crate$get_data(A), A)

    crate$add_data(B)

    expect_setequal(crate$ls_data(), c("A", "B"))
    expect_equal(crate$get_data(B), B)

    crate$add_data(C)

    expect_setequal(crate$ls_data(), c("A", "B", "C"))
    expect_equal(crate$get_data(C), C)

    expect_error(
        crate$add_data(D),
        "`D` does not exist in the environment"
    )

    # Try removing an item
    crate$rm_data("B")

    expect_setequal(crate$ls_data(), c("A", "C"))

    # List with no bottles
    expect_equal(
        crate$ls_bottles(),
        list()
    )

    # Add a bottle
    crate$bottle("A", "plus", "C", search_env = environment())({

        shared(A) + shared(C)

    })

    # Run the bottle
    expect_equal(
        crate$run_bottle("A", "plus", "C"),
        4000
    )

    # Check list of bottles now
    expect_equal(
        crate$ls_bottles(),
        list(
            c("A", "plus", "C")
        )
    )

    crate$bottle("A", "plus", "B", search_env = environment())({

        shared(A) + shared(B)

    })

    expect_error(
        crate$run_bottle("A", "plus", "B"),
        "object 'B' not found"
    )

    expect_equal(
        crate$ls_bottles(),
        list(
            c("A", "plus", "B"),
            c("A", "plus", "C")
        )
    )

})