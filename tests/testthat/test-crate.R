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
        crate$run_bottle("X", "FilterAbove5")$x,
        6:20
    )

    expect_equal(
        crate$run_bottle("X", "FilterAbove15")$x,
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