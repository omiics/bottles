library(devtools)
library(microbenchmark)
library(tidyverse)

# Document and load the package
document()

# The aim is to test with a larger simulated dataset for storage improvements and runtime improvements for saving and loading the data
# Comparsion is between Crate vs List of Bottles

# Simulated dataset
Conditions <- c("A", "B", "C", "D", "E", "F")
Comparisons <- list(
    c("A", "B"),
    c("A", "C"),
    c("A", "D"),
    c("B", "C"),
    c("B", "D"),
    c("B", "E"),
    c("B", "F"),
    c("C", "D"),
    c("C", "E"),
    c("C", "F"),
    c("D", "E"),
    c("D", "F"),
    c("E", "F")
)
n <- 1000000

big_dataset <- tibble(
    Condition = sample(Conditions, n, replace = TRUE),
    Effect = rgamma(n, shape = 2.5, scale = 0.7),
    Age = round(rnorm(n, mean = 40, sd = 15), 0)
)

# Bottle the code
create_bottle_list <- function(Comparisons, big_dataset) {
    bottle_list <- list()

    for (comp in Comparisons) {
        
        comp_name <- paste(comp, collapse = " vs ")

        # Bottle list
        bottle_list[[comp_name]] <- bottle_code({
        
            big_dataset %>%
                filter(Condition %in% comp) %>%
                ggplot(aes(x = Age, y = Effect, color = Condition)) +
                    geom_point() + theme_bw()

        })

    }

    bottle_list
}

create_crate <- function(Comparisons, big_dataset) {
    crate <- Crate$new()

    # Add the dataset into the crate
    crate$add_data(big_dataset)

    for (comp in Comparisons) {
        
        comp_name <- paste(comp, collapse = " vs ")

        # Crate
        crate$bottle(comp[1], comp[2])({

            # Generate a plot from the comparison
            shared(big_dataset) %>%
                filter(Condition %in% unshare(comp)) %>%
                ggplot(aes(x = Age, y = Effect, color = Condition)) +
                    geom_point() + theme_bw()

        })

    }

    crate
}

# Benchmark filling the datastructures
runtime_bench <- microbenchmark(
    Create_and_fill_bottle_list = create_bottle_list(Comparisons),
    Create_and_fill_Crate = create_crate(Comparisons, big_dataset)
)
# Unit: milliseconds
#                         expr      min       lq     mean   median       uq
#  Create_and_fill_bottle_list 3.901207 4.099525 4.535686 4.236987 4.394369
#        Create_and_fill_Crate 7.242977 7.532951 8.067200 7.720994 8.012716
#        max neval cld
#   9.554126   100  a 
#  14.380030   100   b

autoplot(runtime_bench)

bottle_list <- create_bottle_list(Comparisons)
crate <- create_crate(Comparisons, big_dataset)

# Benchmark saving
saving_bench <- microbenchmark(
    save_bottle_list = saveRDS(bottle_list, file = "bottle_list.rds"),
    load_crate = crate$save("crate.rda"),
    times = 25,
    unit = "seconds"
)
# Unit: seconds
#         expr        min         lq       mean     median        uq        max
#  bottle_list 10.5977832 10.8412053 11.0735150 11.2596373 11.297927 11.3240789
#        crate  0.8203121  0.8674775  0.8726243  0.8776552  0.882466  0.9274185
#  neval cld
#     10  a 
#     10  b

autoplot(saving_bench)

# Benchmark reading
microbenchmark(
    load_bottle_list = readRDS("bottle_list.rds"),
    crate = Crate$new(filename = "crate.rda"),
    times = 20,
    unit = "seconds"
)
# Unit: seconds
#         expr      min        lq      mean    median        uq       max neval
#  bottle_list 2.131239 2.2085219 2.3278021 2.3068424 2.4516974 2.4672647    20
#        crate 0.155979 0.1614523 0.1722868 0.1737288 0.1804524 0.2020645    20
#  cld
#   a 
#   b

document()

crate$get_bottle("D", "E")

bottle_list[["A vs B"]]
