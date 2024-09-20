library(devtools)
library(tidyverse)

install()
document()
load_all()

example_data <- tibble(
  x = rnorm(300, 20, 10),
  y = rnorm(300, 5, 2),
  z = rnorm(300, 10, 4)
) %>% mutate(y = y * x ^ 2)

modify_data <- TRUE

bottle <- bottle_code({
  
  if(isTRUE(modify_data)) {
    prep_data <- example_data %>%
      mutate(y = y * 10)
  }
  
  p <- ggplot(prep_data, aes(x, y)) + 
    geom_point()
  
  p + theme_bw()
  
}, debug = TRUE)

bottle

run_bottle(bottle)

save_bottle(bottle, "test_bottle.rds")


bottle <- load_bottle("test_bottle.rds")

bottle

run_bottle(bottle)


bottle_keywords('{

  # Doing something here
  "Some very long string"

  1 + 1 # Math

  \'Some other string\'

}')


document()
load_all()

crate <- Crate$new()

crate$bottle("A")({
    
    1 + 1

})

crate$run_bottle("A")
crate$get_bottle("A")

crate$bottle("A", "B")({

    x <- 50 + 50

    x * 100

})

crate$run_bottle("A", "B")
crate$get_bottle("A", "B")

modify_data <- FALSE

crate$bottle("example_data", "figure")({
  
  if(isTRUE(unshare(modify_data))) {
    prep_data <- example_data %>%
      mutate(y = y * 10)
  } else {
    prep_data <- example_data
  }
  
  p <- ggplot(prep_data, aes(x, y)) + 
    geom_point()
  
  p + theme_bw()
  
})

crate$get_bottle("example_data", "figure")
crate$run_bottle("example_data", "figure")

crate


crate$save("crate.rda")

crate2 <- Crate$new(filename = "crate.rda")

big_number <- 10000000000000000000000000000

crate2$add_to_env(big_number)

ls(parent.env(crate2$get_bottle("A", "B")$env))


get("big_number", envir=crate2$get_bottle("A", "B")$env)
