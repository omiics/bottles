library(devtools)
library(tidyverse)

install()
document()
load_all()

example_data <- tibble(
  x = rnorm(300, 20, 10),
  y = rnorm(300, 5, 2),
  z = rnorm(300, 10, 4)
) %>% mutate(y = y * 0.4 * x)

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
