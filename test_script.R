library(devtools)
library(tidyverse)

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
  
})

bottle

run_bottle(bottle)

save_bottle(bottle, "test_bottle.rds")


bottle <- load_bottle("test_bottle.rds")

bottle

run_bottle(bottle)

bottle_keywords("
xlimits <- c(-max(abs(results$log2FoldChange)), max(abs(results$log2FoldChange)))

ggplot2::ggplot(test_data, ggplot2::aes(x = x, y= y)) + ggplot2::geom_point()
")
