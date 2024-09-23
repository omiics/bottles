# Test an old test bottle

    Code
      print(bottle)
    Output
      
                       [ ]
                     .-' '-.
      R Code Bottle  :-...-:
                 v1  |     |
                     |     |
                     `-...-' 
    Message
      
      -- Bottled code: ---------------------------------------------------------------
      {
          if (isTRUE(modify_data)) {
              prep_data <- example_data %>% mutate(y = y * 10)
          }
          p <- ggplot(prep_data, aes(x, y)) + geom_point()
          p + theme_bw()
      }
      
      -- Bottled packages: -----------------------------------------------------------
      dplyr and ggplot2
      
      -- Bottled environment: --------------------------------------------------------
      example_data and modify_data

# Creation, running and saving bottles works

    Code
      bottle_code({
        x <- tribble(~x, ~y, ~z, 1, 2, 3, 2, 3, 4, 3, 4, 5)
        x$x <- x$x * 2
        x
      }, debug = TRUE)
    Message
      i Running bottle_code debug mode!
      i Detecting keywords
      i Found keywords: tribble and x
      i Active variables: 
      i Remaining keywords: tribble and x
      i Auto detecting keywords
      i Checking keyword: tribble
      i Current packages: 
      i Comes from package: tibble
      i Passed checks will add package name to packages
      i Checking keyword: x
      i Current packages: tibble
      i Comes from package: 
      i Reducing to unique package names
      i Final detected packages: tribble and x
    Output
      
                       [ ]
                     .-' '-.
      R Code Bottle  :-...-:
                 v1  |     |
                     |     |
                     `-...-' 
    Message
      
      -- Bottled code: ---------------------------------------------------------------
      {
          x <- tribble(~x, ~y, ~z, 1, 2, 3, 2, 3, 4, 3, 4, 5)
          x$x <- x$x * 2
          x
      }
      
      -- Bottled packages: -----------------------------------------------------------
      tibble
      
      -- Bottled environment: --------------------------------------------------------
      

