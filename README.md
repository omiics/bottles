# Bottles - Containerize code snippets <img border="0" src="man/figures/logo.png" alt="The 'bottles' hexlogo" align="right" style="width: 100px"/>

<!-- badges: start -->
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  <!-- badges: end -->

## Purpose

The main purpose of the tool is to provide an easy what of exporting a small snippet of code else where provided the differences if the installed package versions are compatible.

Provides a very simple tool for exporting/saving code while saving the minimal amount of environment data and packages required to be able to rerun the code.

The main use of the package currently has been to export code snippets for generating ggplots, to provide some independence from package versions of the ggplot objects when exporting figures to be editted later.

This tool is not a replacement packages for modular reusable code like [box](https://klmr.me/box/).

## Installation

The package tries to have as few dependencies to external packages as possible relying on base R to work.

The package can be installed through github.

``` r
remotes::install_github("omiics/bottles")
```


## Basic usage

Create a bottle by putting in any R expression code block.

``` r
library(bottles)

bottle <- bottle_code({
  ggplot(dataset, aes(x = samples, y = counts)) + geom_point()
})

```

The returned object is now a bottle object. Which can be saved to file using `save_bottle` or simply `saveRDS`.

``` r
save_bottle(bottle, "bottle_file.rds")
```

The bottle can then in a later session, or on another machine been read again, and then it bottled code can be run:

``` r
bottle <- load_bottle("bottle_file.rds")

run_bottle(bottle)
```

## How is it bottled?

The expression given to `bottle_code` will simply convert the expression to a string, that can be evaluated later. To make the code more reproducible it will do a quick 'analysis' of the provided code and look for all of the variables and function calls that are included in the expression.

All of the variables are extracted from the current environment, and placed in a new environment including only the variables that where used in the expression.

Each of the functions will be checked for which package/namespace they are from and it will add it to the packages that should be loaded into the bottle.

The bottle is a simple list with a S3 class called `bottle`, which allows the functions of the package to recognize that it what provided a bottle. In the background it is basically a list with 3 elements, code, env and packages.

The bottle could be improved further by considering conflicting package versions, and using more specific imports using box, but for now it will basically limit it self to whole packages. The keywords detected in the code will ignore specific namespace calls like: `ggplot2::ggplot`.

When running the bottle will load all required packages in a similar way to withr's `with_package`, and will unload the package once it has finished (except if the package is already loaded).

### Bottle structure

Printing a bottle shows you the structure of the bottle in a prettified way making it easy to get an overview of what was included in the bottle. 

```
                 [ ]
               .-' '-.
R Code Bottle  :-...-:
           v1  |     |
               |     |
               `-...-' 

── Bottled code: ───────────────────────────────────────────────────────────────────
{
    if (isTRUE(modify_data)) {
        prep_data <- example_data %>% mutate(y = y * 10)
    } else {
        prep_data <- example_data
    }
    p <- ggplot(prep_data, aes(x, y)) + geom_point()
    p + theme_bw()
}

── Bottled packages: ───────────────────────────────────────────────────────────────
dplyr & ggplot2

── Bottled environment: ────────────────────────────────────────────────────────────
modify_data & example_data
```

## Crates - share data between bottles

When working with multiple bottles and saving them to disk there can be an overlap in the stored data in the environments. Therefore we created a data structure called a Crate, which can store multiple bottles inside and share data between them. This can make storing the bottles for later use much more efficient as you don't need to have duplicated data stored on disk.

The Crate is implemented as an R6 class and can be created with the `$new()` function.

```r
crate <- Crate$new()
```

To bottle some code you can use the `$bottle` function. Since you can store multiple bottles you need an ID to reference particular bottles. Here is an example of how to bottle some code:

```r

crate$bottle("Analysis", "Scatterplot")({

  ggplot(example_data, aes(x = Age, y = Height)) +
    geom_point() + theme_bw()

})

```

A short hand to remember the structure is `$bottle(...)({ code })`. First you input the values that make up the id, which returns a function that will insert the provided code as an item into the crate.

### Sharing data between bottles

By default when adding bottle to the crate it will automatically collect all of the variables used and add them to the shared environment. However any duplicates of the variables will be ignored and a warning will be produced.

The proper way to handle shared data is to first add the data to the crate using `$add_data()`, and then using `shared()` in the bottle to denote that the variable is already shared in the crate

```r

crate$add_data(big_dataset)

crate$bottle("Analysis", "Scatterplot")({

  ggplot(shared(big_dataset), aes(x = Age, y = Height)) +
    geom_point() + theme_bw()

})

```

The opposite case can also happen where you do not want a variable to be shared in the crate, and should only be local to the specific bottle. This can be variables from a for loop generating multiple bottles.

Here you can use the `unshare()` function to mark a variable as something that should be ignored, but still kept in the bottles own environment. A use case is in a for loop where you want to generate figures based on filters placed on a large shared dataset.

```r

crate$add_data(big_dataset)

for (country in countries) {

  crate$bottle("Analysis", country, "Scatterplot")({

    shared(big_dataset) %>%
      filter(country == !!unshare(country)) %>%
      ggplot(aes(x = Age, y = Height)) +
        geom_point() + theme_bw()

  })

}

```

Finally you can use the `ignore()` function to get the bottle to completely ignore the variable. This can be useful if you get some variables added you do not want to be present.

### Running bottles

To run a bottle you can use the `$run_bottle()` function where you provide the same input id you used to bottle it with.

```r

result <- crate$run_bottle("Analysis", "Scatterplot")

```

Similarily you can use the `$get_bottle()` function to get the bottle as a standalone object. It still has access to the shared environment in the Crate, as long as the crate still exists.

```r

bottle <- crate$get_bottle("Analysis", "Scatterplot")

```

### Saving and loading Crates

A crate can be saved using the `$save()` function. The file is stored as a RData file which perserves the whole structure of the crate.

```r

crate$save('crate.rda')

```

To load from file again you can initialize the Crate using the `$new()` function and provide a filename to read from.

```r

crate <- Crate$new(filename = 'crate.rda')

```