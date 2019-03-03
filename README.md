lsr2
================
Danielle Navarro
3 March 2019

Rethinking the lsr package that accompanies Learning Statistics with R. At the moment this is purely experimental, and is as much an exercise in teaching myself package development as anything else.

Currently the only function in the package is `workspace()`, which lists the contents of the calling environment in a fashion that is more verbose than `objects()` but less elaborate than `ls.str()`

``` r
# some variables for the workspace
seeker <- "hello"
lover <- 10
keeper <- list(seeker, lover)

# now print a summary
workspace()
```

    ## # A tibble: 3 x 3
    ##   name   class     length
    ##   <chr>  <chr>      <int>
    ## 1 keeper list           2
    ## 2 lover  numeric        1
    ## 3 seeker character      1
