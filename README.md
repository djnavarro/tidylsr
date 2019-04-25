lsr2
================
Danielle Navarro
3 March 2019

[![Travis build
status](https://travis-ci.org/djnavarro/lsr2.svg?branch=master)](https://travis-ci.org/djnavarro/lsr2)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/lsr2/branch/master/graph/badge.svg)](https://codecov.io/gh/djnavarro/lsr2?branch=master)

Rethinking the `lsr` package that accompanies [Learning Statistics with
R](https://learningstatisticswithr.com). At the moment this is purely
experimental, and is as much an exercise in teaching myself package
development as anything else. It’s an absurdity that I’ve never learned
`roxygen2` until now\!

Looking back, my goal in the package was not to introduce any new
functionality to R, but rather to provide wrappers and simplifications
that novice users (e.g., psychology undergraduate students who might be
encountering R, statistics and programming for the first time ever)
might find handy.

Currently the only function in the `lsr2` package is `workspace()`,
which lists the contents of the calling environment in a fashion that is
more verbose than `objects()` but less elaborate than `ls.str()`

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

It wouldn’t be difficult to add a print method for this so that the
results look more similar to the simple display in the original package:

``` r
lsr::who()
```

    ##    -- Name --   -- Class --   -- Size --
    ##    keeper       list          2         
    ##    lover        numeric       1         
    ##    seeker       character     1
