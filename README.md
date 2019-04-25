lsr2
================
Danielle Navarro
25 April 2019

[![Travis build
status](https://travis-ci.org/djnavarro/lsr2.svg?branch=master)](https://travis-ci.org/djnavarro/lsr2)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/lsr2/branch/master/graph/badge.svg)](https://codecov.io/gh/djnavarro/lsr2?branch=master)

Rethinking the `lsr` package that accompanies [Learning Statistics with
R](https://learningstatisticswithr.com). In the original book, the goal
of the package was to provide a few convenient wrapper functions and
simplifications that novice users might find handy. A typical reader of
the book might be psychology undergraduate students who encountering R,
statistics and programming for the first time, and I found the
simplifications useful in some cases.

The `lsr2` package (working title\!\!) is intended to accompany the next
version of *Learning Statistics with R*, in which the goal is to teach a
tidyverse-focused data analysis pipeline. Currently the only function in
`lsr2` is `workspace()`, which lists the contents of the calling
environment in a fashion that is more verbose than `objects()` but less
elaborate than `ls.str()`

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
results look more similar to the simple display in the original package,
but I am not yet sure if this is worthwhile.

``` r
lsr::who()
```

    ##    -- Name --   -- Class --   -- Size --
    ##    keeper       list          2         
    ##    lover        numeric       1         
    ##    seeker       character     1
