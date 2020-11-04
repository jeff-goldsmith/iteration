Iteration and listcols
================

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.12769 -0.63660 -0.15120 -0.04492  0.63332  2.31461

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.175347 3.595095 2.752129 1.297273 2.928646 2.317817 3.552649 3.528090
    ##  [9] 4.496342 5.324014 3.902048 5.788163 2.544928 2.746764 4.970880 3.341827
    ## [17] 1.581525 3.780612 4.746921 3.865006
    ## 
    ## $b
    ##  [1]   6.0598405   8.3723857  -1.2088528   1.1168816   2.8802333   1.5989895
    ##  [7]   0.7957364   4.4702168   2.7909586  -4.9241105   1.7605236   1.2184304
    ## [13] -13.9910022   0.2004649  -4.2369023  -1.1697219   8.2345186   1.6515154
    ## [19]  -5.0858668  -0.9199613   2.1166912   2.9317397   5.5780165   3.4555608
    ## [25]  -3.5397710  -4.1038225  -5.1419829   3.2126681   8.4686332  -4.2798544
    ## 
    ## $c
    ##  [1]  9.687363  9.974123  9.917949  9.797047 10.304855 10.036357 10.055766
    ##  [8]  9.906203 10.392136 10.190249  9.620888 10.221870 10.314423  9.817361
    ## [15] 10.165507 10.087779  9.930963  9.743500 10.261497 10.183134  9.944012
    ## [22] 10.104345  9.982766  9.972785  9.798116  9.612825 10.131560 10.036401
    ## [29] 10.015287  9.722249 10.273640  9.583531  9.872491  9.742062 10.199013
    ## [36] 10.119293  9.965590  9.989900  9.832484 10.171104
    ## 
    ## $d
    ##  [1] -3.875892 -3.577021 -1.849255 -2.882591 -3.624689 -3.004652 -4.068159
    ##  [8] -1.571878 -2.954183 -3.097359 -5.097205 -5.141018 -3.789460 -2.846545
    ## [15] -3.276969 -2.325195 -2.851383 -2.057745 -4.127542 -2.317854

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.46  1.21

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.610  4.88

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.210

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22 0.970

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])

}
```

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-09-25 14:56:47 (7.519)

    ## file min/max dates: 1869-01-01 / 2020-09-30

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-09-25 14:56:52 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-09-25 14:56:54 (0.877)

    ## file min/max dates: 1999-09-01 / 2020-09-30
