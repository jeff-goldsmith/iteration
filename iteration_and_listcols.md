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
    ## -2.78520 -0.79210  0.03987 -0.00304  0.73977  2.46113

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
    ##  [1] 3.0585484 2.6192745 2.5975848 5.2470508 3.0455744 2.2170078 3.5211656
    ##  [8] 1.7879052 3.4271437 3.0998004 0.6871937 2.7981303 3.9663828 3.3481550
    ## [15] 1.8834822 4.0116375 2.0944356 3.1447049 4.3719055 3.5796134
    ## 
    ## $b
    ##  [1]  -4.8074596  -6.0424147   1.1670069   5.3958304  -0.3102753   2.0381120
    ##  [7]  -2.2820274  -3.0604536   4.2393922  -0.5876631  -1.0734071  -1.4576839
    ## [13]   8.4748120  -3.4875515 -10.4340215  -0.2230290   4.1844966   1.1028647
    ## [19]  -1.5169431   6.3186650   2.5781811  -1.5733467   6.2196785  -3.8202925
    ## [25]  -1.9963079  -1.1966075  -5.3012012   0.0926896  -3.9307885   3.5017699
    ## 
    ## $c
    ##  [1] 10.065049  9.615874 10.038859 10.236067 10.002497 10.097979  9.930618
    ##  [8] 10.205704  9.974230 10.063264  9.973340  9.883668 10.234551 10.089252
    ## [15] 10.367021  9.926598  9.841406  9.933750  9.986943  9.617611 10.065213
    ## [22]  9.700373  9.619431 10.098335 10.198275  9.957559 10.082742 10.185230
    ## [29]  9.866101 10.344799  9.411464 10.048283 10.076431  9.845248  9.924535
    ## [36]  9.728497 10.311225 10.207263  9.895191  9.874394
    ## 
    ## $d
    ##  [1] -2.072697 -2.085664 -4.451077 -5.224156 -4.977017 -2.516965 -3.882358
    ##  [8] -3.905150 -1.639357 -3.907215 -2.028006 -3.531700 -1.430452 -2.978461
    ## [15] -1.106032 -3.441027 -2.976015 -2.041066 -3.089621 -1.909718

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
    ## 1  3.03  1.02

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.260  4.18

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.212

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96  1.19

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])

}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function..?

``` r
output = map(list_norm, median)
```

``` r
output = map_dbl(list_norm, median, .id = "input")
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 3.0585484 2.6192745 2.5975848 5.2470508 3.0455744 2.2170078 3.5211656
    ##  [8] 1.7879052 3.4271437 3.0998004 0.6871937 2.7981303 3.9663828 3.3481550
    ## [15] 1.8834822 4.0116375 2.0944356 3.1447049 4.3719055 3.5796134
    ## 
    ## $b
    ##  [1]  -4.8074596  -6.0424147   1.1670069   5.3958304  -0.3102753   2.0381120
    ##  [7]  -2.2820274  -3.0604536   4.2393922  -0.5876631  -1.0734071  -1.4576839
    ## [13]   8.4748120  -3.4875515 -10.4340215  -0.2230290   4.1844966   1.1028647
    ## [19]  -1.5169431   6.3186650   2.5781811  -1.5733467   6.2196785  -3.8202925
    ## [25]  -1.9963079  -1.1966075  -5.3012012   0.0926896  -3.9307885   3.5017699
    ## 
    ## $c
    ##  [1] 10.065049  9.615874 10.038859 10.236067 10.002497 10.097979  9.930618
    ##  [8] 10.205704  9.974230 10.063264  9.973340  9.883668 10.234551 10.089252
    ## [15] 10.367021  9.926598  9.841406  9.933750  9.986943  9.617611 10.065213
    ## [22]  9.700373  9.619431 10.098335 10.198275  9.957559 10.082742 10.185230
    ## [29]  9.866101 10.344799  9.411464 10.048283 10.076431  9.845248  9.924535
    ## [36]  9.728497 10.311225 10.207263  9.895191  9.874394
    ## 
    ## $d
    ##  [1] -2.072697 -2.085664 -4.451077 -5.224156 -4.977017 -2.516965 -3.882358
    ##  [8] -3.905150 -1.639357 -3.907215 -2.028006 -3.531700 -1.430452 -2.978461
    ## [15] -1.106032 -3.441027 -2.976015 -2.041066 -3.089621 -1.909718

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.03  1.02

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.260  4.18

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.03  1.02
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.260  4.18
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.212
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.96  1.19

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
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
