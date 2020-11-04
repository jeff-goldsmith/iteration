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
    ## -2.48912 -0.78681 -0.03745 -0.03115  0.75128  2.55874

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
    ##  [1] 4.044114 2.744317 3.890185 1.981409 3.322901 3.119868 3.643869 3.832954
    ##  [9] 2.529891 3.959648 3.114440 3.681820 3.964164 1.940920 3.729074 3.164881
    ## [17] 3.335570 3.703325 3.461591 4.092904
    ## 
    ## $b
    ##  [1] -0.8128821  5.0297361  2.0689000  3.5635723 -6.4453476  2.9666816
    ##  [7] -0.1618678  9.1162682  0.3301938 -0.2857063 -1.2186488 -2.5851225
    ## [13] -4.7871073 -0.4187519 -0.9632436 -6.0495591 -9.9124294  1.6698043
    ## [19]  0.9395444  3.5608162  8.8306135  0.7896454 -0.4975230 -8.2158751
    ## [25] -1.7741081 -0.8735917 -5.7305450 -7.7151488 -2.1200699  1.0023457
    ## 
    ## $c
    ##  [1]  9.856291 10.325700 10.430023  9.890921 10.347601  9.688765  9.867367
    ##  [8]  9.967002  9.926390 10.083438  9.880957 10.102762 10.257369  9.739691
    ## [15] 10.283676 10.568747 10.065834 10.187251 10.034923 10.122398 10.216898
    ## [22]  9.832633 10.214824 10.384198  9.961497 10.054725 10.097951  9.662763
    ## [29] 10.160709  9.855272  9.906264 10.122551  9.987141 10.134726  9.744519
    ## [36]  9.687810 10.109496  9.921879 10.018819  9.867411
    ## 
    ## $d
    ##  [1] -1.4447940 -2.2125560 -1.9277919 -2.1905519 -5.8059625 -1.5540742
    ##  [7] -4.3504487 -2.2178042 -4.2384656 -2.9143909 -3.3727411 -2.7167141
    ## [13] -3.6335427 -3.7787981 -3.2294114 -0.4796289 -1.7071612 -3.7562285
    ## [19] -2.7585502 -3.1931269

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
    ## 1  3.36 0.642

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.690  4.53

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.216

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.87  1.22

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
