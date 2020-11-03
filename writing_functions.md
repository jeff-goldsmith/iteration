Writing functions
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.76372114  0.20805475  0.33735918 -0.16005478 -1.24174695  0.42594434
    ##  [7]  1.64395183  0.08977848 -2.16376417  0.22713952  0.92395044 -0.54917530
    ## [13]  2.62085485 -0.75695122 -0.17105616 -0.78797787 -0.20453702  1.07320381
    ## [19]  0.36173274 -1.57244214  0.15195610 -0.08004848 -1.08531135  0.37284912
    ## [25]  1.43429476  0.84305469 -1.07015573  0.13487658  0.47694051 -0.71899939

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.76372114  0.20805475  0.33735918 -0.16005478 -1.24174695  0.42594434
    ##  [7]  1.64395183  0.08977848 -2.16376417  0.22713952  0.92395044 -0.54917530
    ## [13]  2.62085485 -0.75695122 -0.17105616 -0.78797787 -0.20453702  1.07320381
    ## [19]  0.36173274 -1.57244214  0.15195610 -0.08004848 -1.08531135  0.37284912
    ## [25]  1.43429476  0.84305469 -1.07015573  0.13487658  0.47694051 -0.71899939

Try my function on some other things. These should give
    errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```
