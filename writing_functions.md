Writing functions
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.88410219 -0.71144955 -0.94257337  0.86920129  0.83932731 -0.11481861
    ##  [7]  1.53478053  1.12815676  0.28995180 -1.85760798  0.60688339 -1.37886137
    ## [13] -0.47088200 -0.40473479 -1.36471674 -1.21605756 -0.48908548 -1.14575341
    ## [19]  0.06787354 -0.12039353 -0.06101009  0.47389800  1.47545795  2.28557626
    ## [25] -0.08799866  0.36440171  0.96095493  0.22624425  1.03003242 -0.90269482

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

    ##  [1] -0.88410219 -0.71144955 -0.94257337  0.86920129  0.83932731 -0.11481861
    ##  [7]  1.53478053  1.12815676  0.28995180 -1.85760798  0.60688339 -1.37886137
    ## [13] -0.47088200 -0.40473479 -1.36471674 -1.21605756 -0.48908548 -1.14575341
    ## [19]  0.06787354 -0.12039353 -0.06101009  0.47389800  1.47545795  2.28557626
    ## [25] -0.08799866  0.36440171  0.96095493  0.22624425  1.03003242 -0.90269482

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

## Multiple outputs

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

Check that the function works.

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.28  4.11

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.10  3.39

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.60  2.93

``` r
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.52  3.01

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.96  4.40

## Let’s review Napoleon Dynamite

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

What about the next page of reviews…

Let’s turn that code into a function

``` r
read_page_reviews = function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  reviews
  
}
```

Let me try my
function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                  stars       text                                      
    ##    <chr>                  <chr>       <chr>                                     
    ##  1 Great purchase price … 5.0 out of… "Great movie and real good digital purcha…
    ##  2 Movie for memories     5.0 out of… "I've been looking for this movie to buy …
    ##  3 Love!                  5.0 out of… "Love this movie. Great quality"          
    ##  4 Hilarious!             5.0 out of… "Such a funny movie, definitely brought m…
    ##  5 napoleon dynamite      5.0 out of… "cool movie"                              
    ##  6 Top 5                  5.0 out of… "Best MOVIE ever! Funny one liners and ju…
    ##  7 👍                     5.0 out of… "Exactly as described and came on time 👍"
    ##  8 A top favorite movie … 5.0 out of… "Love this movie, needed to add it to my …
    ##  9 Best.Movie!            5.0 out of… "I enjoyed showing my children this \"cla…
    ## 10 Great Movie            5.0 out of… "I love this movie. Showed it to my middl…

Let’s read a few pages of
reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:50)

all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 1.846461

``` r
median(x_vec)
```

    ## [1] 2.552178

``` r
my_summary(x_vec, IQR)
```

    ## [1] 10.19683
