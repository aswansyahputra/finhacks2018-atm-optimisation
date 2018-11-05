# Executive summary
The main goal set in this activity was to forecast the cash withdrawals in certain ATM machines at certain time period using set of historical dataset. During the pre-processing, quite number of variables ware omitted while two new variables were introduced, i.e. paydays and weekend as regressor. Fast Additive Switching of Seasonality, Trend and Exogenous Regressors (FASSTER) model was used to link the available variable and forecast the withdrawals. In this report, we present the steps which we followed from data cleaning, wrangling, forecasting, valiadation, and measuring model performance. The performance metrics show that the model has a quite good predictive properties. Unfortunately, for some reasons that we yet failed to unreveal, the scaling to the full dataset could not be performed. An error was persistly popped up eventough we have tried to thoroughly debug it. We suspect that by a slim chance that it was occured since the fasster package (that we used for building the model) is still in active development phase. Thus, we could not supplement the forecasting on the real testing dataset provided by Finhacks.

# Data cleansing and wrangling

```r
raw_training <- read_csv("/home/aswansyahputra/Projects/Others/finhacks/atm-cash-optim/data_input/atm_train.csv") # import dataset as tibble instead of dataframe format

# Rename non-english column names for consistency
cln_training <-
  raw_training %>%
  `colnames<-`(., str_to_lower(colnames(.))) %>%
  `colnames<-`(., str_replace_all(colnames(.), "[:punct:]", "_")) %>%
  rename(
    atm_code = `no_ atm`,
    opening_balance = `saldo awal`,
    closing_balance = `saldo akhir`,
    idle_money = uang_idle
  ) %>%
  as_tsibble(index = date, key = id(atm_code)) # convert tibble into tsibble
```

In this section, we performed cleansing and wrangling on training dataset which provided by Finhacks 2018 commitee. It is important to mention that the original dataset has mixed-language naming on its columns, thus we renamed some of non-english column names into the equivalent english term for consistency purpose. The insight of the training dataset is provided below.


```r
glimpse(cln_training)
## 2018-01-01 ~ 2018-03-24
## Observations: 881,816
## Variables: 17
## $ x1                   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13...
## $ atm_code             <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1",...
## $ date                 <date> 2018-01-01, 2018-01-02, 2018-01-03, 2018...
## $ currency             <chr> "IDR", "IDR", "IDR", "IDR", "IDR", "IDR",...
## $ opening_balance      <int> 275600000, 196900000, 397800000, 28910000...
## $ deliveries           <int> 0, 460000000, 0, 0, 460000000, 0, 0, 4600...
## $ returns              <int> 0, 146200000, 0, 0, 138000000, 0, 0, 1534...
## $ unplanned_deliveries <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ unplanned_returns    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ deposit              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ pre_withdrawals      <int> 0, 50700000, 0, 0, 50400000, 0, 0, 562000...
## $ withdrawals          <int> 78700000, 112900000, 108700000, 100700000...
## $ closing_balance      <int> 196900000, 397800000, 289100000, 18840000...
## $ trips                <int> 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,...
## $ balance_cost         <int> 32367, 65392, 47523, 30970, 62901, 45805,...
## $ carrier_cost         <int> 0, 350000, 0, 0, 350000, 0, 0, 350000, 0,...
## $ idle_money           <int> 45304, 32367, 65392, 47523, 30970, 62901,...
```

The training dataset consisted of 881,816 observation with 17 variables (in fact, it is 16 variabels as we can ignore x1 column which served as rownames). Speaking of structure, the dataset is tabulated as timeseries data with 10626 ATM machine (indicated as 'atm_code') as key unit of observation. The observation on each ATM machine was conducted from 2018-01-01 until 2018-03-24.


```r
raw_testing <- read_csv("/home/aswansyahputra/Projects/Others/finhacks/atm-cash-optim/data_input/atm_test.csv") # import dataset as tibble instead of dataframe format

cln_testing <-
  raw_testing %>%
  `colnames<-`(., str_to_lower(colnames(.))) %>%
  `colnames<-`(., str_replace_all(colnames(.), "[:punct:]", "_")) %>%
  rename(atm_code = `no_ atm`) %>%
  mutate(date = dmy(date)) %>%
  select(date, atm_code) %>%
  as_tsibble(index = date, key = id(atm_code)) # convert tibble into tsibble
```

We aimed to forecast the withdrawals in certain ATM machine on certain date using the other variables available. Forecasting will be performed on the provided testing dataset which contained predefined date (from 2018-03-25 until 2018-03-31) for each ATM machine (total of 10626 ATM machine in atm_code column). The insight of the testing dataset is presented below. 


```r
glimpse(cln_testing)
## 2018-03-25 ~ 2018-03-31
## Observations: 74,381
## Variables: 2
## $ date     <date> 2018-03-25, 2018-03-26, 2018-03-27, 2018-03-28, 2018...
## $ atm_code <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K10", "K10...
```

The most note-taking property of the testing dataset that it only contain two variables, i.e. the date and atm_code. No information about other variables as in the training dataset available! Therefore, it will be assumptious to train a model that containing the non-available variables in the testing dataset. Finally, we decided to drop other variables and only kept date, atm_code, and withdrawals variables in the training dataset.


```r
cln_training <-
  cln_training %>%
  select(date, atm_code, withdrawals)
```

Since a major number of variables were dropped, we tried to extent the date variable into another information which may be useful for forecasting. Based on the a priori knowledge, we introduced and hypotised two ideas:

* the withdrawals during paydays period is different with the others date within a month

* the withdrawals during weekends is different with weekdays

For paydays period we chose the first three days of date 1 and 20 every month, so it will be date 1, 2, 3, 20, 21, and 22. For weekends we defined Saturday and Sunday as weekend. We introduce the paydays and weekend variables onto both the training and testing dataset.


```r
cln_training <-
  cln_training %>%
  mutate(
    paydays = if_else(day(date) %in% c(1, 2, 3, 20, 21, 22), "TRUE", "FALSE"),
    weekend = as.character(timeDate::isWeekend(date))
  ) # Note that we used "TRUE" and "FALSE" as character vector instead of logical vector
glimpse(cln_training)
## 2018-01-01 ~ 2018-03-24
## Observations: 881,816
## Variables: 5
## $ date        <date> 2018-01-01, 2018-01-02, 2018-01-03, 2018-01-04, 2...
## $ atm_code    <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K...
## $ withdrawals <int> 78700000, 112900000, 108700000, 100700000, 1277500...
## $ paydays     <chr> "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE",...
## $ weekend     <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE...
```


```r
cln_testing <-
  cln_testing %>%
  mutate(
    paydays = if_else(day(date) %in% c(1, 2, 3, 20, 21, 22), "TRUE", "FALSE"),
    weekend = as.character(timeDate::isWeekend(date))
  ) # Note that we used "TRUE" and "FALSE" as character vector instead of logical vector
glimpse(cln_testing)
## 2018-03-25 ~ 2018-03-31
## Observations: 74,381
## Variables: 4
## $ date     <date> 2018-03-25, 2018-03-26, 2018-03-27, 2018-03-28, 2018...
## $ atm_code <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K10", "K10...
## $ paydays  <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE",...
## $ weekend  <chr> "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ...
```

# Exploratory data analysis
Firstly, we looked into the distribution of the withdrawals variable using histogram as depicted in following figure. 


```r
ggplot(cln_training, aes(x = withdrawals)) +
  geom_histogram() +
  labs(title = "The distribution of withdrawals is tend to be left-skewed")
```

<img src="ATM_report_2_files/figure-html/histogram-1.png" width="80%" style="display: block; margin: auto;" />

However, the figure shows that the distribution of withdrawals is left-skewed. A small number of high withdrawals in observation will make it harder to forecast the smaller withdrawals. Therefore, we applied square-root transformation to treat the data better while keeping the order in values intact (note that square-root is a monotonic transformation). We did not use logarithmic transformation as it will introduce infinite values in dataset. The histogram of the transformed withdrawals values is illustrated in following figure.


```r
ggplot(cln_training, aes(x = withdrawals)) +
  geom_histogram() +
  scale_x_sqrt() +
  labs(
    title = "The distribution of transformed withdrawals is quite centered",
    subtitle = "Square-root transformation on original withdrawals values",
    x = "square-root of withdrawals"
  )
```

<img src="ATM_report_2_files/figure-html/trans-histogram-1.png" width="80%" style="display: block; margin: auto;" />

It is shown that after the tranformation the distribution of withdrawals became better. A quite centered bell-shaped distribution is obtained eventough a small number of high withdrawals still a little bit skews the histogram. Therefore, we introduced square-root tranformation on the training dataset and used sqrt_withdrawals for analysis except mentioned otherwise.


```r
cln_training <-
  cln_training %>%
  mutate(sqrt_withdrawals = sqrt(withdrawals)) %>%
  select(date, atm_code, paydays, weekend, withdrawals, sqrt_withdrawals)
glimpse(cln_training)
## 2018-01-01 ~ 2018-03-24
## Observations: 881,816
## Variables: 6
## $ date             <date> 2018-01-01, 2018-01-02, 2018-01-03, 2018-01-...
## $ atm_code         <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K1...
## $ paydays          <chr> "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FA...
## $ weekend          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ...
## $ withdrawals      <int> 78700000, 112900000, 108700000, 100700000, 12...
## $ sqrt_withdrawals <dbl> 8871.302, 10625.441, 10425.929, 10034.939, 11...
```

Subsequently, we inspected the relationship between paydays and weekend variables that we had created with sqrt_witdrawals. This is the descriptive statistic sqrt_withdrawals grouped by whether it is paydays period:


```r
cln_training %>%
  as_tibble() %>% # return tsibble back to tibble
  group_by(paydays) %>%
  summarise(
    n = n(),
    mean = mean(sqrt_withdrawals),
    sd = sd(sqrt_withdrawals)
  )
## # A tibble: 2 x 4
##   paydays      n   mean    sd
##   <chr>    <int>  <dbl> <dbl>
## 1 FALSE   690579 10169. 3764.
## 2 TRUE    191237 10356. 3900.
```

and this is the descriptive statistic sqrt_withdrawals grouped by whether it is weekend or not:


```r
cln_training %>%
  as_tibble() %>% # return tsibble back to tibble
  group_by(weekend) %>%
  summarise(
    n = n(),
    mean = mean(sqrt_withdrawals),
    sd = sd(sqrt_withdrawals)
  )
## # A tibble: 2 x 4
##   weekend      n   mean    sd
##   <chr>    <int>  <dbl> <dbl>
## 1 FALSE   637456 10341. 3715.
## 2 TRUE    244360  9868. 3976.
```

It is suggested that the amount of money withdrawn from ATM machine was slightly higher during paydays period, while it was smaller during the weekend. To further investigate the differences, inferential statistic using two sample t-test then performed for paydays period:


```r
cln_training %>%
  as_tibble() %>%
  ntbt_t.test(sqrt_withdrawals ~ paydays) %>%
  tidy()
## # A tibble: 1 x 10
##   estimate estimate1 estimate2 statistic  p.value parameter conf.low
##      <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>
## 1    -187.    10169.    10356.     -18.7 3.88e-78   297131.    -207.
## # ... with 3 more variables: conf.high <dbl>, method <chr>,
## #   alternative <chr>
```

and for weekend variable:


```r
cln_training %>%
  as_tibble() %>%
  ntbt_t.test(sqrt_withdrawals ~ weekend) %>%
  tidy()
## # A tibble: 1 x 10
##   estimate estimate1 estimate2 statistic p.value parameter conf.low
##      <dbl>     <dbl>     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>
## 1     473.    10341.     9868.      50.9       0   417300.     455.
## # ... with 3 more variables: conf.high <dbl>, method <chr>,
## #   alternative <chr>
```

Both results show that the withdrawals differences during paydays period and the differences during weekend are both statistically significant (P<0.05). Thus, support our ideas that there are possibilites that the withdrawals would be different in certain period of time. Therefore, paydays and weekend variables then included in model building.

# Forecasting
## Data splitting
As the testing dataset provided by Finhacks commitee is not supplied with the true/real withdrawals values, therefore, we splitted the training dataset into n_training and n_testing for crossvalidation and measurement of model performance.

**Training data**

The sampled training dataset was obtained by taking observation on 1 January 2018 until 14 March 2018 from the original training dataset.


```r
set.seed(556)
# atm <- base::sample(unique(cln_training$atm_code), 2126, replace = FALSE)
atm <- unique(cln_training$atm_code)

n_training <- cln_training %>%
  filter(atm_code %in% atm) %>%
  filter(date < ymd("2018-03-15")) %>%
  select(-withdrawals) # remove withdrawals just to be more clear
glimpse(n_training)
## 2018-01-01 ~ 2018-03-14
## Observations: 775,560
## Variables: 5
## $ date             <date> 2018-01-01, 2018-01-02, 2018-01-03, 2018-01-...
## $ atm_code         <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K1...
## $ paydays          <chr> "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FA...
## $ weekend          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ...
## $ sqrt_withdrawals <dbl> 8871.302, 10625.441, 10425.929, 10034.939, 11...
```

**Testing data**

The sampled testing data was obtained by slicing original training dataset. In this report we demonstrate the forecast in sqrt_withdrawals on 15 March 2018 until 24 March 2018.


```r
n_testing <- cln_training %>%
  filter(atm_code %in% atm) %>%
  filter(date >= ymd("2018-03-15")) %>%
  select(-withdrawals) # remove withdrawals and sqrt_withdrawal just to be more clear
glimpse(n_testing)
## 2018-03-15 ~ 2018-03-24
## Observations: 106,256
## Variables: 5
## $ date             <date> 2018-03-15, 2018-03-16, 2018-03-17, 2018-03-...
## $ atm_code         <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K1...
## $ paydays          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ...
## $ weekend          <chr> "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "F...
## $ sqrt_withdrawals <dbl> 11445.523, 12114.041, 9189.668, 7921.490, 108...
```

## Model building

For forecasting the withdrawals, more pricesely the sqrt_withdrawals, we used Fast Additive Switching of Seasonality, Trend and Exogenous Regressors (FASSTER) model. We created model with trend and weekly seasonality, introduced switching on paydays as seasonality, and weekend as regressor. The model was created on each ATM machines independently.


```r
n_model <- n_training %>%
  fasster(
    sqrt_withdrawals ~ paydays %S% (poly(1) + trig(7) + weekend)
  )
n_model
## # A mable: 10,625 models [1D]
## # Key:     atm_code [10,625]
##    atm_code data               model  
##    <chr>    <list>             <model>
##  1 K1       <tsibble [73 × 4]> FASSTER
##  2 K10      <tsibble [73 × 4]> FASSTER
##  3 K100     <tsibble [73 × 4]> FASSTER
##  4 K1000    <tsibble [73 × 4]> FASSTER
##  5 K10000   <tsibble [73 × 4]> FASSTER
##  6 K10001   <tsibble [73 × 4]> FASSTER
##  7 K10002   <tsibble [73 × 4]> FASSTER
##  8 K10003   <tsibble [73 × 4]> FASSTER
##  9 K10004   <tsibble [73 × 4]> FASSTER
## 10 K10005   <tsibble [73 × 4]> FASSTER
## # ... with 10,615 more rows
```

Subsequently, we applied the model for forecasting the sampled testing data. The result is saved as prdiction variable.


```r
n_forecast <-
  n_model %>%
  forecast(new_data = n_testing) %>%
  select(forecast) %>%
  pull() %>%
  map(~select(.data = ., date, mean)) %>%
  map(as_tibble) %>%
  `names<-`(unique(n_model$atm_code)) %>%
  bind_rows(.id = "atm_code") %>%
  rename(prediction = mean) %>%
  as_tsibble(key = id(atm_code), index = date) %>%
  left_join(x = n_testing, y = ., by = c("date", "atm_code"))
glimpse(n_forecast)
## 2018-03-15 ~ 2018-03-24
## Observations: 106,256
## Variables: 6
## $ date             <date> 2018-03-15, 2018-03-16, 2018-03-17, 2018-03-...
## $ atm_code         <chr> "K1", "K1", "K1", "K1", "K1", "K1", "K1", "K1...
## $ paydays          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ...
## $ weekend          <chr> "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "F...
## $ sqrt_withdrawals <dbl> 11445.523, 12114.041, 9189.668, 7921.490, 108...
## $ prediction       <dbl> 10597.710, 10102.308, 10524.770, 7758.907, 10...
```

## Cross-validation and performance

First, we assesed the predictive properties of the model by regressing the prediction (from model forecast) to the actual sqrt_withdrawals values. It is suggested that the R^2^ value is 0.69 and the predicted sqrt_withdrawals values has predictive properties on actual sqrt_withdrawal valies as reflected by the p-value of F statistic (P<0.05).


```r
n_forecast %>%
  as_tibble() %>%
  ntbt_lm(sqrt_withdrawals ~ prediction) %>%
  glance()
## # A tibble: 1 x 11
##   r.squared adj.r.squared sigma statistic p.value    df  logLik    AIC
## *     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <int>   <dbl>  <dbl>
## 1     0.688         0.688 2047.   233909.       0     2 -9.61e5 1.92e6
## # ... with 3 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>
```

We also interested in assesing the correlation between the predicted and the actual sqrt_withdrawals value. The result shows that they have high-positive correlation of 0.83 (P<0.05). It support the previous regression's result.


```r
n_forecast %>%
  as_tibble() %>%
  ntbt_cor.test(sqrt_withdrawals, prediction) %>%
  glance()
## # A tibble: 1 x 8
##   estimate statistic p.value parameter conf.low conf.high method
##      <dbl>     <dbl>   <dbl>     <int>    <dbl>     <dbl> <chr> 
## 1    0.829      484.       0    106248    0.827     0.831 Pears…
## # ... with 1 more variable: alternative <chr>
```

Next, we also evaluate the residuals between the predicted and actual sqrt_withdrawals value. If the distribution of the residuals is centered around zero, it means that the model has good properties. Here is the histogram of the residuals:


```r
n_forecast %>%
  mutate(residual = sqrt_withdrawals - prediction) %>%
  ggplot(aes(x = residual)) +
  geom_histogram() +
  labs(
    title = "The residuals distribution is centered around zero",
    subtitle = "A small number of overestimation is observed"
  )
```

<img src="ATM_report_2_files/figure-html/unnamed-chunk-12-1.png" width="80%" style="display: block; margin: auto;" />

The histogram above shows that the performance of the model is quite good. However, we also notice that the distribution is a little bit right-skewed. Thus we inspected the pattern by plotting the residuals againts the actual sqrt_values.


```r
n_forecast %>%
  mutate(residual = sqrt_withdrawals - prediction) %>%
  ggplot(aes(x = sqrt_withdrawals, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(
    title = "A lot of failures when the true value of sqrt_withdrawals is zero",
    subtitle = "Overestimations mainly occured at smaller and higher value of sqrt_withdrawals"
  )
```

<img src="ATM_report_2_files/figure-html/unnamed-chunk-13-1.png" width="80%" style="display: block; margin: auto;" />

The pattern on the figure shows that when the sqrt_withdrawals are small, roughly below 5000, the model tends to overestimate the actual values. However, the general pattern is still proved that the residuals are distributed around zero. Finally, we calculated the score of the model if we used cut-off of 10% to tolerate the error rate. The final score will lies between 0 to 100, the closer the score to 100 the better the model is.


```r
scores <-
  n_forecast %>%
  as_tibble() %>%
  mutate(
    error = (sqrt_withdrawals - prediction) / sqrt_withdrawals
  ) %>%
  summarise(
    correct = sum(error[is.finite(error)] < 0.10), # number of prediction with error rate < 10%
    n = length(error),
    score = 100 * correct / n
  )
scores
## # A tibble: 1 x 3
##   correct      n score
##     <int>  <int> <dbl>
## 1   86008 106256  80.9
```

Surprisingly, we reached score of 80.94. It means that the model really did a good job in forecasting the withdrawals (as sqrt_withdrawals).

## Implementation on testing data
Now, we will extent the forecasting to the real testing dataset provided by Finhacks commitee. The procedure was exactly the same as the previous procedure. The only differences was that we using the full training dataset to build the model.


```r
full_model <-
  cln_training %>%
  fasster(
    sqrt_withdrawals ~ paydays %S% (poly(1) + trig(7) + weekend)
  ) %>%
  forecast(new_data = cln_testing)
```


```r
full_forecast <-
  full_model %>%
  forecast(new_data = cln_testing) %>%
  select(forecast) %>%
  pull() %>%
  map(~select(.data = ., date, mean)) %>%
  map(as_tibble) %>%
  `names<-`(unique(full_model$atm_code)) %>%
  bind_rows(.id = "atm_code") %>%
  rename(prediction = mean) %>%
  as_tsibble(key = id(atm_code), index = date) %>%
  left_join(x = cln_testing, y = ., by = c("date", "atm_code"))
glimpse(full_forecast)
```

# Session Info

```r
sessionInfo()
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.1 LTS
## 
## Matrix products: default
## BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## locale:
##  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
##  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
##  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2       fasster_0.1.0.9000   fablelite_0.0.0.9000
##  [4] tsibble_0.5.3        broom_0.5.0          intubate_1.0.0      
##  [7] timeDate_3043.102    lubridate_1.7.4      forcats_0.3.0       
## [10] stringr_1.3.1        dplyr_0.7.7          purrr_0.2.5         
## [13] readr_1.1.1          tidyr_0.8.2          tibble_1.4.2        
## [16] ggplot2_3.1.0        tidyverse_1.2.1     
## 
## loaded via a namespace (and not attached):
##  [1] uroot_2.0-9       Rcpp_0.12.19      lattice_0.20-35  
##  [4] zoo_1.8-4         utf8_1.1.4        assertthat_0.2.0 
##  [7] rprojroot_1.3-2   digest_0.6.18     lmtest_0.9-36    
## [10] R6_2.3.0          cellranger_1.1.0  plyr_1.8.4       
## [13] backports_1.1.2   evaluate_0.12     httr_1.3.1       
## [16] pillar_1.3.0      rlang_0.3.0.1     lazyeval_0.2.1   
## [19] curl_3.2          readxl_1.1.0      rstudioapi_0.8   
## [22] fracdiff_1.4-2    TTR_0.23-4        rmarkdown_1.10   
## [25] labeling_0.3      munsell_0.5.0     numDeriv_2016.8-1
## [28] compiler_3.4.4    modelr_0.1.2      pkgconfig_2.0.2  
## [31] forecast_8.4      urca_1.3-0        htmltools_0.3.6  
## [34] nnet_7.3-12       tidyselect_0.2.5  dlm_1.1-5        
## [37] quadprog_1.5-5    fansi_0.4.0       crayon_1.3.4     
## [40] withr_2.1.2       grid_3.4.4        nlme_3.1-131     
## [43] jsonlite_1.5      gtable_0.2.0      magrittr_1.5     
## [46] scales_1.0.0      quantmod_0.4-13   cli_1.0.1        
## [49] stringi_1.2.4     tseries_0.10-45   xml2_1.2.0       
## [52] xts_0.11-1        tools_3.4.4       glue_1.3.0       
## [55] hms_0.4.2         parallel_3.4.4    yaml_2.2.0       
## [58] colorspace_1.3-2  rvest_0.3.2       knitr_1.20       
## [61] bindr_0.1.1       haven_1.1.2
```


