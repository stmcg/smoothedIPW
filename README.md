
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pooledipw

<!-- badges: start -->
<!-- badges: end -->

The `pooledipw` package implements inverse probability weighted (IPW)
methods to estimate effects of generalized time-varying treatment
strategies on sparsely measured continuous outcomes in electronic health
records (EHR) data. Specifically, this package implements pooled and
nonpooled IPW methods as described in McGrath et al. (in preparation).
In settings with truncation by death, two different pooled IPW methods
(i.e., the stacked and nonstacked methods) are available, which rely on
different assumptions to smooth over time.

## Installation

You can install the development version of `pooledipw` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stmcg/pooledipw")
```

## Example

We first load the package.

``` r
library(pooledipw)
```

We will estimate the effect of treatment strategies with the following
three components:

- Initiate medication $z$ ($z \in \{0, 1\}$) at baseline
- Adhere to medication $z$ throughout the follow up, allowing for a
  grace period of 2 months
- Ensure an outcome measurement at the follow-up time of interest

We consider the follow-up time of interest to be $t^*$,
$t^* \in \{6, 12, 18, 24\}$. In this example, we consider that there are
no deaths over the study period. See the package help files for examples
with deaths over the study period.

#### Data Set

We will use the example data set `data_null` which contains longitudinal
data on 1,000 individuals over 25 time points. This data set was
generated so that the treatment has no effect on the outcome at all time
points. The data set `data_null` contains the following columns:

- `id`: Participant ID
- `time`: Follow-up time index
- `L`: Time-varying covariate
- `Z`: Medication initiated at baseline
- `A`: Adherence to the medication initiated at baseline
- `R`: Indicator of outcome measurement
- `Y`: Outcome

The first 10 rows of `data_null` are:

``` r
data_null[1:10,]
#>        id  time     L     Z     A     R         Y
#>     <num> <int> <int> <int> <num> <int>     <num>
#>  1:     1     0     1     0     1     0        NA
#>  2:     1     1     0     0     1     0        NA
#>  3:     1     2     1     0     1     0        NA
#>  4:     1     3     0     0     1     0        NA
#>  5:     1     4     0     0     1     1 -4.367446
#>  6:     1     5     1     0     1     0        NA
#>  7:     1     6     1     0     1     0        NA
#>  8:     1     7     1     0     1     0        NA
#>  9:     1     8     0     0     0     0        NA
#> 10:     1     9     0     0     1     0        NA
```

The package generally expects users to follow these naming conventions
for the columns of the observed data set. The columns for the
time-varying covariate(s) are an exception to this, which can take on
any names.

#### Applying IPW

###### Preparing the data set

We first need to add some variables to the data set before applying
inverse probability weighting. Specifically, we need to add:

- `C_artificial`: An indicator specifying when an individual should be
  artificially censored from the data
- `A_model_eligible`: An indicator specifying what records should be
  used for fitting the treatment adherence model

We will also need to add columns for the baseline value of the
time-varying covariates. In our case, we will add a column `L_baseline`
for the baseline value of `L`.

These columns can be added by the `prep_data` function, as shown below:

``` r
data_null_processed <- prep_data(data = data_null, grace_period_length = 2,
                                 baseline_vars = 'L')
data_null_processed[id == 2,]
#>        id  time     L     Z     A     R           Y A_model_eligible
#>     <num> <int> <int> <int> <num> <int>       <num>            <num>
#>  1:     2     0     1     0     1     1  -6.8964763                0
#>  2:     2     1     1     0     1     0          NA                0
#>  3:     2     2     1     0     1     1 -10.3900416                0
#>  4:     2     3     1     0     1     0          NA                0
#>  5:     2     4     1     0     1     0          NA                0
#>  6:     2     5     1     0     1     0          NA                0
#>  7:     2     6     1     0     0     0          NA                0
#>  8:     2     7     0     0     0     1  -4.2866350                0
#>  9:     2     8     1     0     0     1  11.3850700                1
#> 10:     2     9     0     0     1     0          NA                0
#> 11:     2    10     1     0     1     0          NA                0
#> 12:     2    11     0     0     1     0          NA                0
#> 13:     2    12     1     0     0     0          NA                0
#> 14:     2    13     1     0     1     0          NA                0
#> 15:     2    14     0     0     1     1  -3.6372290                0
#> 16:     2    15     1     0     1     1  -0.5293098                0
#> 17:     2    16     0     0     1     0          NA                0
#> 18:     2    17     1     0     0     0          NA                0
#> 19:     2    18     1     0     1     1  -2.9451809                0
#> 20:     2    19     1     0     1     0          NA                0
#> 21:     2    20     1     0     1     0          NA                0
#> 22:     2    21     1     0     1     1  -3.6248307                0
#> 23:     2    22     0     0     1     0          NA                0
#> 24:     2    23     1     0     1     0          NA                0
#> 25:     2    24     0     0     1     0          NA                0
#>        id  time     L     Z     A     R           Y A_model_eligible
#>     C_artificial L_baseline
#>            <num>      <int>
#>  1:            0          1
#>  2:            0          1
#>  3:            0          1
#>  4:            0          1
#>  5:            0          1
#>  6:            0          1
#>  7:            0          1
#>  8:            0          1
#>  9:            1          1
#> 10:            1          1
#> 11:            1          1
#> 12:            1          1
#> 13:            1          1
#> 14:            1          1
#> 15:            1          1
#> 16:            1          1
#> 17:            1          1
#> 18:            1          1
#> 19:            1          1
#> 20:            1          1
#> 21:            1          1
#> 22:            1          1
#> 23:            1          1
#> 24:            1          1
#> 25:            1          1
#>     C_artificial L_baseline
```

###### Point estimation

We will use the pooled IPW method, which is implemented in the `ipw`
function. This method involves specifying the following models:

- `A_model`: Treatment adherence model
- `R_model_denominator`: Outcome measurement indicator model (used in
  the denominator of weights)
- `R_model_numerator`: (Optional) Outcome measurement indicator model
  (used in the numerator of weights for stabilization)
- `Y_model`: Outcome (marginal structural) model

An example application of `ipw` is below:

``` r
res_est <- ipw(data = data_null_processed,
               pooled = TRUE,
               outcome_times = c(6, 12, 18, 24),
               A_model = A ~ L + Z,
               R_model_denominator = R ~ L + A + Z,
               R_model_numerator = R ~ L_baseline + Z,
               Y_model = Y ~ L_baseline * (time + Z))
```

The estimated counterfactual outcome mean for each medication at each
follow-up time of interest ($t^*$) is given below.

``` r
res_est
#> INVERSE PROBABILITY WEIGHTED ESTIMATES OF THE COUNTERFACTUAL OUTCOME MEAN 
#> 
#>  time          Z=0         Z=1
#>     6  0.007124865 -0.03536354
#>    12 -0.019445369 -0.06193377
#>    18 -0.046015603 -0.08850401
#>    24 -0.072585838 -0.11507424
```

###### Interval estimation

To obtain 95% confidence intervals around our estimates, we can apply
the `get_CI` function. It constructs percentile-based bootstrap
confidence intervals using `n_boot` bootstrap replicates. We use 10
bootstrap replicates for ease of computation.

``` r
set.seed(1234)
res_ci <- get_CI(res_est, data = data_null_processed, n_boot = 10)
res_ci$res_boot
#> $`0`
#>      Time     Estimate   CI Lower   CI Upper
#> [1,]    6  0.007124865 -0.1774043 0.13580696
#> [2,]   12 -0.019445369 -0.2534594 0.08688074
#> [3,]   18 -0.046015603 -0.3295146 0.04689198
#> [4,]   24 -0.072585838 -0.4055698 0.05887030
#> 
#> $`1`
#>      Time    Estimate   CI Lower   CI Upper
#> [1,]    6 -0.03536354 -0.2027095 0.05405786
#> [2,]   12 -0.06193377 -0.2137749 0.02011806
#> [3,]   18 -0.08850401 -0.2910591 0.03815230
#> [4,]   24 -0.11507424 -0.4132964 0.06315579
```
