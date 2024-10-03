
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pooledipw

<!-- badges: start -->
<!-- badges: end -->

The `pooledipw` package implements inverse probability weighted (IPW)
methods to estimate effects of generalized time-varying treatment
strategies on sparsely measured continuous outcomes in electronic health
records (EHR) data. Specifically, this package implements pooled and
nonpooled IPW methods as described in McGrath et al. (in preparation).

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

#### Data Set

We will use the example data set `data_null` which contains longitudinal
data on 1,000 individuals over 25 time points. This data set was
generated so that the true average treatment effect is 0 at all time
points. The data set `data_null` contains the following columns:

- `id`: Participant ID
- `t0`: Follow-up time index
- `L`: Time-varying covariate
- `L0`: Baseline value of `L`
- `Z`: Medication initiated at baseline
- `A`: Adherence to the medication initiated at baseline
- `R`: Indicator of outcome measurement
- `Y`: Outcome

The first 10 rows of `data_null` are:

``` r
data_null[1:10,]
#>        id    t0     L    L0     Z     A     R         Y C_artificial     G
#>     <num> <int> <int> <int> <int> <num> <int>     <num>        <num> <num>
#>  1:     1     0     1     1     0     1     0        NA            0     0
#>  2:     1     1     0     1     0     1     0        NA            0     0
#>  3:     1     2     1     1     0     1     0        NA            0     0
#>  4:     1     3     0     1     0     1     0        NA            0     0
#>  5:     1     4     0     1     0     1     1 -4.367446            0     0
#>  6:     1     5     1     1     0     1     0        NA            0     0
#>  7:     1     6     1     1     0     1     0        NA            0     0
#>  8:     1     7     1     1     0     1     0        NA            0     0
#>  9:     1     8     0     1     0     0     0        NA            0     0
#> 10:     1     9     0     1     0     1     0        NA            0     0
```

#### Applying IPW

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
res_est <- ipw(data = data_null,
               pooled = TRUE,
               A_model = A ~ L + Z,
               R_model_denominator = R ~ L + A + Z,
               R_model_numerator = R ~ L0 + Z,
               Y_model = Y ~ L0 * (t0 + Z))
```

The estimated average treatment effect at each time point is given
below.

``` r
res_est$est_z0 - res_est$est_z1
#>  [1] 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562
#>  [8] 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562
#> [15] 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562 0.0434562
#> [22] 0.0434562 0.0434562 0.0434562 0.0434562
```

Note that the estimated average treatment effect is identical across all
time points due to the lack of an interaction term between `t0` and `Z`
in the outcome model.

###### Interval estimation

To obtain 95% confidence intervals around the estimated average
treatment effects, we can apply the `get_CI` function. It constructs
percentile-based bootstrap confidence intervals using `n_boot` bootstrap
replicates. We use 10 bootstrap replicates for ease of computation.

``` r
res_ci <- get_CI(res_est, data = data_null, n_boot = 10)
res_ci$res_dif
#>        Estimate   CI Lower  CI Upper
#>  [1,] 0.0434562 -0.3030726 0.2512331
#>  [2,] 0.0434562 -0.3030726 0.2512331
#>  [3,] 0.0434562 -0.3030726 0.2512331
#>  [4,] 0.0434562 -0.3030726 0.2512331
#>  [5,] 0.0434562 -0.3030726 0.2512331
#>  [6,] 0.0434562 -0.3030726 0.2512331
#>  [7,] 0.0434562 -0.3030726 0.2512331
#>  [8,] 0.0434562 -0.3030726 0.2512331
#>  [9,] 0.0434562 -0.3030726 0.2512331
#> [10,] 0.0434562 -0.3030726 0.2512331
#> [11,] 0.0434562 -0.3030726 0.2512331
#> [12,] 0.0434562 -0.3030726 0.2512331
#> [13,] 0.0434562 -0.3030726 0.2512331
#> [14,] 0.0434562 -0.3030726 0.2512331
#> [15,] 0.0434562 -0.3030726 0.2512331
#> [16,] 0.0434562 -0.3030726 0.2512331
#> [17,] 0.0434562 -0.3030726 0.2512331
#> [18,] 0.0434562 -0.3030726 0.2512331
#> [19,] 0.0434562 -0.3030726 0.2512331
#> [20,] 0.0434562 -0.3030726 0.2512331
#> [21,] 0.0434562 -0.3030726 0.2512331
#> [22,] 0.0434562 -0.3030726 0.2512331
#> [23,] 0.0434562 -0.3030726 0.2512331
#> [24,] 0.0434562 -0.3030726 0.2512331
#> [25,] 0.0434562 -0.3030726 0.2512331
```
