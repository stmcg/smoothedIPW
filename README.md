
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

The estimated counterfactual outcome mean for each medication at each
time point is given below.

``` r
res_est$est
#>       time          Z=0         Z=1
#>  [1,]    0  0.031534949 -0.01192126
#>  [2,]    1  0.027343466 -0.01611274
#>  [3,]    2  0.023151983 -0.02030422
#>  [4,]    3  0.018960500 -0.02449570
#>  [5,]    4  0.014769017 -0.02868719
#>  [6,]    5  0.010577534 -0.03287867
#>  [7,]    6  0.006386051 -0.03707015
#>  [8,]    7  0.002194568 -0.04126164
#>  [9,]    8 -0.001996915 -0.04545312
#> [10,]    9 -0.006188398 -0.04964460
#> [11,]   10 -0.010379881 -0.05383609
#> [12,]   11 -0.014571364 -0.05802757
#> [13,]   12 -0.018762847 -0.06221905
#> [14,]   13 -0.022954330 -0.06641053
#> [15,]   14 -0.027145813 -0.07060202
#> [16,]   15 -0.031337296 -0.07479350
#> [17,]   16 -0.035528779 -0.07898498
#> [18,]   17 -0.039720262 -0.08317647
#> [19,]   18 -0.043911745 -0.08736795
#> [20,]   19 -0.048103228 -0.09155943
#> [21,]   20 -0.052294711 -0.09575092
#> [22,]   21 -0.056486194 -0.09994240
#> [23,]   22 -0.060677677 -0.10413388
#> [24,]   23 -0.064869160 -0.10832536
#> [25,]   24 -0.069060643 -0.11251685
```

###### Interval estimation

To obtain 95% confidence intervals around the estimated counterfactual
outcome means, we can apply the `get_CI` function. It constructs
percentile-based bootstrap confidence intervals using `n_boot` bootstrap
replicates. We use 10 bootstrap replicates for ease of computation.

``` r
set.seed(1234)
res_ci <- get_CI(res_est, data = data_null, n_boot = 10)
res_ci$res_boot
#> $`0`
#>       Time     Estimate   CI Lower   CI Upper
#>  [1,]    0  0.031534949 -0.1092570 0.23272791
#>  [2,]    1  0.027343466 -0.1211586 0.21258969
#>  [3,]    2  0.023151983 -0.1330602 0.19245148
#>  [4,]    3  0.018960500 -0.1449618 0.17231327
#>  [5,]    4  0.014769017 -0.1568634 0.15853839
#>  [6,]    5  0.010577534 -0.1687651 0.14708488
#>  [7,]    6  0.006386051 -0.1806667 0.13563137
#>  [8,]    7  0.002194568 -0.1925683 0.12674436
#>  [9,]    8 -0.001996915 -0.2044699 0.11879122
#> [10,]    9 -0.006188398 -0.2163716 0.11083807
#> [11,]   10 -0.010379881 -0.2282732 0.10288492
#> [12,]   11 -0.014571364 -0.2401748 0.09493178
#> [13,]   12 -0.018762847 -0.2520764 0.08697863
#> [14,]   13 -0.022954330 -0.2639780 0.07902548
#> [15,]   14 -0.027145813 -0.2758797 0.07107234
#> [16,]   15 -0.031337296 -0.2877813 0.06311919
#> [17,]   16 -0.035528779 -0.2996829 0.05553564
#> [18,]   17 -0.039720262 -0.3115845 0.04921082
#> [19,]   18 -0.043911745 -0.3234861 0.04440936
#> [20,]   19 -0.048103228 -0.3353878 0.03960791
#> [21,]   20 -0.052294711 -0.3472894 0.03499151
#> [22,]   21 -0.056486194 -0.3591910 0.03776593
#> [23,]   22 -0.060677677 -0.3710926 0.04054036
#> [24,]   23 -0.064869160 -0.3829942 0.04376236
#> [25,]   24 -0.069060643 -0.3948959 0.04837243
#> 
#> $`1`
#>       Time    Estimate   CI Lower   CI Upper
#>  [1,]    0 -0.01192126 -0.1926767 0.12464006
#>  [2,]    1 -0.01611274 -0.1942545 0.10379589
#>  [3,]    2 -0.02030422 -0.1958324 0.09218908
#>  [4,]    3 -0.02449570 -0.1974103 0.08193930
#>  [5,]    4 -0.02868719 -0.1989882 0.07180658
#>  [6,]    5 -0.03287867 -0.2005661 0.06167386
#>  [7,]    6 -0.03707015 -0.2021440 0.05154113
#>  [8,]    7 -0.04126164 -0.2037219 0.04140841
#>  [9,]    8 -0.04545312 -0.2052998 0.03127569
#> [10,]    9 -0.04964460 -0.2068777 0.02114296
#> [11,]   10 -0.05383609 -0.2084556 0.01479554
#> [12,]   11 -0.05802757 -0.2100335 0.01716436
#> [13,]   12 -0.06221905 -0.2116114 0.01953319
#> [14,]   13 -0.06641053 -0.2131893 0.02190201
#> [15,]   14 -0.07060202 -0.2171542 0.02427084
#> [16,]   15 -0.07479350 -0.2276040 0.02663967
#> [17,]   16 -0.07898498 -0.2477422 0.02900849
#> [18,]   17 -0.08317647 -0.2678804 0.03137732
#> [19,]   18 -0.08736795 -0.2880186 0.03449785
#> [20,]   19 -0.09155943 -0.3081568 0.03801541
#> [21,]   20 -0.09575092 -0.3282950 0.04153297
#> [22,]   21 -0.09994240 -0.3484333 0.04505053
#> [23,]   22 -0.10413388 -0.3685715 0.04856809
#> [24,]   23 -0.10832536 -0.3887097 0.05208565
#> [25,]   24 -0.11251685 -0.4088479 0.05560321
```
