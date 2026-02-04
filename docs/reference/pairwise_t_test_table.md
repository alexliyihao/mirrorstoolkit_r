# Pairwise distribution comparison report for normally distributed variables

A wrapper allowing multiple variable of interest, will run
stats::pairwise.t.test in an iterative manner

## Usage

``` r
pairwise_t_test_table(data, variable, by, adjustment_method = "fdr")
```

## Arguments

- data:

  data.frame, the source data

- variable:

  str, the variable interested in

- by:

  str, the categorical variable stratifying the dataset

- adjustment_method:

  optional str, the correction adjusting pairwise, Benjamini-Hochberg
  FDR by default

## Value

data.frame, the pairwise result for variable(s) stratified by "by" in
"data"
