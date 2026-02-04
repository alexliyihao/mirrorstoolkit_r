# Pairwise distribution comparison report for boolean(0/1) variables

A wrapper running allowing multiple variable of interest, will run the
result in an interative manner

## Usage

``` r
pairwise_boolean_table(data, variable, by, adjustment_method = "fdr")
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
