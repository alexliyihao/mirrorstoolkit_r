# Pairwise distribution comparison report for boolean(0/1) variable

A cleaner wrapper running stats::pairwise.prop.test and clean the output

## Usage

``` r
pairwise_boolean_test(data, variable, by, adjustment_method = "fdr")
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

data.frame, the pairwise result for variable stratified by "by" in
"data"
