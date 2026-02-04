# A wrapper preparing pairwise distribution comparison report normally distributed variables

This wrapper will run a stats::pairwise.wilcox.test for each element
mentioned in "variable" while stratified across different value in "by"
column and prepare a barchart for illustration.

## Usage

``` r
pairwise_wilcox_report(
  data,
  variable,
  by,
  p_threshold = 0.05,
  adjustment_method = "fdr"
)
```

## Arguments

- data:

  data.frame, the source data

- variable:

  str or vector of str, the variable you are interested in

- by:

  str, the categorical variable stratifying the dataset

- p_threshold:

  optional float, the significance threshold, 0.05 by default

- adjustment_method:

  optional str, the correction adjusting pairwise, Benjamini-Hochberg
  FDR by default

## Value

a list with following attributes: \* result: data.frame complete
pairwise result. \* significant_result: data.frame which only include
the significant result \* plot: ggplot2 instance, the figure
illustrating the significant result.
