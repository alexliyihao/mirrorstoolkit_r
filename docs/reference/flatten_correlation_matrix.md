# Function cleaning correlation matrix to pairwise dataframe

This function takes a correlation matrix and a p-value matrix from
correlation matrix computing clean the matrix to pairwise terms credit
to
https://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

## Usage

``` r
flatten_correlation_matrix(corr_matrix, p_matrix)
```

## Arguments

- corr_matrix:

  matrix, a numerical matrix, the correlation matrix

- p_matrix:

  matrix, a numerical matrix, the p-value matrix

## Value

data.frame, a data.frame with two columns: corr and p_value
