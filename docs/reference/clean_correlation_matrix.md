# Function cleaning input and computing correlation matrix

If no variable list provided, the function will clean all the value as
numerical matrix and compute the correlation matrix

## Usage

``` r
clean_correlation_matrix(data, variable = NULL)
```

## Arguments

- data:

  data.frame, the data table.

- variable:

  optional character vector, the list of variable included in
  computation, all columns by default

## Value

a list with elements r, the matrix of correlations, n the matrix of
number of observations used in analyzing each pair of variables, P, the
asymptotic P-values, and type.
