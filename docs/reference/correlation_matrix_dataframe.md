# Function wrapper compute pairwise correlation matrix and return as a data.frame

If no variable list provided, the function will clean all the value as
numerical matrix and compute the correlation matrix

## Usage

``` r
correlation_matrix_dataframe(data, variable = NULL, p_threshold = 1)
```

## Arguments

- data:

  data.frame, the data table.

- variable:

  optional character vector, the list of variable included in
  computation, all columns by default

- p_threshold:

  optional float, the only the rows with p-value less than this value
  will be included, default 1 (all included)

## Value

data.frame, a data.frame with two columns: corr and p_value
