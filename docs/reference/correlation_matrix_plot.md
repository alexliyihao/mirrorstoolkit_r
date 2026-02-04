# Function wrapper compute pairwise correlation matrix and return as a visualization format

If no variable list provided, the function will clean all the value as
numerical matrix and compute the correlation matrix

## Usage

``` r
correlation_matrix_plot(
  data,
  variable = NULL,
  p_threshold = NULL,
  font_size = 5
)
```

## Arguments

- data:

  data.frame, the data table.

- variable:

  optional character vector, the list of variable included in
  computation, all columns by default

- p_threshold:

  optional float, the only the rows with p-value less than this value
  will be included, Null default(all included)

- font_size:

  optional int, the size of row/column labels, default 5

## Value

data.frame, a data.frame with two columns: corr and p_value
