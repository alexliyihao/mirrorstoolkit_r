# Shapiro-Wilk test on each variable

For a data.frame, run Shapiro-Wilk test on each column, if a "by" is
specified, it will run a stratified Shapiro-wilk test by this column

## Usage

``` r
normality_table(data, by = NULL)
```

## Arguments

- data, :

  data.frame, the data table to be tested

- by, :

  optional str, the column by which data will be stratified

## Value

data.frame, a data frame including all the results
