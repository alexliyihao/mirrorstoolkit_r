# Logistics Linear Regression wrapper with multi variable of interest and stratification

A wrapper running multiple linear_regression() for a list of variables
of interest, stratification function provided

## Usage

``` r
logistic_wrapper(
  data,
  response,
  adjustments,
  by = NULL,
  variable_of_interest = NULL,
  variable_of_interest_formal_name = NULL
)
```

## Arguments

- data:

  data.frame the table working on

- response:

  str, the name of column of target variable in table data

- adjustments:

  vector of str, the list of adjustment variables in the table data

- by:

  str, the name of column of stratified variable in table data

- variable_of_interest:

  vector of str, the names of columns of interested variable in table
  data

- variable_of_interest_formal_name:

  vector of str, the output names of interested variable in result

## Value

data.frame, one variable in variable_of_interest as a row, with it's
beta and p-value
