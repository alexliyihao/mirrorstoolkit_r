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
  variable_of_interest_formal_name = NULL,
  with_power = FALSE,
  variable_distribution = NULL
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

- with_power:

  boolean, when set to TRUE, will call pwrss.z.logreg function computing
  the power and sample size at 80 percent power, default FALSE to save
  computation burden

- variable_distribution:

  vector of str, either "normal" or "binary", the distribution of
  variable of interest specified, only necessary when with_power = TRUE,
  default "normal", should be 1-1 corresponding to variable of interest

## Value

data.frame, one variable in variable_of_interest as a row, with it's
beta and p-value
