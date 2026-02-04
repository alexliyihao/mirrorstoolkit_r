# Ordinary Linear Regression

A wrapper creating linear regression formula and clean the output

## Usage

``` r
linear_regression(data, variable, response, adjustments, mode = "table")
```

## Arguments

- data:

  data.frame the table working on

- variable:

  str, the name of column of interested variable in table data

- response:

  str, the name of column of target variable in table data

- adjustments:

  vector of str, the list of adjustment variables in the table data

- mode:

  str, when "summary", only output the coefficient corresponding to
  column specified in variable

## Value

data.frame, if mode summary, only the beta and p-value for variable
specified in "variable", otherwise the general summary()
