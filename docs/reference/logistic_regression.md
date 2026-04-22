# Logistics Linear Regression

A wrapper creating logistic regression formula and clean the output

## Usage

``` r
logistic_regression(
  data,
  variable,
  response,
  adjustments,
  mode = "table",
  with_power = FALSE,
  variable_distribution = "normal"
)
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

- with_power:

  boolean, when set to TRUE, will call pwrss.z.logreg function computing
  the power and sample size at 80 percent power, default FALSE to save
  computation burden

- variable_distribution:

  str, either "normal" or "binary", the distribution of variable of
  interest specified, only necessary when with_power = TRUE, default
  "normal"

## Value

data.frame, if mode summary, only the beta and p-value(z-value) for
variable specified in "variable", otherwise the general summary()
