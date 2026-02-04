# Characteristics for boolean variable

A function extract boolean 1/total(percentage) format from a column
named variable from table df, when by has and only has 2 classes, run a
chi-square test

## Usage

``` r
generate_boolean_ratio_output(variable, df, by)
```

## Arguments

- variable:

  str, The name of column in table df

- df:

  data.frame, the table working on

- by:

  str, stratification variable name

## Value

data.frame, frequency and relative frequency for the second term in
table() and prop.table which is designed to be 1 (positive indicator for
a disease against as negative)
