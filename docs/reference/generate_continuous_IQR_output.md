# Characteristics for non-normality distributed variable

Extract median(IQR) format from a column named variable from table df,
when "by" has and only has 2 classes, run a Mann-Whitney U test

## Usage

``` r
generate_continuous_IQR_output(variable, df, by)
```

## Arguments

- variable:

  str, The name of column in table df

- df:

  data.frame, the table working on

- by:

  str, stratification variable name

## Value

data.frame, the characteristic data median(IQR), when by has and only
has 2 classes, run a Mann Whitney U test
