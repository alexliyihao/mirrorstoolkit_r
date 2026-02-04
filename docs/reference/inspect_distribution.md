# Wrapper quickly check the distribution from Shapiro-Wilk test and visualization

Wrapper quickly check the distribution from Shapiro-Wilk test and
visualization

## Usage

``` r
inspect_distribution(data, variable, by = NULL, palette_color = NULL)
```

## Arguments

- data, :

  data.frame, the data table with original data

- variable, :

  str or vector of str, the column name of variable of interest

- by, :

  optional str, if given, the column by which data will be stratified

- palette_color, :

  optional vector of str, list of colors used

## Value

a list with two element, "visualization" is a list of ggplot instance
with the histogram and density plot, "shapiro-wilk_test" is a data.frame
