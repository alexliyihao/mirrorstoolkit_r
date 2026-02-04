# A fill bar chart for composition over a specific variable

This function returns a filled bar chart illustrate the trend of a
composition over the change of a specific variable

## Usage

``` r
trend_bar_chart(
  data,
  id,
  by,
  variable = NULL,
  legend_title = "Component",
  palette_color = NULL
)
```

## Arguments

- data, :

  data.frame, The table working on

- id, :

  str, the name of column of subject id

- by, :

  str, the name of column of variable on x

- variable, :

  optional vector of str, the name of column of composition components
  if specified, otherwise will use all the other variables

- legend_title, :

  str, the legend title in the figure

- palette_color, :

  list or str, pre-specfied colors used for different phenotypes

## Value

ggplot2 instance, the plot output
