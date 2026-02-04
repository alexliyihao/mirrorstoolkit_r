# Wrapper for a Manhattan plot

A wrapper function cleans column format for a Manhattan plot

## Usage

``` r
manhattan_plot(
  data,
  pos,
  phenotype,
  p_value,
  p_value_threshold = 0.05,
  formal_name = NULL,
  coding = NULL,
  phenotype_list = NULL,
  highlight_region = NULL,
  palette_color = NULL,
  coord_ratio = 2500/1
)
```

## Arguments

- data, :

  data.frame, The table working on

- pos, :

  string, the name of column of position of the variant

- phenotype, :

  str, the name of column of phenotype the variant associated to

- p_value, :

  str, the name of column of p-values

- p_value_threshold, :

  float, the threshold for significance, default 0.05

- formal_name, :

  optional str, the name of column of formal name of each variant, if
  not specified, will use pos

- coding, :

  optional str, (currently not used), the name of column of exonic vs
  intronic,

- phenotype_list, :

  optional vector of str, the list of phenotypes which is put into
  consideration

- highlight_region, :

  optional list of vector of str, in list(c(start, end)...) format, the
  list of highlighted region

- palette_color, :

  list or str, pre-specfied colors used for different phenotypes

- coord_ratio, :

  float, the coordinate ratio for the figure

## Value

ggplot2 instance, the Manhattan plot
