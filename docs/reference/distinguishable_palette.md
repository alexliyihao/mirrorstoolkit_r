# Wrapper creating a distinguishable, non-white color palette for ggplot2 based on Polychrome::glasbey.colors

Generate a color palette for ggplot2 with specified number of color,
white is intentionally removed, added codes dealt with 1 or 2 color(s).

## Usage

``` r
distinguishable_palette(num_color, pre_specified_palette = NULL)
```

## Arguments

- num_color, :

  int, the number of color need

- pre_specified_palette, :

  vector of str, the palette provided, if provided, will directly output
  this vector

## Value

list of str, the color list
