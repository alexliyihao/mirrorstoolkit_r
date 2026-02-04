# 3d visualization for rms fit object

A wrapper generating plotly figures for rms result in 3d surface,
mimicking the behaviour for ggrmsMD in 2D

## Usage

``` r
rms_illustration_3d(
  fit,
  datadist,
  variable,
  target_name = "target",
  density = 50
)
```

## Arguments

- fit, :

  rms model fit result

- datadist, :

  rms datadist() result

- variable, :

  vector of str with length 2, the two variables on x and y axis

- target_name, :

  str, the axis label for prediction on z axis, default "target"

- density, :

  int or vector of int with length 2, the number of points on x and y
  axis, if only given one number will deployed on both axes, default 50

## Value

plotly::plot_ly instance, the 3d output for the model
