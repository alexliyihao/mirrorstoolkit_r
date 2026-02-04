# Single column inspector

Wrapper function cleaning the format of generated statistic

## Usage

``` r
characteristic_wrapper(
  data,
  variable_list,
  by,
  mode = "boolean",
  normal_policy = "wilcox"
)
```

## Arguments

- data:

  data.frame, the data.frame working on

- variable_list:

  character vector, the vector of variables included

- by:

  character, the column name used for stratifying

- mode:

  character, the mode generating result, can be "boolean",
  "continuous_sd", "continuous_IQR"

- normal_policy:

  str, for "continuous_sd" mode, if "wilcox", run stats::wilcox.test, if
  "t-test", run stats::t.test

## Value

data.frame, the cleaned result
