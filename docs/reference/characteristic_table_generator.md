# Characteristic Table Generator

Wrapper function cleaning the format of generated statistic

## Usage

``` r
characteristic_table_generator(
  data,
  by = NULL,
  boolean_list = NULL,
  sd_list = NULL,
  IQR_list = NULL,
  order = NULL,
  normal_policy = "wilcox"
)
```

## Arguments

- data:

  data.frame, the data.frame working on

- by:

  character, the column name used for stratifying

- boolean_list:

  optional character vector, the column name which is treated as boolean
  variable

- sd_list:

  optional character vector, the column name which is treated as normal
  distributed continuous variable

- IQR_list:

  optional character vector, the column name which is treated as
  non-normal distributed continuous variable

- order:

  optional character vector, if specified, the rows will follow the
  order provided

- normal_policy:

  str, for variables in sd_list, if "wilcox", run stats::wilcox.test, if
  "t-test", run stats::t.test

## Value

data.frame, the cleaned result
