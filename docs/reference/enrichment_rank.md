# Distribution illustration for proteomics/lipidomics, etc

A wrapper cleaning the output from Python pipeline (TODO)

## Usage

``` r
enrichment_rank(
  data,
  formal_name_table,
  id = NULL,
  save_name_col = NULL,
  formal_name_col = NULL,
  sort_policy = "median",
  scale = "raw"
)
```

## Arguments

- data:

  data.frame carries the enrichment

- formal_name_table:

  data.frame, the table including the formal name and save name of the
  output

- id:

  optional str, the column name in data referring to the subject id, by
  default the first column of data

- save_name_col:

  optional str, the column name in formal_name, taking the save_name
  from pipeline output

- formal_name_col:

  optional str, the column name in formal_name, taking the formal name
  in illustration

- sort_policy, :

  optional str, the sort policy of enrichments, by default "median",
  "mean" provided

- scale, :

  optional str, the scale of enrichments, by default "raw", "logged"
  provided as log10(x+1)

## Value

a list with two element, "visualization" is a ggplot2 instance as a
scatter plot, "data" is the source data
