# Generate and preview a directed acyclic graph for multi-omics result

A wrapper prepare the data for a directed acyclic graph for multi-omics
data, and preview it

## Usage

``` r
multi_omics_directed_acyclic_graph(
  data,
  from,
  to,
  beta,
  p_value,
  from_class,
  to_class,
  class_level = NULL,
  color_palette = NULL
)
```

## Arguments

- data:

  data.frame, the data input

- from:

  str, the column name for the starting point

- to:

  str, the column name for the end point

- beta:

  str, the column name for the beta

- p_value, :

  str, the column name for p-value in the association study

- from_class, :

  str, the column name for the class label of from column if specified

- to_class, :

  str, the column name for the class label of to column if specified

- class_level, :

  optional vector of str, if need to specify a order for classes defined
  in from_class and to_class

- color_palette, :

  optional vector of str, the color for each class, the package will
  prepare a default color if not specified.

## Value

A list of result with following attributes:

\* nodes data.frame, the nodes table \* edges data.frame, the edges
table \* illustration_network visNetwork instance, the network in
illustration \* raw_network visNetwork instance, the network without any
visNetwork::visPhysics settings
