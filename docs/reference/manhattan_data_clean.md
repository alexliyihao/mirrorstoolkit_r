# Clean format for a Manhattan plot

A function cleans column format for a Manhattan plot, in which
"phenotype" and "coding" should be factor, "pos" and "p-value" should be
numerical, formal name should be character

## Usage

``` r
manhattan_data_clean(
  data,
  pos,
  phenotype,
  p_value,
  formal_name = NULL,
  coding = NULL,
  phenotype_list = NULL
)
```

## Arguments

- data, :

  data.frame, The table working on

- pos, :

  str, the name of column of position of the variant

- phenotype, :

  str, the name of column of phenotype the variant associated to

- p_value, :

  str, the name of column of p-values

- formal_name, :

  optional str, the name of column of formal name of each variant

- coding, :

  optional str, (currently not used) the name of column of exonic vs
  intronic,

- phenotype_list, :

  optional vector of str, the list of phenotypes which is put into
  consideration

## Value

data.frame, the table cleaned
