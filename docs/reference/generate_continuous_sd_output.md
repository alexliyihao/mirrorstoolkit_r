# Characteristics for normality distributed variable

A function extract mean(sd) format from a column named variable from
table df, when "by" has and only has 2 classes, run a Mann Whitney U
test or two sample t-test user can specify normal_policy = "wilcox" or
"t-test"

## Usage

``` r
generate_continuous_sd_output(variable, df, by, normal_policy = "wilcox")
```

## Arguments

- variable:

  str, The name of column in table df

- df:

  data.frame, the table working on

- by:

  str, stratification variable name

- normal_policy:

  str, default "wilcox",for it has relaxed assumptions \* if "wilcox",
  run stats::wilcox.test(2 categories) or stats::kruskal.test(3+) \* if
  "t-test", run stats::t.test(2 categories), or stats::aov(3+)

## Value

data.frame, the characteristic data mean(sd), when by has and only has 2
classes, run a Mann Whitney U test
