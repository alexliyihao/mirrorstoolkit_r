# Mirrorstoolkit in R

## What

Welcome to Mirrorstoolkit (in R)! It is a collection for data analyses deployed in my research works.

My workflow is generally working with Python on a HPC cluster and have the result visualized in R. Thus this part will be more about quick exploratory visualization and statistic analysis.

I abstracted and wrapped them from my previous projects for those implementations which are either:

 - Frequently deployed.
 - A bit wordy in the implementation.
 - Might be deployed again in future project.
 - Very R: I'm a pythonic person, my practice is very seaborn-like.

## Why

This is majorly for my personal usage (thus it won't be on CRAN). But I'll keep it maintained here. Feel free to grab anything you like.

## Who

If you are:
 - Biomed person who need a basic stat wrappers (and your supervisor won't check your code XD).
 - As what I mentioned, python Pandas person.
I hope it can make your life easier.

## Where

The environment is in DESCRIPTION.

I'm using a Macbook 2016 with macOS Monterey 12.7.6. Running R 4.4.0 with tidyverse family. I'll make sure all the packages I used is based on CRAN.

Dependency note: This package has a part heavily depend on dplyr::do() which is officially superseded. But I haven't find a proper way to deal with it by dplyr::reframe(), dplyr::nest_by(), and dplyr::pick() as what dplyr claimed. I'll try to finish it before its official deprecation.

## When

I'll keep growing this one. Along with its Python counterpart (https://github.com/alexliyihao/mirrorstoolkit)

## How

```
devtools::install_github("alexliyihao/mirrorstoolkit_r")
library(mirrorstoolkit)
```
# Acknowledgements

All the code are abstracted from my daily task in:
 - Gissette Soffer Lab (Division of Preventive Medicine, Dept. of Medicine, CUIMC)  
 - Badri Vardarajan Lab (Gertrude H. Sergievsky Center, Dept. of Neurology, CUIMC)
