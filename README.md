# tgR

This package currently is wrapping a pile of functions commonly in use to interact with and manage the Fred Hutch Genomics Repository.  

## Installation

You will need the following packages installed as well:
```{r}
install.packages(pkgs = c("httr", "REDCapR", "aws.s3", "jsonlite", "magrittr", "dplyr", "purrr", "RCurl", "checkmate", "stringr"))
```

You can install the most recent version of tgR by:

```r
require(remotes)
remotes::install_github('FredHutch/tgR')
```

Install a specific release version (in this case v1.0) by:
```r
require(remotes)
remotes::install_github('FredHutch/tgR@v1.0')
```
