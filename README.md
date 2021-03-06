
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EpiNOAA

<!-- badges: start -->
<!-- badges: end -->

The EpiNOAA R package is designed as a programmatic interface to the
nClimGrid daily data created by NOAA and made publicly available through
the NOAA Big Data Program on public cloud providers. The goal of this
project is to allow data subsetting and facilitate access to an analysis
ready dataset using the `tidyverse` framework for tabular data.

If you would like to know more about nClimGrid data and the background
behind this dataset, please visit: [AWS Open Data
Registry](https://registry.opendata.aws/noaa-nclimgrid/)

\[toc\]

## Installation

    # Install from CRAN
    install.packages("epinoaa")

    # Or the development version from GitHub
    # install.packages("devtools")
    devtools::install_github("...")

## Configuration

This relies on a number of underlying packages and data to facilitate
access to nClimGrid data.

**Arrow** [Arrow](https://arrow.apache.org/) is used to facilitate fast
reading of data files in parquet format. Installing [Apache Arrow for
R](https://arrow.apache.org/docs/r/) can sometimes be problematic if the
underlying C++ libraries are not installed/configured appropriately. If
you have difficulty, please consult the install docs here: [Arrow
Package Install](https://arrow.apache.org/docs/r/articles/install.html).

**Futures** This package relies on the
[Futureverse](https://www.futureverse.org/) as a parallel backend to
support performant ingest and filtering of the nClimGrid data. Please be
sure that you are aware of the computational resources needed before
scaling up your workers.

**AWS.S3** This package relies on the AWS.S3 library under the hood to
pull data from the NOAA S3 bucket. This bucket is publicly accessible
and does not require any credentials. If you see odd access denied or
object/resource do not exist errors, it could be that your default AWS
configuration is interfering with the requests. See [The Cloudyr
Project](https://cloudyr.github.io/) for more information.

## Example

This is a basic example which pulls in county data from 2021:

``` r
library(EpiNOAA)

# Make sure both that the number of workers is scaled to your computer
# and that you have enough memory to support the calls.  The data pulled below are ~100mb
data_2021 <- read_nclimgrid_epinoaa(beginning_date = '2021-01-01', end_date = '2021-12-31', workers = 10)
```

## Data Considerations

The data pulled using this package are produced by NOAA and made
available through [NOAA???s Big Data
Program](https://www.noaa.gov/information-technology/big-data). This
specific dataset is available through the [AWS Open Data
Registry](https://registry.opendata.aws/noaa-nclimgrid/). All of the raw
data can be accessed
[here](https://noaa-nclimgrid-daily-pds.s3.amazonaws.com/index.html).
Data can also be pulled through an interactive web explorer
[here](http://shiny-app-lb-db29d17-1318539185.us-east-1.elb.amazonaws.com/arc-nclimgrid-downloader/).
If you are interested in working with the data out-of-memory, raw
monthly and decadal data files can be accessed
[here](https://noaa-nclimgrid-daily-pds.s3.amazonaws.com/index.html#EpiNOAA/).

**Scaled and Preliminary Data** As these data are made available through
NOAA, they are first released as preliminary datasets, then adjusted
through a quality control process. After that additional processing and
scaling, they are released as scaled datasets. This package does not
access any preliminary data to obviate the ramifications of
inadvertently combining scaled and preliminary data in an analysis.
Preliminary data are available through the Open Data Registry here.
