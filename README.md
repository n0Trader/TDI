# TDI - Trading Data Interface
Generic interface to retrieve trading data, inspired by **[DBI](https://dbi.r-dbi.org/)**.

The generic interface has a first draft implementation for the Yahoo Finance API, IEX Cloud API and FRED Economic data API. For now the generic **Trading Data Interface** and its first implementation are developed into the TDI package. Later we shall split the generic interface and the API implementations into different packages. 

In the philosophy of Tinyverse - **"[Light weight is the right weight](http://www.tinyverse.org/)"** - our aim is **keep it simple and keep it clean**.

The code is in initial development (alpha), and suggestion or contributions are welcome.

## Installation

You can install the latest development version from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("n0trader/TDI")
```

To install from Github you will need a [development environment](https://www.rstudio.com/ide/docs/packages/prerequisites).

The packages comes with a sample configuration file (`config.Sample`) that can be copied to create the `config.R` with your API settings.
To be able to run the [tinytest](https://cran.r-project.org/web/packages/tinytest/index.html) cases the configuration must also be available in the test working directory (e.g. copied or soft link).

## Basic usage

```R
library(DBI)

# Create a TDI connection object to access the API.
con <- TDIConnection$connect("yahoo")
getSymbol(con, "MSFT")
```