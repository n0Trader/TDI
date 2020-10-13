# TDI - Trading Data Interface
Generic interface to retrieve trading data, inspired by **[DBI](https://dbi.r-dbi.org/)**, and implemented with **[R6](https://r6.r-lib.org)** object-oriented classes. The purpose is to support the eco-system of trading application(s) and quantitative analysis with a package which is solely developed to collect trading data from multiple sources (API's) through a reliable standardized interface.

The package offers a first draft implementations for the Yahoo Finance API, IEX Cloud API and FRED Economic data API. For now the generic **Trading Data Interface** and its first implementations are developed into the TDI package. When growing the intention is to split the generic interface and the API implementations into different packages.

In the philosophy of Tinyverse - **"[Light weight is the right weight](http://www.tinyverse.org/)"** - our aim is to **keep it simple and keep it light**.

The current version retrieves historical prices (e.g. quotes) and economical data series. Suggestion or contributions are welcome to further develop the package to retrieve additional quantitative data.

## Installation

You can install the latest development version from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("n0trader/TDI")
```

To install from Github you will need a [development environment](https://www.rstudio.com/ide/docs/packages/prerequisites).

The package comes with a sample environment profile (`/inst/TDI/Renviron.sample`) with environment keys. These keys should be added to the project environment profile to store (personal) credentials for API access.

## Basic usage

```R
library(TDI)

# Create a TDI connection object to access the API.
con <- TDIConnection$connect("yahoo")
getSymbol(con, "MSFT")
```
