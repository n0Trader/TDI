% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TDICashFlow.R
\name{TDICashFlow}
\alias{TDICashFlow}
\title{Cash flow data (R6 class constructor)}
\description{
Generic abstract base class for TDI cash flow data.
}
\details{
The class is a data structure for cash flow data,
and should be implemented in the API implementation.
Using R6 classes for data structures enforce standardization.
}
\section{Super classes}{
\code{\link[TDI:baseTDI]{TDI::baseTDI}} -> \code{\link[TDI:TDIResult]{TDI::TDIResult}} -> \code{TDICashFlow}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{endDate}}{Reporting end date.}

\item{\code{netIncome}}{Net income.}

\item{\code{operatingCashFlow}}{Operating cash flow.}

\item{\code{capitalExpenditures}}{Capital expenditures.}

\item{\code{freeCashFlow}}{Free cash flow.}
}
\if{html}{\out{</div>}}
}
