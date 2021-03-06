% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yahooAPI.R
\name{YahooAPI}
\alias{YahooAPI}
\title{Yahoo Finance API}
\description{
Yahoo Finance API class inheriting from `TDIConnection`.
This class implements the Yahoo Finance API requests.
}
\section{Super classes}{
\code{\link[TDI:baseTDI]{TDI::baseTDI}} -> \code{\link[TDI:TDIConnection]{TDI::TDIConnection}} -> \code{YahooAPI}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-getChart}{\code{YahooAPI$getChart()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="TDI" data-topic="TDIConnection" data-id="initialize">}\href{../../TDI/html/TDIConnection.html#method-initialize}{\code{TDI::TDIConnection$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="TDI" data-topic="TDIConnection" data-id="jsonRequest">}\href{../../TDI/html/TDIConnection.html#method-jsonRequest}{\code{TDI::TDIConnection$jsonRequest()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="TDI" data-topic="TDIConnection" data-id="requestString">}\href{../../TDI/html/TDIConnection.html#method-requestString}{\code{TDI::TDIConnection$requestString()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="TDI" data-topic="TDIConnection" data-id="validValue">}\href{../../TDI/html/TDIConnection.html#method-validValue}{\code{TDI::TDIConnection$validValue()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getChart"></a>}}
\subsection{Method \code{getChart()}}{
Retrieve historical prices for the symbol.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{YahooAPI$getChart(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{see \code{\link{TDIConnection}}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An object of class `Instrument` with historical prices.
}
}
}
