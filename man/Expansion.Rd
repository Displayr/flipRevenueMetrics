% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-ratios.R
\name{Expansion}
\alias{Expansion}
\title{\code{Expansion}}
\usage{
Expansion(data)
}
\arguments{
\item{data}{A \code{MetricData} object.}
}
\value{
A named vector if \code{cohort.type} is set to \code{"None"} or \code{"Preceding"}, or,
  a \code{matrix}. This will contain a number of attributes includeing:
\enumerate{
  \item \code{denominator} cohort.typed when \code{ratio} is \code{TRUE}.
  \item \code{numerator} cohort.typed when \code{ratio} is \code{TRUE}.
  \item The \code{detail} cohort.typed when \code{ratio} is \code{TRUE}.
  }
}
\description{
The percentage of customers whose recurring revenue increased relative to the previous
period.
}
\details{
The denominator is the annual recurring revenue of custoomers who were
customers at the beginning of the period and remained customers at the end of the period.
The numerator is sum of all the ARRs of customers whose ARR increased from the beginning
of the period to the end fo the period.
}
