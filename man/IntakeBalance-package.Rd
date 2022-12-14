% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0b_package_doc.R
\name{IntakeBalance-package}
\alias{IntakeBalance-package}
\title{IntakeBalance: Apply intake-balance methods for calculating energy intake}
\description{
This Facilitates energy intake calculation using the intake-balance method.
}
\section{Core functions}{

\code{\link{IntakeBalance-function}}

\code{\link{delta_es}}
}

\examples{
\donttest{

d <- data.frame(
  fm_start = c(50.5, 70.2), fm_end = c(50.7, 70.0),
  ffm_start = c(75.1, 90.3), ffm_end = c(74.9, 90.3),
  ee = c(1950, 2473), n_days = c(14, 14)
)

IntakeBalance(50.5, 50.7, 75.1, 74.9, ee_per_day = 1950, n_days = 14)

IntakeBalance(
  "fm_start", "fm_end", "ffm_start", "ffm_end",
  ee_per_day = "ee", n_days = "n_days", df = d
)

}

}
