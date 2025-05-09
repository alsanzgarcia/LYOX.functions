% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trapz.R
\name{trapz}
\alias{trapz}
\title{AUC calculation}
\usage{
trapz(x, y, t0, t1, threshold, method = "linear")
}
\arguments{
\item{x}{Time vector.}

\item{y}{Observation vector matching with time vector}

\item{t0}{Start time for AUC calculation}

\item{t1}{End time for AUC calculation}

\item{threshold}{Lower threshold. Any value below threshold will be set to threshold for AUC calculation}

\item{method}{Method for AUC calculation: "linear" or "linear up/log down"}

\item{return}{}

\item{examples}{trapz(x = time, y = concentration, t0 = 0, t1 = 100, threshold = 0.01,
method = "linear up/log down")}
}
\description{
Computes the area under the concentration time curve
and above 'threshold'. In order to have the normal AUC threshold
should be set to 0
}
