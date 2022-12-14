% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combined_rr_ap_pa.R
\name{combined_rr_ap_pa}
\alias{combined_rr_ap_pa}
\title{Combine relative risks from AP and PA}
\usage{
combined_rr_ap_pa(ind_pa, ind_ap, conf_int = FALSE)
}
\arguments{
\item{ind_pa}{data frame of individual RRs for diseases affected by PA}

\item{ind_ap}{data frame of individual RRs for diseases affected by AP}

\item{conf_int}{logic: whether to include confidence interval from dose response relationships or not}
}
\value{
combined RR for diseases after accounted for AP and PA exposures
}
\description{
Combine relative risks from AP and PA through multiplication for crossover diseases
}
