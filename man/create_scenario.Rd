% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_scenario.R
\name{create_scenario}
\alias{create_scenario}
\title{Create individual scenario}
\usage{
create_scenario(
  rdr,
  scen_name,
  source_modes,
  combined_modes = F,
  target_modes,
  source_distance_cats,
  source_trips,
  target_trips
)
}
\arguments{
\item{rdr}{data frame of trips}

\item{scen_name}{name of scenario}

\item{source_modes}{which mode(s) to take trips from}

\item{combined_modes}{whether or not to combine source modes}

\item{target_modes}{mode to change to}

\item{source_distance_cats}{which categories to select trips from}

\item{source_trips}{how many trips to leave, or to take}

\item{target_trips}{}
}
\value{
list of scenarios
}
\description{
Function to create individual scenario for the five prespecified scenarios from the baseline for Accra and Sao Paulo (create_all_scenarios)
}
