% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patient_history_plot.R
\name{patient_history_plot}
\alias{patient_history_plot}
\title{Build a patient history plot}
\usage{
patient_history_plot(
  weanings_trd,
  weanings_log,
  pt_names,
  pt_registry,
  id_ospedale = NA,
  id_paziente = NA
)
}
\arguments{
\item{weanings_trd}{weaningTRD dataset}

\item{weanings_log}{weaningLOG dataset}

\item{pt_names}{pt_names dataset}

\item{pt_registry}{pt_registry dataset}

\item{id_ospedale}{(chr) 2 letter string of the hospital}

\item{id_paziente}{(double) id of a patient (between 1 and 99)}
}
\value{
a \link[ggplot2:ggplot2-package]{ggplot} showing the patient
history plot
}
\description{
Show on a single plot all relevant data about a single patient:
\itemize{
\item TRD time density plot
\item LOG time density plot
\item ICU-stay time span
\item Mechanical ventilation time span
\item SBT readiness
\item if the patient on a certain day was weaned or not
}
}
\examples{
\dontrun{
  library(weaning)

  patient_history_plot(id_ospedale = "TS", id_paziente = 15)

}

}
