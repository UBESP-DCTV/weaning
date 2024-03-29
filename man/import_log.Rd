% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_log.R
\name{import_log}
\alias{import_log}
\title{Import LOG file}
\usage{
import_log(.file_path, verbose = FALSE, need_hdr = FALSE)
}
\arguments{
\item{.file_path}{(chr) path to the LOG file to import}

\item{verbose}{(lgl, FALSE) would you like to have additional
messages to be signaled?}

\item{need_hdr}{(lgl, default = FALSE) we do not use that, here for
potential future purposes}
}
\value{
a \link[tibble:tibble-package]{tibble} with the imported
information (i.e. the tabular content plus the patient id) from the
\code{.file_path} LOG file.
}
\description{
Read and import in a table the information reported in a LOG file
from weanings study. Among the information, add a column reporting
the patient' ID as \code{id_pat}.
}
\examples{
\dontrun{
  library(weaning)

  file.path(get_data_path(), "AB/AN001_356_LOG.SI") |>
  import_log()
}
}
