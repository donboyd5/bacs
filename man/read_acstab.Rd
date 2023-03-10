% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_acs_data_newformat.R
\name{read_acstab}
\alias{read_acstab}
\title{Read a single ACS table and return the full table}
\usage{
read_acstab(tabname, xdacs = bacs:::default_xdacs)
}
\arguments{
\item{tabname}{Census Bureau name for the table (e.g., B01001) as a string.}

\item{xdacs}{Local folder where table is saved (or default).}
}
\value{
Tibble with the full data for the ACS table.
}
\description{
Read a single ACS table and return the full table
}
\examples{
df <- read_acstab("B01003")
}
