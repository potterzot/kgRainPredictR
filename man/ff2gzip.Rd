% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/io.R
\name{ff2gzip}
\alias{ff2gzip}
\title{filter and convert zipped csv to gzipped csv.}
\usage{
ff2gzip(in_file, out_file, filter_vector, number_chunks, chunk_size = 100000L,
  compression = 9, ...)
}
\arguments{
\item{in_file}{name of csv file.}

\item{out_file}{name of file to save.}

\item{filter_vector}{boolean vector to filter on.}

\item{number_chunks}{number of chunks to use. Overrides chunk_size.}

\item{chunk_size}{number of rows in each chunk.}

\item{compression}{whether to compress the saved file.}

\item{...}{}
}
\value{
TRUE if no errors.
}
\description{
filter and convert zipped csv to gzipped csv.
}

