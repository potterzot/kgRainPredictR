#' Calculate the time difference.
#'
#' @importFrom zoo na.fil
#' @export
#'
#' @param times vector of times to be calculated.
#' @param num_per_segment number of time steps per segment (e.g. 60 minutes
#' per hour)
#' @return vector of time differences, weighted to span the entire segment.
time_difference <- function(times, num_per_segment = 60) {
  n <- length(times)
  valid_time <- vector(mode="numeric", length = n)
  valid_time[1] <- times[1]
  valid_time[-1] <- diff(times, 1)
  valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
  valid_time <- valid_time / num_per_segment
  valid_time
}

#' Interpolate
#'
#' A wrapper around zoo::na.fill to interpolate without errors.
#' @importFrom zoo na.fill
#' @export
#' @param v a vector of data
#'
interpolate <- function(v, fill = "extend", ...) {
  n <- length(v)
  nna <- n - sum(is.na(v))
  if (nna == 1) res <- rep(sum(v, na.rm = T), n)
  else if (nna == 0) res <- rep(0, n) #rep(NA, n)
  else res <- na.fill(v, fill, ...)
  res
}


#' Select a group of ids from a large data file.
#'
#' @importFrom bit chunk bit
#' @export
#'
#' @param dat the data.
#' @param group_var the group variable.
#' @param s vector of group identifiers to be selected.
#' @param columns the columns to return, or NULL for all.
#' @param num_chunks number of chunks to use.
#' @param chunk_size number of rows in each chunk. Overridden by \code{num_chunks}
#' @return selected data.
select_group <- function(dat, group_var, s, columns, num_chunks,
                         chunk_size = 10000) {
  if (missing(columns)) columns <- names(dat)

  N <- dim(dat)[1]
  b <- bit(N)
  if (!missing(num_chunks)) chunk_size <- N / num_chunks
  for (i in chunk(1,N,chunk_size)) {
    b[i] <- (dat[i, group_var] %in% s)
  }
  dat[b, columns]
}

#' Calculate mm/hr from dbz with Marshall Palmer equation.
#'
#' @export
#' @param dbz reflectivity as measured by dbz
#' @return estimated rainfall in mm/hr.
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}

#' Nearest neighbor of a value.
#'
#' @export
#' @param v the value
#' @param n the neighbors
#' @return the value of the nearest neighbor.
nearest_neighbor <- function(v, n) {
  lapply(v, function(x) {
    if (is.na(v)) res <- NA
    else res <- n[which.min(abs(n - v))]
    res
  })
}


