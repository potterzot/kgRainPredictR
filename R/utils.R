#' Sample the data repeatedly and analyze.
#'
#' @export
#'
#' @param x the vector or data frame to sample.
#' @return results of analysis
sample_test <- function(x, id_var, target_var, FUN, ...,
                        nrounds = 1, sample_percent = .75) {
  N <- nrow(x)
  sample_size = floor(sample_percent * N)
  ids <- as.matrix(x[, id_var, with = FALSE])

  err = array(dim = nrounds)
  for (i in 1:nrounds) {
    s_tr <- sort(sample(ids, sample_size))
    b <- rep(FALSE, N)
    b[which(ids %in% s_tr)] <- TRUE
    m <- FUN(data = subset(x$Id, b), ...)
    res <- data.frame(
      pred = predict(object = m, newdata = x[!b]),
      actual = x[!b, target_var, with = FALSE]
    )
    err[i] = res$actual - res$pred
  }
  err
}


#' Adjust the target values to account for various factors.
#'
#' Attempts to adjust for various limitations with the target data. These
#' include: (1) guage resolution is likely 0.1mm or less, (2) values over 69 are
#' extremely unlikely, etc...
#'
#' @export
#'
#' @param orig vector of original values.
#' @param resolution the resolution of the measuring device.
#' @param v_max the maximum allowable value of the device.
#' @return the adjusted values.
#'
adjust_target <- function(orig, resolution = -1.25, v_max = 69) {
  if (!is.null(v_max)) orig[orig>v_max] <- NA
  round(orig/resolution)*resolution
}

#' Calculate the time difference.
#'
#' @importFrom zoo na.fill
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


