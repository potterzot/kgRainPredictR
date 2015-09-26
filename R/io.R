#' Write the submission file.
#'
#' @export
#'
#' @param d the data to write
#' @param file_name the name of the file.
#' @param zip boolean to zip the file or not.
#' @return TRUE
createSubmission <- function(d, file_name, zip=TRUE) {
  if (typeof(zip) != "logical") stop("zip must be TRUE or FALSE.")
  if (!file.exists(file_name)) stop(paste0(file_name, " location does not exist."))

  write.csv(d[,c("Id", "Expected")], file_name, row.names = FALSE, quote=FALSE)
  if (zip) {
    zip(paste0(file_name, ".zip"), file_name)
    file.remove(file_name)
  }
}

