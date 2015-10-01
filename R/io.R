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
  if (!file.exists(dirname(file_name))) stop(paste0(file_name, " location does not exist."))

  write.csv(d[,c("Id", "Expected")], file_name, row.names = FALSE, quote=FALSE)
  if (zip) {
    zip(paste0(file_name, ".zip"), file_name, flags = "-jr9X")
    file.remove(file_name)
  }
}

#' filter and convert zipped csv to gzipped csv.
#'
#' @export
#' @param in_file name of csv file.
#' @param out_file name of file to save.
#' @param filter_vector boolean vector to filter on.
#' @param number_chunks number of chunks to use. Overrides chunk_size.
#' @param chunk_size number of rows in each chunk.
#' @param compression whether to compress the saved file.
#' @param ...
#' @return TRUE if no errors.
ff2gzip <- function(in_file, out_file, filter_vector,
                     number_chunks,
                     chunk_size = 100000L,
                     compression = 9, ...) {
  ffload(file = in_file, rootpath = "./", overwrite = FALSE)

  N = length(filter_vector)
  if (!missing(number_chunks)) chunk_size = ceiling(N / number_chunks)

  #open a connection
  if (!as.logical(compression)) con <- file(out_file, open="w")
  else con <- gzfile(out_file, open="wb", compression = compression)

  for (i in 1:ceiling(N/chunk_size)) {
    idx <- 1:chunk_size + (i-1)*chunk_size
    if (idx[chunk_size] > N) idx <- 1:(N %% chunk_size) + (i-1)*chunk_size
    dat <- fread(in_file, nrows = chunk_size)[filter_vector[idx],]
    if (i==1) {
      write.csv(dat, file = con,
                append = FALSE,
                row.names = FALSE)
    }
    else {
      write.csv(dat, file = con,
                append = TRUE,
                row.names = FALSE)
    }
  }

  close(con)
}


#' convert zipped csv files to FFDF format.
#'
#' @import ff
#' @importFrom lazyeval interp
#' @export
#'
#' @param files list of file names.
#' @param dir_in directory source files are in.
#' @param dir_out directory to save rds files in.
#' @param dir_sep separator to join file names to directories.
#' @param chunks number of chunks to split the text file into while converting.
#' @param headers logical indicating csv has headers.
#' @param compression type of compression to use or FALSE if none.
#' @param ... arguments passed to \code{read.csv()}
#' @return TRUE
zip2ff <- function(file, dir_in, dir_out,
                   dir_sep="/",
                   chunks = 20,
                   headers = TRUE,
                   compression = c("gzip"),
                   ...) {
  if (missing(dir_out)) dir_out <- dir_in
  if (!is.logical(compression)) compression <- match.arg(compression)

  wd <- getwd()
  setwd(dir_in)

  zip_name <- paste(dir_in, paste0(f, ".zip"), sep = dir_sep)
  csv_name <- paste(dir_in, paste0(f, ".csv"), sep = dir_sep)
  ff_name <- paste(dir_out, f, sep = dir_sep)

  system(paste0("unzip -n ", zip_name))

  #weird problem with temp dir, so set it to the user raw data directory
  fftempdir <- options("fftempdir")
  options("fftempdir" = dir_in)

  data <- read.csv.ffdf(file = csv_name, ...)

  ffsave(data, file = ff_name, compress = TRUE)

  file.remove(csv_name)

  ff_files <- list.files(dir_in)
  file.remove(ff_files[which(substr(ff_files, 1, 2) == "ff")])
  options("fftempdir" = fftempdir)
  setwd(wd)
  invisible(TRUE)
}
