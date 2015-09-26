#' convert zipped csv files to binary RDS.
#'
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
zip2rds <- function(files, dir_in, dir_out,
                    dir_sep="/",
                    chunks = 20,
                    headers = TRUE,
                    compression = c("gzip"),
                    ...) {
  if (chunks > 26) stop("chunks cannot currently be more than 26.")
  if (missing(dir_out)) dir_out <- dir_in
  if (!is.logical(compression)) compression <- match.arg(compression)

  alpha <- strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]]

  file_names <- lapply(files, function(x) { strsplit(x, "\\.")[[1]][1] })

  wd = getwd()
  setwd(dir_in)

  for (f in file_names) {
    zip_name = paste(dir_in, paste0(f, ".zip"), sep = dir_sep)
    csv_name = paste(dir_in, paste0(f, ".csv"), sep = dir_sep)
    rds_name = paste(dir_out, paste0(f, ".rds"), sep = dir_sep)

    system(paste0("unzip ", zip_name))

    #open a connection
    if (compression == "gzip") con = gzfile(rds_name, 'wb')
    else con = file(rds_name, 'wb')

    #loop through split of file
    system(paste0("split ", csv_name, " -n 20"))
    for (chunk in alpha[1:chunks]) {
      chunk_name = paste(dir_in, paste0("xa", chunk), sep=dir_sep)
      if (chunk == "a") headers = headers
      else headers = FALSE
      d = read.csv(chunk_name, header=headers, ...)
      saveRDS(d, con)
      file.remove(chunk_name)
    }
    file.remove(csv_name)
    close(con)
  }
  setwd(wd)
  invisible(TRUE)
}
