##
## internal: automatically read/write compressed files based on filename extension (.gz)
##
## This function returns an unopened file(), gzfile() or bzfile() or xzfile() connection,
## depending on whether <filename> ends in ".gz", ".bz2" or ".xz" (case-insensitive),
## with character encoding set appropriately.
## It is safe to use file=auto.gzfile(filename) in read.table() and write.table().

auto.gzfile <- function (filename, encoding=getOption("encoding")) {
  stopifnot(length(filename) == 1, is.character(filename))
  if (grepl("\\.gz$", filename, ignore.case=TRUE)) {
    gzfile(filename, encoding=encoding)  # return connection to gzip-compressed file, which can be used for reading or writing
  }
  if (grepl("\\.bz2$", filename, ignore.case=TRUE)) {
    bzfile(filename, encoding=encoding)  # bzip2-compressed file
  }
  if (grepl("\\.xz$", filename, ignore.case=TRUE)) {
    xzfile(filename, encoding=encoding)  # xz-compressed file
  }
  else {
    file(filename, encoding=encoding)    # return file() connection for plain files
    ## when used for reading, this should auto-detect compressed files with different extension
  }
}
