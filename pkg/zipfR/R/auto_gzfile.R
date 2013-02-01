##
## internal: automatically read/write compressed files based on filename extension (.gz)
##
## This function returns an unopened file() or gzfile() connection, depending on whether
## <filename> ends in ".gz" (case-insensitive), with encoding set appropriately.
## It is safe to use file=auto.gzfile(filename) in read.table() and write.table().

auto.gzfile <- function (filename, encoding="") {
  stopifnot(length(filename) == 1, is.character(filename))
  if (length(grep("\\.gz$", filename, ignore.case=TRUE)) > 0) {
    gzfile(filename, encoding="")  # return connection to gzip-compressed file, which can be used for reading or writing
  }
  else {
    file(filename, encoding="")    # return file() connection for plain files
  }
}
