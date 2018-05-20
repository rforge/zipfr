##
## some utility functions and preferences
##

## zipfR graphics configuration
six.colors <- c("#000000", "#D65F5F", "#4878CF","#6ACC65","#B47CC7","#C4AD66")
zipfR.par(col=six.colors, barcol=six.colors, lty=rep(c("solid", "22"), c(6, 4)), lwd=rep(4, 10))

## Perl's qw() construction
qw <- function (x, sep="\\s+") unlist(strsplit(x, sep, perl=TRUE))
