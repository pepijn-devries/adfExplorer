% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02amigaDiskBasics.r
\docType{methods}
\name{is.amigaDOS}
\alias{is.amigaDOS}
\alias{is.amigaDOS,amigaDisk-method}
\title{Check if amigaDisk object is DOS formatted}
\usage{
\S4method{is.amigaDOS}{amigaDisk}(x)
}
\arguments{
\item{x}{An \code{\link{amigaDisk}} object for which
the check should be performed.}
}
\value{
Returns a \code{logical} value, indicating whether the
disk is DOS formatted. When it is not, the attributes to the
returned value will contain information as to why the disk is
not DOS compatible.
}
\description{
This method checks if there is a DOS file structure is present
on the \code{\link{amigaDisk}} object.
}
\details{
Not all Amiga Disk Files have a DOS file structure on them.
This function checks if there is.
}
\examples{
data(adf.example)

## let's check if the example amigaDisk object
## is DOS formatted:

is.amigaDOS(adf.example)

## it apparently is
}
\author{
Pepijn de Vries
}
