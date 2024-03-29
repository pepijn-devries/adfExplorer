% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04fileOperations.r
\docType{methods}
\name{adf.file.info}
\alias{adf.file.info}
\alias{adf.file.info,amigaDisk,character-method}
\alias{adf.file.mode}
\alias{adf.file.mode,amigaDisk,character-method}
\alias{adf.file.mode<-}
\alias{adf.file.mode<-,amigaDisk,character,character-method}
\alias{adf.file.mode<-,amigaDisk,character,logical-method}
\alias{adf.file.time}
\alias{adf.file.time,amigaDisk,character,missing-method}
\alias{adf.file.time,amigaDisk,character,character-method}
\alias{adf.file.time<-}
\alias{adf.file.time<-,amigaDisk,character,missing,POSIXt-method}
\alias{adf.file.time<-,amigaDisk,character,character,POSIXt-method}
\alias{adf.file.size}
\alias{adf.file.size,amigaDisk,character-method}
\title{File information on virtual amigaDisk objects}
\usage{
\S4method{adf.file.info}{amigaDisk,character}(x, path)

\S4method{adf.file.mode}{amigaDisk,character}(x, path)

\S4method{adf.file.mode}{amigaDisk,character,character}(x, path) <- value

\S4method{adf.file.mode}{amigaDisk,character,logical}(x, path) <- value

\S4method{adf.file.time}{amigaDisk,character,missing}(x, path, which)

\S4method{adf.file.time}{amigaDisk,character,character}(x, path, which = c("m", "c", "a"))

\S4method{adf.file.time}{amigaDisk,character,missing,POSIXt}(x, path, which) <- value

\S4method{adf.file.time}{amigaDisk,character,character,POSIXt}(x, path, which = c("m", "c", "a")) <- value

\S4method{adf.file.size}{amigaDisk,character}(x, path)
}
\arguments{
\item{x}{An \code{\link{amigaDisk}} object in which this method
will obtain file information.}

\item{path}{A (\code{vector} of) \code{character} string(s) representing a file or directory name.
Use Amiga specifications for file name (see \code{\link{current.adf.dir}}). Wildcards are not allowed.}

\item{value}{In case of \code{adf.file.time} an object of class \code{\link[base:DateTimeClasses]{base::POSIXt}}.
In case of \code{adf.file.mode} either a character string representing the flags,
or a \code{vector} of named \code{logical} values, where the name of the \code{logical}
represents the flag to be altered (see also details).}

\item{which}{Character indicating which time to obtain/modify. One of
\code{"m"} (date modified), \code{"c"} (date created), or \code{"a"} (date root modification).
This parameter works only on the disk's root and will be ignored for any
other directory or file.}
}
\value{
In case of the replace methods, an \code{\link{amigaDisk}} class object is returned with the file
information updated. Otherwise, it will return the requested file information (see also details).
}
\description{
Obtain file information of file from a virtual \code{\link{amigaDisk}} object.
}
\details{
Use \code{adf.file.mode} to obtain or set a \code{character} string reflecting which
file mode flags are set, where:
\itemize{
\item \code{D}: deletable
\item \code{E}: executable
\item \code{W}: writeable
\item \code{R}: readable
\item \code{A}: archived
\item \code{P}: pure command
\item \code{S}: script
\item \code{H}: hold
\item starting without lower case: applies to user
\item starting with lower case \code{g}: applies to group
\item starting with lower case \code{o}: applies to other
}

Use \code{adf.file.time} to obtain or set the \link[base:DateTimeClasses]{base::POSIXt} properties of
a file on an \code{\link{amigaDisk}}.

Use \code{adf.file.info} to obtain a combination of the information
listed above in a \code{data.frame}.
}
\examples{
\dontrun{
data(adf.example)

adf.file.mode(adf.example,  c("mods", "mods/mod.intro"))
adf.file.time(adf.example, c("mods", "mods/mod.intro"))
adf.file.size(adf.example,  c("mods", "mods/mod.intro"))
adf.file.info(adf.example,  c("mods", "mods/mod.intro"))

## set the writeable flag for a group to TRUE
adf.file.mode(adf.example, "mods/mod.intro") <- c(gW = T)

## Set the modified time-stamp to the current system time
adf.file.time(adf.example, "mods/mod.intro") <- Sys.time()
}
}
\author{
Pepijn de Vries
}
