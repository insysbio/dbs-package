\name{read.list}
\alias{read.list}
\alias{check.list}
\alias{write.list}
\title{
Read, write and check object of ruList format 
}
\description{
The common functions for manipulating objects of structure \code{ruList}: reading from file, writing to file of format \code{ruList.txt} and checking \code{ruList} object for appropriateness.
}
\usage{
read.list(file)

write.list(x, file="")

check.list(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the object of format \code{ruList}
}
  \item{file}{
a character string naming a file
}
}
\details{
\code{ruList} and \code{ruList.txt} is the internal \code{dbs-package} format for representing complex objects.

The structure of \code{ruList} format can be described as three-level nested object: (1) \code{list} with any number of elements of 2-d level, (2) \code{list} with any number of elements of 3-d level, (3) objects of classes: data.frame, matrix, numeric, character, integer, logical, mod.frame. Any level may have attributes of classes: data.frame, matrix, numeric, character, integer, logical.

The \code{ruList.txt} is a human readable representation of \code{ruList} object saved to .TXT file. 
}
\value{
\code{read.list} returns the object of \code{ruList} format

\code{check.list} returns logical \code{TRUE} if the object \code{x} has appropriate structure
}
\note{
The current version of \code{check.list} does not check attributes of 3-d level.

The current version of \code{write.list} does not check \code{x} argument for consistency. Be carefull.
}
\seealso{
\code{\link{list}}
}
\examples{
### write, read and check ruList
models<-list(example4_ruSlv)
write.list(models, "models.txt")
models1<-read.list("models.txt")
check.list(models1) # output: TRUE
all.equal(models1, models) # output: 'names for target but not for current'
}