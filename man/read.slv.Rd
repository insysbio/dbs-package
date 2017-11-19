\name{read.slv}
\alias{read.slv}
\alias{compatible.slv}
\alias{import.slv}
\title{
Import .SLV files to R
}
\description{
A set of functions to import model files of DBSolveOptimum (.SLV) to R-environment as the object of class \code{ruSlv}. \cr
\code{read.slv} function reads .SLV file and creates \code{list} of format \code{ruSlv.raw}\cr
\code{compatible.slv} function checks compatibility of \code{ruSlv.raw} passed from \code{read.slv} for compatibility with current version of \code{import.slv}.\cr
\code{import.slv} function analyzes \code{ruSlv.raw} passed from \code{read.slv} and creates the object of class \code{ruSlv}.
}
\usage{
read.slv(file)

compatible.slv(x)

import.slv(x)
}
\arguments{
  \item{file}{
valid .SLV file saved from DBSolveOptimum.
}
  \item{x}{
object of format \code{ruSlv.raw} to use for model import.
}
}
\details{
In many cases DBSolveOptimum features are not enough to manipulate the model structure and to simulate specific conditions. This set of functions transforms all the structure of .SLV file to the R-environment for easy manipulation.

\code{read.slv} function reads model code from file and perform initial parsing based on SLV version described in file \file{slv25tab.csv}.

\code{import.slv} analyzes different parts of \code{read.slv} output and create an object of class \code{ruSlv} which is an image of initial .SLV file.
\code{compatible.slv} is developed to check the structure of \code{ruSlv.raw} object for further parsing using \code{import.slv}. It is part of \code{import.slv} function so it is not necessary to use it separately.
}
\value{
\item{read.slv }{returns \code{list} of the format \code{ruSlv.raw}}
\item{compatible.slv }{returns \code{TRUE} if \code{x} output is compatible with current version of \code{import.slv } and \code{FALSE} otherwise}
\item{import.slv }{returns the object of mode \code{list} and \code{ruSlv} class attribute  }
}
\references{
%% ~put references to the literature/web site here ~
}
\note{
The current version of dbs-package officially supports only .SLV version 25. Try \code{compatible.slv} to check.\cr
Known restrictions:
\enumerate{
\item Cannot read 'events'
\item Cannot read 'fit conditions'
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rct.from.slv}}\cr
\code{\link{C.from.slv}}
}
\examples{
### import 'example4.slv'
filePath<-system.file(package = "dbs", "extdata/example4.slv")
raw<-read.slv(filePath) # read from example
compatible.slv(raw) # TRUE
example4_ruSlv<-import.slv(raw)

### import 'example1.slv'
filePath<-system.file(package = "dbs", "extdata/example1.slv")
raw<-read.slv(filePath) # read from example
compatible.slv(raw) # TRUE
example1_ruSlv<-import.slv(raw)
}
