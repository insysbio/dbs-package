\name{read.dat}
\alias{read.dat}
\alias{import.dat}
\title{
Import .DAT files
}
\description{
The set of functions to import experimental dataset from DBSolveOptimum format .DAT file.\cr
\code{read.dat} reads .DAT file and perform initial parsing.\cr
\code{import.dat} creates \code{ruData} object from \code{read.dat} output.
}
\usage{
read.dat(file)

import.dat(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
filename of .DAT file.
}
  \item{dat}{
list object of format ruData.raw which is output of \code{read.dat} function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value of \code{import.dat} is an object of class \code{ruData} which mode is \code{list} and structure corresponds to \code{ruList}. Second level is lists each of which has the following components:
\item{data_id }{character identifier of the dataset}
\item{data }{experimental data of \code{data.frame} class}
\item{conditions }{data.frame describing conditions}
\item{solver }{character identifier of solver type: ode, explicit, implicit}
\item{error.type }{description of error model, currently possible values are: "additive T", "additive F"}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{write.list}}
}
\examples{
### read and save data from 'example4.dat'
filePath<-system.file(package = "dbs", "extdata/example4.dat")
dat_raw<-read.dat(filePath)
example4_ruData<-import.dat(dat_raw) 
write.list(example4_ruData, "example4_ruData.txt")
}
