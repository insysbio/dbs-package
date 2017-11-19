\name{clean.comments}
\alias{clean.comments}
\title{
Clean C-style text
}
\description{
Function takes character vector of C-style lines (as passed from \link{readLines}), delete comments, spaces, empty lines, multiple semicolomns and line breaks.
}
\usage{
clean.comments(input)
}
\arguments{
  \item{input}{
     character vector of C-style text lines, vector can be passed from \link{readLines} function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Character vector of cleaned C-style text. It can be saved to file by \link{cat}(..., sep = "\\n") function.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{cTextExample}
(cln<-clean.comments(cTextExample))
cat(cln, file="cln.txt", sep = "\n") # save text to file
}
\keyword{ C-style code }
