\name{rct.from.slv}
\alias{rct.from.slv}
\title{
Create .RCT from ruSlv
}
\description{
The function analyzes ruSlv stoicheometry matrix and creates reaction list in .RCT format.  
}
\usage{
rct.from.slv(y)
}
\arguments{
  \item{y}{
the object of class \code{ruSlv}, model image.
}
}
\details{
The function uses the stoicheometry matrix, names of rates and metabolites only. The true structure of differential equation is not taken into account.
}
\value{
character vector representing list of reactions which can be saved using \code{cat(..., sep="\\n")}.
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
\code{\link{import.slv}}\cr
\code{\link{cat}}\cr
}
\examples{
### create .RCT file from 'example4.slv'
rct<-rct.from.slv(example4_ruSlv)
cat(rct, file="example.rct", sep="\n")
}