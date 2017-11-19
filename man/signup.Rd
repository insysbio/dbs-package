\name{signup}
\alias{signup}
\alias{signdown}
\title{
Up- and down- rounding
}
\description{
\code{signup} rounds the values in its first argument to the specified number of significant digits upwards.

\code{signdown} rounds the values in its first argument to the specified number of significant digits downwards.
}
\usage{
signup(x, digits = 6)

signdown(x, digits = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector.
}
  \item{digits}{
integer indicating the number of significant digits to be used. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{signif}}
}
\examples{
signup(c(1.111, 1.2345e5, 9.8765e-5), 3)

signdown(c(1.111, 1.2345e5, 9.8765e-5), 3)
}
