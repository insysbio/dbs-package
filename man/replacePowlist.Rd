\name{replacePowlist}
\alias{replacePowlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for replacing ^'s to pow inside the list
}
\description{
The function replacing ^'s to pow based on parsing the string vector
}
\usage{
replacePowlist(l,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
list with vectors containing exrpessions to replace
}
\item{x}{
names of the vector inside list
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output is the list with replaced ^'s.
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
\code{read.slv}\cr
\code{import.slv}
}
\examples{

##Example of using the function

temp1<-list(one=c("eee^3","2^rrr"))
replacePowlist(temp1,1)

}
