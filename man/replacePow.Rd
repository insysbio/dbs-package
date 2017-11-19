\name{replacePow}
\alias{replacePow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replacing ^ to Pow
}
\description{
Function \code{replacePow(e)} used in \code{import.slv} to replace ^ to pow, for convert to C model file without errors.
}
\usage{
replacePow(e)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{
expression with ^ and sqrt as expression type. 
}
}
\details{
For using function \code{replacePow(e)}, You need to parse your text and after using function deparse to obtain the text.
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
\code{\link{C.from.slv}}\cr
\code{\link{read.slv}}
}
\examples{
#Example to using the replacePow function

text = "Cr1=fdr^x+x^y+sqrt(25^(x+3))-sqrt(sqrt(13-x)+(4*x-5)^(2+y))"
e=parse(text=text) #Its neccesary to use parse and deparse functions
print(deparse(replacePow(e)[[1]])) #printing text with pow 

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}