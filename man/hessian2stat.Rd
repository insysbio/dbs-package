\name{hessian2stat}
\alias{hessian2stat}
\alias{parset2stat}
\title{
Functions for calculating statistical characteristics from hessian or parameter set
}
\description{
The functions calculate statistical characteristics based on normal distribution or transformed normal distribution.
}
\usage{
  hessian2stat(hessian, optim, transform="", level=0.95, ...)
  
  parset2stat(parset, transform="", level=0.95, norm.test=shapiro.test, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hessian}{
    Hessian matrix of second derivatives (-2logL vs parameters). It must be positive-definite.
}
  \item{optim}{
  Optimal values of parameters (best-fit values).
  }
  \item{transform}{
  String vector for what distribution is used on parameter. For default all parameters are set to normal distribution.
  }
  \item{level}{
  confidence level for calculation of confidence intervals.
  }
  \item{...}{
  other arguments passed to \code{solve}.}
  \item{parset}{
  data.frame with parameter set (mandatory for \code{parset2stat})
  }
  \item{norm.test}{
  function to perform normality test
  }
}
\details{
}
\value{
The output for this function is covariance \code{matrix} calculated based on assymptotic approach, and it takes into account the transformation of parameter space.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\examples{
### calculate statistical characteristics based on calculated hessian, see 'example4.slv' from DBSolve manual
\dontrun{example4_parset_bs<-read.delim("example4_hessian.txt")} # read hessian from file
optimal<-c(kcat=7.130016e-01, Vd=5.205980e+00, Km=5.240306e+00, kabs=2.014304e+00)
hessian2stat(hessian=as.matrix(example4_hessian), optim=optimal, transform="log")

### calculate statistical characteristics based on parameter set, see 'example4.slv' from DBSolve manual
\dontrun{example4_parset_bs<-read.delim("example4_parset_bs.txt")} # read hessian from file
parset2stat(parset=example4_parset_bs[,1:4])

}
