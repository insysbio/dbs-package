\name{parsetgen}
\alias{parsetgen}
\alias{parsetgen.cond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for DBSolveOptimum to generate parameters and conditions using multivariate normal distribution
}
\description{
The function \code{parsetgen} generates random dataset.

The function \code{parsetgen.cond} add to the dataset from \code{parsetgen} conditions columns and nos column
}
\usage{
parsetgen(cov, mu, transform="", samples=1024)
parsetgen.cond(parset, cond=data.frame(), max.samples=nrow(parset), uniq.nos=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cov}{
  covariance matrix with dimension nxn and whose element in i,j positions is the covariance between the i and j elements.
  }
  \item{mu}{
  named vector length of n with named parameters and their expectations
  }
  \item{transform}{
  character vector containing the distribution for each parameter. It can be "" for normal distribution and "log" for log-normal distribution
  }
  \item{samples}{
  number of the samples that we want to get in the ouput dataset (1024 as default)
  }
  \item{parset}{
  output data.frame from parsetgen function or users data.frame
  }
  \item{cond}{
  conditional data.frame. If empty, the result of parsetgen.cond is input data.frame with nos column
  }
  \item{max.samples}{
  number of resulted samples for each condition. If nrow(parset)<max.samples, that provide an error. If nrow(parset)>max.samples, that cuts the data frame by number of samples
  }
  \item{uniq.nos}{
  number of unique parameter set. If FALSE, then nos will be with repeating, and if TRUE - nos will have unique number.  
  }
}
\details{
}
\value{
The output is \code{data.frame} with the columns named as names in your \code{expect} vector, and for \code{parsetgen.cond} it will be \code{data.frame} with the "nos" column, and names(parset) columns and names(cond) column
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
  \code{parsetgen} function does not provide \code{nos} column. If you need one, use 
  \code{out<-parsetgen() followed by \code{final<-parstgen.cond(out)}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mvtnorm]{rmvnorm}}\cr
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\examples{
### create parameter set for Monte-Carlo simulation using covariance matrix (example4)
example4_parset<-parsetgen(example4_stat$muCov, example4_stat$mu, example4_stat$transform)
example4_parset.cond<-parsetgen.cond(
  example4_parset,
  cond=data.frame(Dose=c(1,5,10,1,5,10), T=c(12,12,12,24,24,24))
  )
write.delim(example4_parset.cond, "example4_parset.cond.txt")

  
### create parameter set for Monte-Carlo simulation using parameter set based on bootstrapping(example4)
example4_parset_bs.cond<-parsetgen.cond(
  example4_parset_bs[,c(1,2,3,4,6)],
  cond=data.frame(Dose=c(1,5,10,1,5,10), T=c(12,12,12,24,24,24))
  )
write.delim(example4_parset_bs.cond, "example4_parset_bs.cond.txt")

### Making some parameter set with three expectation and cov as vector
  expect<-c(kcat=0.5, Vd=23.4, Km=12.4) #making expect vector
  cov<-c(23.5, 37.9, 23.5) #making vector for coviance matrix
  transform<-c("log", "", "log") #the transform vector with distributions
  output<-parsetgen(cov, expect, transform)
  
  #Making some dataset,with three expectation and cov as vector
  expect<-c(kcat=0.5,Vd=23.4,Km=12.4) #making expect vector
  cov<-matrix(c(23.5,0,1, 0,37.9,0, 1,0,23.5), ncol=3) #coviance matrix
  transform<-c("log","","log") #the transform vector with distributions
  output<-parsetgen(cov, expect, transform)
  
  #Using the parsetgen.cond function. Suppose we have output from parsetgen function
  cond<-data.frame(cond1=c(1,2.5,1), cond2=c(0,0,1), cond3=c(0,0,0))
  output1<-parsetgen.cond(output, cond=cond, max.samples=1024,uniq.nos=FALSE)
}