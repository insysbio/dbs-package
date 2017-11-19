\name{calccb}
\alias{calccb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of confidence bands
}
\description{
The function calculates pointwice confidence bands based on Monte-Carlo simulations in DBSolveOptimum. The lower and upper confidence band calculated as lower and upper quantile for interpolated particular \code{x} point.
}
\usage{
calccb(input, x.col, x.seq, y.col, factor.col = c(),
        q.seq = c(0.025, 0.5, 0.975), nos.col = "nos",
        par_calc = FALSE, cpu.cores = 4, silent = FALSE, include.nos = c(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
\code{data.frame} passed from DBSolveOptimum output
}
  \item{x.col}{
number or name of column in \code{input} corresponded to free variable (i.e. time).
}
  \item{x.seq}{
numerical vector of points to interpolate values in \code{x.col}.
}
  \item{y.col}{
vector of column numbers or names in \code{input} corresponded to simulated variables (model output).
}
  \item{factor.col}{
vector of column numbers or names in \code{input} corresponded to condition parameters (model input).
}
  \item{q.seq}{
sequence of probabilities for calculation of lower and upper quantile. The default vector \code{c(0.025, 0.5, 0.975)} corresponds to calculation of median value and 0.95 confidence band.
}
  \item{nos.col}{
number or name of column in \code{input} corresponded to enumeration of random parameter set.
}
  \item{par_calc}{
logical value to use parallel calculation for acceleration. It requires \code{parallel}, \code{foreach}, \code{iterators} packages.
}
  \item{cpu.cores}{
the number of CPU cores to use if \code{par_calc=TRUE}. 
}
  \item{silent}{
logical value to suppress the messages during calculations.
}
  \item{include.nos}{
vector of number of samples to analyze the approximation in the chosed number of sample.
}
  \item{\dots}{
other arguments passed to \code{quantile}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is \code{data.frame} class object.
The columns describe:
  \item{names(x.col) }{free variable values passed from argument \code{x.col}.}
  \item{var_id }{names of simulated variables as passed from \code{y.col}.}
  \item{quant_ }{columns represent calculated quntiles for interpolated points.}
  \item{names(factor.col) }{condition variable values passed from argument \code{factor.col}.}
  \item{group }{unique identifier for combination of \code{factor.col}.}
The value has the additional attributes:
  \item{col.def }{definition of columns, type of data in columns.}
  \item{col.title }{titles for columns. May be usefull for visualization.}
  \item{var.title }{titles for simulated variables. May be usefull for visualization.}
  \item{group.title }{titles for condition groups. May be usefull for visualization.}
  \item{approx_nos_}{column(or columns) represents interpolated points for chosed number of sample (presented only if \code{include.nos} has values in).}
}
\references{
%% ~put references to the literature/web site here ~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[foreach]{foreach}}\cr
\code{\link[stats]{quantile}}\cr
\code{\link[stats]{approxfun}}
}
\examples{
### calculation of confidence bands based on example4.slv 
\dontrun{example4_parset_bs.cond_res<-read.delim("example4_parset_bs.cond_res.txt")} # read from DBSolve output simulated with MC parameters from bootstrapping
example4_cb<-calccb(input=example4_parset_bs.cond_res,
                    x.col="t",
                    x.seq=seq(0,96,by=0.5),
                    y.col=c("C0","C1"),
                    factor.col = c("Dose","T"))
\dontrun{write.delim(example4_cb, "example4_cb.txt")} # save results

### plot all results with lattice
library(lattice)
xyplot(quant_0.025+quant_0.5+quant_0.975~t|var_id+group,
       data=example4_cb,
       type="l",
       lty=c(2,1,2),
       xlab="Time, h",
       ylab="Concentration of drug, ng/ml",
       main="All CB simulations")
       
###You can also plot all results using ggplot2:
library(ggplot2)
ggplot(example4_cb,aes(t,quant_0.025))+
        geom_line(linetype="dashed", color="blue")+
        geom_line(aes(t,quant_0.5),color="black",linetype="solid")+
        geom_line(aes(t,quant_0.975),color="blue",linetype="dashed")+
        facet_wrap(~var_id+group)+
        ggtitle("All CB simulations")+
        scale_x_continuous(name="Time,h")+
        scale_y_continuous(name="Concentration of drug, ng/ml")


### plot dbsolve output results
example4_cb<-calccb(input=example4_parset.cond_res,
                    x.col="t",
                    x.seq=seq(0,96,by=0.5),
                    y.col=c("C0","C1"),
                    factor.col = c("Dose","T"),
                    par_calc = FALSE)
}