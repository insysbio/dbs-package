\name{calcop}
\alias{calcop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate interpolation for optimal values
}
\description{
The function interpolates simulations in DBSolveOptimum for the series of conditions and create \code{mod.frame} object.
}
\usage{
calcop(input, x.col, x.seq, y.col, factor.col = c())
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is \code{mod.frame} class object which is extension of \code{data.frame} class with the additional attributed. The columns describe:
  \item{names(x.col) }{free variable values passed from argument \code{x.col}.}
  \item{var_id }{names of simulated variables as passed from \code{y.col}.}
  \item{simulation }{column represents simulation value for interpolated points.}
  \item{names(factor.col) }{condition variable values passed from argument \code{factor.col}.}
  \item{group }{unique identifier for combination of \code{factor.col}.}
The value has the additional attributes:
  \item{col.def }{definition of columns, type of data in columns.}
  \item{col.title }{titles for columns. May be usefull for visualization.}
  \item{var.title }{titles for simulated variables. May be usefull for visualization.}
  \item{group.title }{titles for condition groups. May be usefull for visualization.}
}
\references{
%% ~put references to the literature/web site here ~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calccb}}\cr
\code{\link[stats]{approxfun}}
}
\examples{
### calculation based on example4.slv
\dontrun{example4_output_op<-read.delim("dbs_output_op.txt")} # read from output
example4_op<-calcop(input=example4_output_op,
                    x.col="t",
                    x.seq=seq(0,96,by=0.5),
                    y.col=c("C0","C1"),
                    factor.col = c("Dose","T"))
\dontrun{write.delim(example4_op, "example4_op.txt")} # save results

### plot all results with lattice
library(lattice)
xyplot(simulation~t|var_id+group,
       data=example4_op,
       type="l",
       lty=1,
       xlab="Time, h",
       ylab="Concentration of drug, ng/ml",
       main="All CB simulations")
}
