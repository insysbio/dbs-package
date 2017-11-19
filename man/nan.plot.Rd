\name{nan.plot}
\alias{nan.plot}
\alias{nan.points}
\title{
Plot all points including infinite
}
\description{
The functions to plot all points of the dataset even they are out of the \code{xlim} and \code{ylim} range.
}
\usage{
nan.plot(x, y, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
          log = c("", "x", "y", "xy"), ...,
          force.bound = FALSE, delta = 0.1)

nan.points(x, y, ...)
}
\arguments{
  \item{x}{
numeric vector to plot on x-axis.
}
  \item{y}{
numeric vector to plot on y-axis.
}
  \item{xlim}{
the x limits (x1, x2) for main region.
}
  \item{ylim}{
the y limits (y1, y2) for main region.
}
  \item{log}{
a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
}
  \item{\dots}{
other graphical parameters passed to \code{plot.default} or \code{points}
}
  \item{force.bound}{
logical value to forcefully create the extended region. \code{F} means the extended region is created if some of points is out of the main region.
}
  \item{delta}{
the relative size of region based on main region size.
}
}
\details{
If someone use the default \code{plot} and \code{points} method the points out of \code{xlim} and \code{ylim} bacame invisible. In some cases it is not OK. The presented functions creates the exteded region and plot all points including infinete values there.

The function work only for numerical vectors but not for data.frame, matrix, etc.
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
\note{
These are experimental functions so the troubles are possible.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.default}}\cr
\code{\link{points}}\cr
}
\examples{
### comparison of exponential plots
x<-seq(0,20, 0.1)
y<-5*exp(x)
plot(x,y) # default plot without limits
plot(x,y, ylim=c(0,100)) # default plot with y limits
nan.plot(x,y, ylim=c(0,100)) 
}
