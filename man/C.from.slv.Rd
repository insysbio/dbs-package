\name{C.from.slv}
\alias{C.from.slv}
\title{
Create .C code from ruSlv object
}
\description{
The function creates \pkg{deSolve}-compatible C code from .SLV image and save it to a file.
}
\usage{
C.from.slv(slv, file = "model.c",
            output = slv$output.on, dbs.compatibility = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slv}{
object of \code{ruSlv} class, model image.
}
  \item{file}{
filename to save .C code.
}
  \item{output}{
character vector with names of additional output values.
}
  \item{dbs.compatibility}{
logical value to use DBSolve-like method of parameters updates.
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
\code{\link{import.slv}}\cr
\code{\link[deSolve]{deSolve}}\cr
\code{\link[deSolve]{ode}}
}
\examples{
### create and compile C code for 'example1.slv', Rtools may be reqired
C.from.slv(example1_ruSlv, output="D")
\dontrun{
system("R CMD SHLIB model.c") # compilation for .DLL
library(deSolve)
dyn.load(paste0("model", .Platform$dynlib.ext))
res<-ode(y=example1_ruSlv$ode.initials,
         times=seq(0,example1_ruSlv$solver.time.limit,0.1),
         func = "derivs",
         parms=example1_ruSlv$ode.parameters.external,
         dllname = "model",
         initfunc = "initmod",
         nout=length("D"),
         outnames = "D"
  )
dyn.unload(paste0("model", .Platform$dynlib.ext))  
plot(res)
}

### plot simulations from SLV model 
\dontrun{
  filePath<-system.file(package = "dbs", "extdata/example1.slv")
  raw<-read.slv(filePath) # read from example
  compatible.slv(raw) # TRUE
  example1_ruSlv<-import.slv(raw)
  C.from.slv(example1_ruSlv, output="D")
  system("R CMD SHLIB model.c") # compilation for .DLL
  library(deSolve)
  dyn.load(paste0("model", .Platform$dynlib.ext))
  res<-ode(y=example1_ruSlv$ode.initials,
           times=seq(0,example1_ruSlv$solver.time.limit,0.1),
           func = "derivs",
           parms=example1_ruSlv$ode.parameters.external,
           dllname = "model",
           initfunc = "initmod",
           nout=length("D"),
           outnames = "D"
  )
  dyn.unload(paste0("model", .Platform$dynlib.ext))  
  plot(res)
}
}