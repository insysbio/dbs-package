\name{write.delim}
\alias{write.delim}
\title{
Data output with tab delimiters
}
\description{
prints its required argument x (after converting it to a data frame if it is not one nor a matrix) to a file with tab delimiter without quotes and row names
}
\usage{
write.delim(x, file = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.
}
  \item{file}{
a character string naming a file
}
  \item{\dots}{
other arguments passed to write.table
}
}
\details{
The function is equivalent to\cr
\code{write.table(x = x, file = file, quote = F, sep = "\t", row.names = F, ...)}
}
\seealso{
\code{\link{write.table}}
}
\examples{
### create and write data.frame
df<-data.frame(number=1:5, words=c("one", "two", "three", "four", "five"))
write.delim(df, "df.txt")
}