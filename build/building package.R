# new build file
# https://www.rstudio.com/products/rpackages/devtools/
# https://github.com/klutometis/roxygen

setwd("Y:\\dbs-package")

library(devtools)
# library(dbs)

### Just build and install the package from current directory
install() # build and install

### Install the latest version from github
install_github("insysbio/dbs-package")

### ???
out <- check(manual=TRUE)
write(out[[2]], "warn.log")

build(path = "dist", binary = TRUE, manual=TRUE) # build for windows binary = TRUE

#load_all()
