# new build file
# https://www.rstudio.com/products/rpackages/devtools/
# https://github.com/klutometis/roxygen

setwd("Y:\\dbs-package")

library(devtools)
library(dbs)

out <- check(manual=TRUE)
write(out[[2]], "warn.log")

build(path = "dist", binary = TRUE, manual=TRUE) # build for windows binary = TRUE

install() # build and install

install_github("insysbio/dbs-package")

#load_all() # 
