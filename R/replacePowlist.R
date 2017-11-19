##########################
### Function for replacing ^ to pow in list
### 24.11.2016-25.11.2016
### A.Kalinkin
#########################

replacePowlist <- function(l,x) {
  #l - list with rhs and iv
  #x - elements in list
  for (i in 1:length(l[[x]])) {
    if (!(grepl("^\\/\\/",l[[x]][i]))) {
      if ((grepl("\\^",l[[x]][i])) || (grepl("(sqrt)",l[[x]][i]))) {
        e = parse(text = l[[x]][i])
        l[[x]][i] <- deparse(replacePow(e)[[1]])
        l[[x]][i] <- sub("([^\n])$","\\1;",l[[x]][i])
      }
    }
  }
  return(l)
}
  