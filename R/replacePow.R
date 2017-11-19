##########################
### Function for replacing ^ to pow
### 15.11.2016-18.11.2016
### A.Kalinkin
#########################

replacePow <- function(e) {    
  #check if you are at the end of the tree's branch
  #e - string as expression(using parse function)
  if (is.name(e) || is.atomic(e)) { 
    #replace ^
    if (e == quote(`^`)) return(quote(pow))
    return(e)
  }
  if (e[[1]] == quote(sqrt)) {
    #replace sqrt
    e[[1]] <- quote(pow)
    #add the second argument
    e[[3]] <- quote(0.5)
  }
  #follow the tree with recursion
  for (i in seq_along(e)) e[[i]] <- replacePow(e[[i]])
  return(e)    
}

print_parsed <- function(e) {
  deparse(replacePow(e)[[1]])
}

parsed <- function(t) {
  e=parse(text=t)
  return(e)
}