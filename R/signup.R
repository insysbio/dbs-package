signup<-function(x, digits = 6)
  # Function parameters:
  #x    - vector to round up (numeric)
  #digits    - number of significant numbers (integer)
  ceiling(x/10^(ceiling(log10(x))-digits))*10^(ceiling(log10(x))-digits)

signdown<-function(x, digits = 6)
  #x    - vector to round down (numeric)
  #digits    - number of significant numbers (integer)
  floor(x/10^(ceiling(log10(x))-digits))*10^(ceiling(log10(x))-digits)
  