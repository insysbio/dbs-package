write.delim<-function(x, file="",...)
  #x    - table to save (data.frame)
  #file - filename (character)
  #...  - other arguments passed to write.table
  write.table(x=x, file=file, quote = F, sep="\t", row.names = F, ...)
