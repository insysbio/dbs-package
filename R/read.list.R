############################
### Function to read/write/check ruList object from file
### for dbs-package
### 10.05.2015-17.05.2015
### E.Metelkin
###########################

### function to transform data from ruList.txt to ruList
read.list<-function(file)
{
  # Function parameters:
  # file      - file with list in ruList.txt format (character)
  
  lns<-readLines(file)
  startLines<-grep("^\\${1}|^@",lns)
  hir<-rep(0,length(startLines))
  hir[grep("^\\$", lns[startLines])]<-1
  hir[grep("^\\${2}", lns[startLines])]<-2
  is.attr<-grepl("^\\$*@",lns[startLines])
  len<-diff(c(startLines,length(lns)+1))-1
  #len<-diff(c(startLines,length(lns)+2))-1
  nms<-gsub("\\$|@","", lns[startLines])
  
  # argument checking must be true
  if (!all(len[is.attr==F & hir %in% c(1,0)]==0)) stop ("Unclear structure of the object!")
  
  ### aux function
  templev1<-function(start,end)
  {
    if (start>end)
    {
      character()
    } else {
      if (any(grepl("\\t",lns[start:end])))
        read.table(file=file, header=T, sep="\t", nrows=end-start, skip=start-1, stringsAsFactors=F, blank.lines.skip=F)
      else
        read.table(file=file, header=F, sep="\t", nrows=end-start+1, skip=start-1, stringsAsFactors=F, blank.lines.skip=F)[[1]]
    }
  }
  
  # create top object: list with sublists
  chosen_value<-which(!is.attr & hir==1) 
  chosen_value_len<-diff(c(chosen_value,length(startLines)+1))
  res<-vector("list", length(chosen_value))
  
  # prepare and add attributes of 0 level
  chosen_attr<-which(is.attr & hir==0)
  res_attr<-lapply(chosen_attr, function(x) templev1(startLines[x]+1, startLines[x]+len[x]))
  names(res_attr)<-nms[chosen_attr]
  attributes(res)<-c(res_attr, list(names=nms[chosen_value])) # add attributes including names
  
  # add sublists of 1 and attributes of 1 and 2 level
  for (i in seq_along(chosen_value))
  {
    chosen_value_i<-which(!is.attr & hir==2 & chosen_value[i]<seq_along(startLines) & chosen_value[i]+chosen_value_len[i]>seq_along(startLines))
    res[[i]]<-lapply(chosen_value_i, function(x) templev1(startLines[x]+1, startLines[x]+len[x]))
    #
    chosen_attr_i<-which(is.attr & hir==1 & chosen_value[i]<seq_along(startLines) & chosen_value[i]+chosen_value_len[i]>seq_along(startLines))
    res_attr_i<-lapply(chosen_attr_i, function(x) templev1(startLines[x]+1, startLines[x]+len[x]))
    names(res_attr_i)<-nms[chosen_attr_i]
    attributes(res[[i]])<-c(res_attr_i, list(names=nms[chosen_value_i])) # add attributes including names
    #
    chosen_value_len_i<-diff(c(chosen_value_i,chosen_value_len[i]+chosen_value[i]))
    #print(chosen_value_len[i]+chosen_value[i]); print("==\n==")
    for (j in seq_along(chosen_value_i))
    {
      chosen_attr_j<-which(is.attr & hir==2 & chosen_value_i[j]<seq_along(startLines) & chosen_value_i[j]+chosen_value_len_i[j]>seq_along(startLines))
      res_attr_j<-lapply(chosen_attr_j, function(x) templev1(startLines[x]+1, startLines[x]+len[x]))
      names(res_attr_j)<-nms[chosen_attr_j]
      attributes(res[[i]][[j]])<-c(attributes(res[[i]][[j]]),res_attr_j) # add attributes
      #print(chosen_attr_j); print("==\n==")
    }
  }
  
  return(res)
}

### function to transform data from ruList to ruList.txt
###to do list
#add checking of ruList object

write.list<-function(x, file="")
{
  # Function parameters:
  # x - ruList object (ruList)
  # file      - file to read list in ruList.txt format (character)
  
  #if (!check.ruList(x)) stop("The object is not ruList object!")
  
  concon<-file(file, "w")
  
  savdata1<-function(dt)
  {
    if (mode(dt)=="list")
      write.table(x=dt, file=concon, sep="\t", row.names=F, col.names = T, quote = F)
    else
      write.table(x=as.vector(dt), file=concon, sep="\t", row.names=F, col.names = F, quote = F)
  }
  
  savlist1<-function(ll, flag="", exclude.names=F)
  {
    if (exclude.names)
      l<-ll[! (names(ll) %in% "names")]
    else
      l<-ll
    for (i in seq_along(l))
    {
      cat(paste0(flag, names(l)[i]), file=concon, sep="\n")
      savdata1(l[[i]])
    }
  }
  
  #add attributes level 0
  savlist1(attributes(x),"@",T)
  
  #create sublists and add attributes level 1
  for (i in seq_along(x))
  {
    cat(paste0("$", names(x)[i]), file=concon, sep="\n")
    savlist1(attributes(x[[i]]),"$@",T)
    for (j in seq_along(x[[i]]))
    {
      savlist1(x[[i]][j],"$$")
      tem<-attributes(x[[i]][[j]])
      #savlist1(tem[!(names(tem) %in% exclude)],"$$@")
      savlist1(tem,"$$@")
    }
    
  }
  close(concon)
}

###to do list
#checking structure of attributes of 3-d level
#allow class POSIXct, or other double classes

check.list<-function(x)
{
  # Function parameters:
  # x - ruList object (ruList)
  
  res<-T
  
  ###checking 1-st level
  if(!"list" %in% mode(x))
  {
    message("The object is not a list.")
    res<-F
  }
  if(!all(sapply(attributes(x),class) %in% c("data.frame", "matrix", "numeric", "character", "integer")))
  {
    message("Some of 1-st level attributes has unappropriate class.")
    res<-F
  }
    
  ###checking 2-d level
  if(!all(sapply(x,mode)=="list"))
  {
    message("Some of 2-st level elements are not lists.")
    res<-F
  }
  for (i in seq_along(x))
  {
    if(!all(sapply(attributes(x[[i]]),class) %in% c("data.frame", "matrix", "numeric", "character", "integer")))
    {
      message("Some of 2-d level attributes has unappropriate class.")
      res<-F
    }
  }
  
  ###checking 3-d level
  for (i in seq_along(x))
  {
    if(!all(sapply(x[[i]],class) %in% c("data.frame", "matrix", "numeric", "character", "integer", "logical")))
    {
      message("Some of 3-d level objects has unappropriate class.")
      res<-F
    }
    #add here the checking of 3-d level attributes 
  }
  
  return(res)  
}