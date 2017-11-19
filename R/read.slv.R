##########################
### Functions for read and analyze dbsolve files
### 28.04.2015-05.05.2015
### E.Metelkin, A.Alekseev
#########################

### function to transform data from SLV to ruSlv.raw
### To do list
# read events, fit conditions

read.slv<-function(file) # transform SLV file to list-object
{
  # Function parameters:
  # file: SLV filename (character)
  
  defaultVersion<-c("SLV25.00*") #available versions
  availableVersions<-c("SLV25.00*") #available versions
  
  ### argument checking
  if (missing(file)) stop("argument file is missing, with no default!")
  
  ### file reading
  slv<-readLines(file)

  if (!(slv[2] %in% availableVersions)) warning("SLV file has the unknown format:", slv[2], ". The structure will be analyzed as:", defaultVersion)
  else message("The file of version ", slv[2], " has been succesefully read.")
  
  ### reading
  sf<-system.file(package = "dbs", "extdata", mustWork = T)
  slv_tab<-read.csv(paste(sf, "slv25tab.csv", sep="/"),stringsAsFactors =F)
  slv_tab$found<-match(slv_tab$mark,slv)+slv_tab$move
  
  ### choosing
  temp1<-unique(slv_tab[slv_tab$element!="","element"])
  slv_tab0<-slv_tab[slv_tab$end==0,]
  mm0<-slv_tab0[match(temp1, slv_tab0$element), "found"]
  slv_tab1<-slv_tab[slv_tab$end==1,]
  mm1<-slv_tab1[match(temp1, slv_tab1$element), "found"]

  elements<-data.frame(element=temp1, begin=mm0, end=mm1)
  elements$end<-ifelse(is.na(elements$end),elements$begin, elements$end)
  
  tempfun1<-function(el)
  {
    t1<-match(el,elements$element)
    slv[(elements[t1,2]):(elements[t1,3])]
  }
  
  res<-sapply(as.character(temp1), tempfun1, USE.NAMES=T)
  attr(res,"currenttime")<-Sys.time()
  attr(res,"filename")<-file
    
  return(res)
}

### function to check compartibility of ruSlv.raw for ruSlv transformation
compatible.slv<-function(x)
{
  # Function parameters:
  # x: file in ruSlv.raw format (list)
  
  res=T
  ###checking arguments
  if ("list"!= mode(x)) stop("argument is not a list!")
  if (!all(c("rhs", "iv", "compound.names", "stoicheometry", "stoicheometry.nrows", "stoicheometry.ncols", "ode.dimension") %in% names(x)))
    { message("Some of important slv parts is missing."); res=F}

  # comment old style structure
  rhs<-sub("<\\?","//<?",x$rhs)
  iv<-sub("<\\?","//<?",x$iv)
  # clearing comments and unnecessary symbols
  rhs.cleared<-clean.comments(rhs)
  iv.cleared<-clean.comments(iv)
  
  ### search for '^' elements
#   if (any(grepl("\\^",rhs.cleared))) 
#     { message("The ^ simbol is not supported in RHS. Replace X^Y by pow(X,Y) manually in DBSolve."); res=F}
#   if (any(grepl("\\^",iv.cleared))) 
#   { message("The ^ simbol is not supported in Initial Values. Replace X^Y by pow(X,Y) manually in DBSolve."); res=F}

  ### search for 'for' element
  if (any(grepl("^ *if *\\(",iv.cleared))) 
  { message("The if/else construction is not supported in Initial Values."); res=F}
  
  ### search for triple '_' elements
  if (any(grepl("___",rhs.cleared)))
  { message("Triple _ simbol in RHS is prohibited."); res=F}
  prob8<-grep("___",x$iv)
  if (any(grepl("___",iv.cleared)))
  { message("Triple _ simbol in Initial Values is prohibited."); res=F}
  
  ### search lines with incorrect structure
  if (!all(grepl("^[_a-zA-Z]+[][_a-zA-Z0-9]*=.+;$",iv.cleared)))
  { message("The structure of Innitial Values lines must sutisfy the criterion: ^[_a-zA-Z]+[][_a-zA-Z0-9]*=.+;$ "); res=F}
  
  ### search and check lines initials
  compound.names<-gsub("#","", x$compound.names)                          #take compound names
  ode.dimension<-as.integer(gsub(".*#","", x$ode.dimension))              # take ode.dimension
  temp1<-gsub("=.*$","", iv.cleared)
  temp2<-iv.cleared[!duplicated(temp1, fromLast=T)]
  temp3<-temp1[!duplicated(temp1, fromLast=T)]
  temp4<-temp2[temp3 %in% compound.names[seq_len(ode.dimension)]]
  if(!all(grepl(".*=(\\+|-)?[0-9.]+(e|E)?(\\+|-)?[0-9]*;$",temp4)))
  { message("The initials must be a numbers."); res=F}
  
  return(res)
}

### function to transform data from ruSlv.raw to ruSlv
# add analysis of fit conditions
# add analysis of set events
import.slv<-function(x) #create structured list 
{
  # Function parameters:
  # x: file in ruSlv.raw format (list)
  
#   #Replace ^ to Pow in RHS
#   for (i in 1:length(x$rhs)) {
#     if (!(grepl("^\\/\\/",x$rhs[i]))) {
#       if ((grepl("\\^",x$rhs[i])) || (grepl("(sqrt)",x$rhs[i]))) {
#         e = parse(text = x$rhs[i])
#         x$rhs[i] <- deparse(replacePow(e)[[1]])
#         x$rhs[i] <- sub("([^\n])$","\\1;",x$rhs[i])
#       }
#     }
#   }
#   
#   #Replace ^ to pow in Initial Values
#   for (i in 1:length(x$iv)) {
#     if (!(grepl("^\\/\\/",x$iv[i]))) {
#       if ((grepl("\\^",x$iv[i])) || (grepl("(sqrt)",x$iv[i]))) {
#         e = parse(text = x$iv[i])
#         x$iv[i] <- deparse(replacePow(e)[[1]])
#         x$iv[i] <- sub("([^\n])$","\\1;",x$iv[i])
#       }
#     }
#   }
  
  if (!compatible.slv(x)) stop("The argument is not compatible SLV list") #checking compartibility
  
  ### rhs adaptation
  rhs<-sub("(<\\?)", "//<\\?",x$rhs)    #comment old structure      
  rhs<-gsub("\\[|\\]","___", rhs)       # substitute [] inj RHS by tripple _
  
  ### iv adaptation
  initialvalues<-sub("(<\\?)", "//<\\?",x$iv)           #comment old structure      
  initialvalues<-gsub("\\[|\\]","___", initialvalues)   # substitute [] inj RHS by tripple _
  
  ###Replacing ^'s to pow
  aux=lapply(names(x)[1:2],function(x,y) replacePowlist(y,x),y=x)
  x=aux[[1]]
  
  ### iv pre-analysis
  temp5<-clean.comments(initialvalues)  # clear comments
  iv.names<-gsub("=.*","",temp5)             # search the names in iv
  iv.values<-gsub("(.*=)|;","",temp5)            # search the values in iv
  iv.unique<-!duplicated(iv.names, fromLast =T) # choose the unique from-last names
  iv.numeric<-grepl(".*=(\\+|-)?[0-9.]+(e|E)?(\\+|-)?[0-9]*;$", temp5)      #choose the lines with numeric right sides
  
  ### compound.names analysis
  compound.number<-as.integer(gsub(".*#","", x$stoicheometry.ncols))
  compound.names<-gsub("#","", x$compound.names)                          #take compound names
  ode.dimension<-as.integer(gsub(".*#","", x$ode.dimension))              # take ode.dimension
  initial.match<-match(compound.names[seq_len(ode.dimension)], iv.names[iv.unique]) #matching names of initials in iv
  ode.initials<-structure(as.double(iv.values[iv.unique][initial.match]), .Names=compound.names[seq_len(ode.dimension)]) #un.iv[match(compound.names[seq_len(ode.dimension)],names(un.iv))]    #create vector of ODE initial values
  #ode.initials1<-structure(iv.values[iv.unique][initial.match], .Names=compound.names[seq_len(ode.dimension)])   #create vector of ODE initial values
  
  ### create functions list
  temp1<-clean.comments(rhs)
  temp2<-temp1[grepl("=.*", temp1) &! grepl("==.*", temp1)]
  temp3<-gsub("(=.*)","",temp2)
  ode.functions<-temp3[!duplicated(temp3, fromLast=T)]
  
  ### create parameter vector
  not.initials<-!(iv.names %in% compound.names[seq_len(ode.dimension)]) # choose the iv that is not the initials
  parameters.external<-structure(as.double(iv.values[not.initials & iv.numeric & iv.unique]), .Names=iv.names[not.initials & iv.numeric & iv.unique]) # create external parameters vector
  parameters.internal<-structure(temp5[(!iv.numeric|!not.initials) & iv.unique], .Names=iv.names[(!iv.numeric|!not.initials) & iv.unique]) # create external parameters vector
  parameters<-iv.names[iv.unique]
  
  ### reaction.names analysis
  reaction.number<-as.integer(gsub(".*#","", x$stoicheometry.nrows))
  reaction.names<-gsub("#","", x$reaction.names)
  
  ### stoicheometry matrix creation
  #stoicheometry<-matrix(0,nrow=reaction.number, ncol=compound.number, dimnames=list(reaction=reaction.names, compound=compound.names)) # create matrix with zero
  stoicheometry<-matrix(0,nrow=reaction.number, ncol=compound.number) # create matrix with zero
  temp<-strsplit(gsub("#",  "", x$stoicheometry), " ") # split descrition of stoicheometry
  lapply(temp, function(tmp) { tmp.num<-as.integer(tmp); stoicheometry[tmp.num[1],tmp.num[2]]<<-tmp.num[3]}) # fill the matrix
  
  ### slv.version analysis
  source.version<-gsub("\\*", "", x$slv.version)
  
  ### ode solver part
  solver.time.limit<-as.numeric(gsub(".*#", "", x$time.limit))
  
  ### fitter part
  fitter.parameters.number<-as.numeric(gsub("#| ", "", x$fit.pars.num))
  fitter.parameters.size<-as.numeric(gsub("#| ", "", x$fit.pars.size))
  fitter.parameters.settings<-data.frame(
    Parameter=gsub("#", "", x$fit.pars),
    Value=NA,
    Minimum=as.numeric(strsplit(gsub("#| $", "", x$fit.pars.min)," ")[[1]]),
    Maximum=as.numeric(strsplit(gsub("#| $", "", x$fit.pars.max), " ")[[1]]),
    InitialStep=as.numeric(strsplit(gsub("#| $", "", x$fit.pars.step), " ")[[1]]),
    ON=c(rep(1,fitter.parameters.number),rep(0,fitter.parameters.size-fitter.parameters.number)),
    Log=as.numeric(strsplit(gsub("#| $", "", x$fit.pars.log), " ")[[1]]), stringsAsFactors=F
      )
  fitter.termination.Minimumstep<-as.numeric(gsub("#| $", "", x$fitter.min.step))
  fitter.termination.MinimumFchange<-as.numeric(gsub("#| $", "", x$fitter.min.F0.change))
  fitter.termination.MaximumFcalls<-NA
  fitter.termination.MinimumFsensitivity<-as.numeric(gsub("#| $", "", x$fitter.min.F0.sensitivity))
  
  ### objective function part
  OF.settings.absolute<-gsub("#| $", "", x$fitter.OF.absolute)=="1"
  OF.settings.square<-gsub("#| $", "", x$fitter.OF.square)=="1"
  
  ### parscan part
  parscan.parameters.number<-as.numeric(gsub("#| ", "", x$scan.pars.num))
  parscan.parameters.size<-as.numeric(gsub("#| ", "", x$scan.pars.size))
  parscan.parameters.settings<-data.frame(
    Parameter=gsub("#", "", x$scan.pars),
    Minimum=as.numeric(strsplit(gsub("#| $", "", x$scan.par.min)," ")[[1]]),
    Maximum=as.numeric(strsplit(gsub("#| $", "", x$scan.par.max), " ")[[1]]),
    Divisions=as.numeric(strsplit(gsub("#| $", "", x$scan.par.dev), " ")[[1]]),
    Iterations=as.numeric(strsplit(gsub("#| $", "", x$scan.par.iter), " ")[[1]]),
    ON=c(rep(1,parscan.parameters.number),rep(0,parscan.parameters.size-parscan.parameters.number)), stringsAsFactors=F
  )
  
  ### output part
  temp1<-strsplit(gsub("#","", x$output.vars), " ")[[1]]
  temp2<-strsplit(gsub("#","", x$output.vars.on), " ")[[1]]
  output.total<-temp1[grepl(".",temp1)]
  output.on<-temp2[grepl(".",temp2)]
  
  ### dat
  dat.file<-gsub("#", "", x$dat.file)
  
  ### output preparation
  y<-list(compound.number=compound.number,
          compound.names=compound.names,
          reaction.number=reaction.number,
          reaction.names=reaction.names,
          stoicheometry=stoicheometry,
          ode.dimension=ode.dimension,
          ode.initials=ode.initials,
          ode.parameters=parameters,
          ode.parameters.external=parameters.external,
          ode.parameters.internal=parameters.internal,
          ode.functions=ode.functions,
          solver.time.limit=solver.time.limit,
          fitter.parameters.number=fitter.parameters.number,
          fitter.parameters.size=fitter.parameters.size,
          fitter.parameters.settings=fitter.parameters.settings,
          fitter.termination.Minimumstep=fitter.termination.Minimumstep,
          fitter.termination.MinimumFchange=fitter.termination.MinimumFchange,
          fitter.termination.MaximumFcalls=fitter.termination.MaximumFcalls,
          fitter.termination.MinimumFsensitivity=fitter.termination.MinimumFsensitivity,
          parscan.parameters.number=parscan.parameters.number,
          parscan.parameters.size=parscan.parameters.size,
          parscan.parameters.settings=parscan.parameters.settings,
          OF.settings.absolute=OF.settings.absolute,
          OF.settings.square=OF.settings.square,
          dat.file=dat.file,
          output.total=output.total,
          output.on=output.on,
          rhs=rhs,
          iv=initialvalues
          )
  
  attributes(y)<-c(list(class="ruSlv",
                      source.version=source.version,
                      comment=paste0("This model was imported from file ", attr(x, "filename"), ", ", attr(x,"currenttime")),
                      source.file=attr(x, "filename")
                      ),attributes(y))
  
  return(y)
}

### function to transform data from ruSlv to RCT
rct.from.slv<-function(y) #function to create rct structure from slv 
{
  # Function parameters:
  # y: file in ruSlv format (ruSlv)
  
  ### argument checking
  if(!("ruSlv" %in% class(y))) stop("argument must be in class ruSlv!")
  
  out<-"//This RCT file is created by dsb-package"
  out[2]<-paste0("//based on slv file: \"", attr(y, "source.file"), "\"")
  right.matr<-ifelse(y$stoicheometry>0, paste(y$stoicheometry, rep(y$compound.names,each=y$reaction.number), sep="*"), NA)
  left.matr<-ifelse(y$stoicheometry<0, paste(-y$stoicheometry, rep(y$compound.names,each=y$reaction.number), sep="*"), NA)
  right.vec<-apply(right.matr, 1, function(l){ll<-l[!is.na(l)]; paste0(ll, collapse=" + ")} )
  left.vec<-apply(left.matr, 1, function(l){ll<-l[!is.na(l)]; paste0(ll, collapse=" + ")} )
  out<-c(out, paste0(y$reaction.names, ":\t", left.vec, " = ", right.vec, ";"))
  
  return(out)
}

### function to read data from DAT to ruDat.raw
read.dat<-function(file)
{
  # Function parameters:
  # file: filename of experimental data in DAT format (character)
  
  dat1<-readLines(file)
  sym<-paste0(substr(dat1,1,1), collapse = "")
  
  start.exp<-gregexpr("#@", sym)[[1]]+2
  end.exp<-gregexpr("@\\$", sym)[[1]]+1
  end.cond<-c(gregexpr("(#@)", sym)[[1]][-1], nchar(sym))+1
  
  out<-lapply(1:length(start.exp), function(i){
    exper<-dat1[start.exp[i]:end.exp[i]]
    xy<-dat1[(end.exp[i]+2):(end.exp[i]+3)]
    cond<-dat1[(end.exp[i]+4):end.cond[i]]
    list(experiment=exper,options=xy,conditions=cond)
  })
  
  attr(out,"currenttime")<-Sys.time()
  attr(out,"filename")<-file
  
  return(out)
}

### function to transform data from ruDat.raw ru to ruData
import.dat<-function(dat)
{
  # Function parameters:
  # dat: filename of experimental data in ruDat.raw format (list)
  
  counter<-0 #counter of experimental sets
  
  tempfun<-function(l)
  {
    ### experiment part
    temp1<-sapply(strsplit(l$experiment,"@? +"), function(ttt) { as.numeric(ttt[-1])})
    temp2<-t(temp1)
    #temp1<-as.data.frame(strsplit(l$experiment,"@? +"))  #split experimental lines
    #temp2<-subset(t(temp1), select=-1 )   #create matrix from experimental lines 
    #temp2.1<-apply(temp2,2,as.numeric)
    temp4<-gsub("#","", l$options[2])  #take variable 2 id
    temp5<-strsplit(gsub("#","",l$options[1])," ")[[1]] #take variable 1 id, method, OF type
    
    method<-switch(as.integer(temp5[2])+1, "explicit", "ode", "implicit") #user friendly method id
    exp.out<-data.frame(temp2[,1],temp4,subset(temp2, select=c(2,3,4)), stringsAsFactors =F)  # create data frame with experiments
    names(exp.out)<-c(temp5[3], "var_id","sample", "weight","variance")
    attr(exp.out,"col.def")<-c("input", "var_id", "sample", "weight", "variance")
    attr(exp.out,"class")<-c("mod.frame", "data.frame")
    
    ### conditions part
    temp6<-strsplit(gsub("#","",l$conditions)," ") # split conditions lines
    temp7<-sapply(temp6, rbind)     #create matrix from conditions lines
    unID<-!duplicated(temp7[2,], fromLast=T)
    cond.out<-as.data.frame(t(as.numeric(temp7[1, unID]))); names(cond.out)<-temp7[2, unID]
    
    ### other part
    counter<<-counter+1; 
    error.type<-switch(as.integer(temp5[1])+1L,
                       "zero",
                       "additive T",
                       "additive F")
    
    ### local output
    lout<-list(data_id=paste0("number",counter), data=exp.out, conditions=cond.out, solver=method, error.type=error.type)
    return(lout)
  }
  
  out<-lapply(dat,tempfun)
  
  # create attributes of level 0
  attr(out, "version")<-"L1"
  attr(out, "comment")<-paste0("This dataset was imported from file \'", attr(dat,"filename"), "\', ", attr(dat,"currenttime"))
  attr(out, "class")<-c("ruData","ruList")
  
  return(out)
}

### transform from ruSlv to ruC
#to do
#add events
C.from.slv<-function(slv, file="model.c", output=slv$output.on, dbs.compatibility=FALSE)
{
  # Function parameters:
  # slv  - model in ruSlv format (ruSlv)
  # file - filename of resulted C file in ruC format (character)
  # output - names of output values (character)
  # dbs.compatibility - use DBSolve-like method of parameters updated (logical)
  
  #checking parameters
  if(!all(output %in% c("t", slv$ode.functions, slv$ode.parameters, slv$reaction.names, names(slv$ode.initials)))) stop ("All output names must be presented in the model!")
  
  concon<-file(file, "w") #open file for writing
  
  sf<-system.file(package = "dbs", "extdata", mustWork = T)
  extFun<-readLines(paste(sf, "additionalmath.c.txt", sep="/"))
  cat(extFun, file=concon, sep="\n")
  
  #header    
  cat(paste0("/* This file was generated based on
SLV file: ",attr(slv,"source.file"), "
using dbs-package,",Sys.time(),"*/
"), file=concon, sep="\n")
  
  #constants
  cat(paste0("// number of input/outputs
#define Npars ", length(slv$ode.parameters), "
#define NparsExt ", length(slv$ode.parameters.external), "
#define Nout ", length(output), "
"), file=concon, sep="\n")
  
  #defining dbsolve functions
  # to do
  
  #parameters
  cat("//external parameters", file=concon, sep="\n")
  cat(paste0("#define ", names(slv$ode.parameters.external), " parms[", seq_along(slv$ode.parameters.external)-1, "]", collapse="\n"), file=concon, sep="\n")
  cat("//internal parameters", file=concon, sep="\n")
  cat(paste0("#define ", names(slv$ode.parameters.internal), " parms[", seq_along(slv$ode.parameters.internal)+length(slv$ode.parameters.external)-1, "]"), file=concon, sep="\n")
      
  #main part
  cat("\n//===== main part =====
#include <R.h> 
//#include <additionalmath.h> 
static double parms[Npars];

// initializer !!! first - initalize, then recalculate in this version, dbsolve use another algorythm !!! 
void initmod(void (* odeparms)(int *, double *))
{", file=concon, sep="\n")
  if (dbs.compatibility) #choose the algorythm depending on dbs.compartibility
  {
    cat(slv$iv,
        "\tint N=NparsExt;
  odeparms(&N,parms);", file=concon, sep="\n")
  } else {
  cat("\tint N=NparsExt;
  odeparms(&N,parms);
  //calculation of internal",
      paste0("\t", slv$ode.parameters.internal), file=concon, sep="\n")
  }
  cat("} 

// Functions, derivatives and output variables
void derivs (int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{", file=concon, sep="\n")
  cat("\t//undefining functions and variables",paste0("\t#undef ",c(slv$ode.functions, names(slv$ode.initials))), file=concon, sep="\n")
  cat("\t//defining variables",paste0("\t#define ", names(slv$ode.initials)," y[", seq_len(slv$ode.dimension)-1, "]", collapse="\n"), file=concon, sep="\n")
  #compatibility
  cat(paste0("\t//compatibility with DBSolve designations
  #define X___0___ t[0]"), file=concon, sep="\n")
  cat(paste0("\t#define ", "X___", seq_len(slv$compound.number),"___ y[", seq_len(slv$compound.number)-1, "]", collapse="\n"), file=concon, sep="\n")
  cat(paste0("\t#define ", "V___", seq_len(slv$reaction.number),"___ ", slv$reaction.names, collapse="\n"), file=concon, sep="\n")
  
  cat("\t#define t t[0]
  //errors checking
  if (ip[0] <Nout) error(\"nout should be at least Nout\");
  //initialization", file=concon, sep="\n")
  cat(paste0("\tdouble ", paste0(slv$ode.functions, collapse=", "), ";
  //functions"), file=concon, sep="\n")
  cat(paste0("\t",slv$rhs), file=concon, sep="\n")
  cat("  //RHS", file=concon, sep="\n")
  cat(paste0("\tydot[", seq_len(slv$ode.dimension)-1, "] = F___", seq_len(slv$ode.dimension), "___;", collapse="\n"), file=concon, sep="\n")
  if(length(output)!=0)
  {
    cat("  //output", file=concon, sep="\n")
    cat(paste0("\tyout[", seq_along(output)-1, "] = ", output, ";",collapse="\n"), file=concon, sep="\n")
  }
  cat("}", file=concon, sep="\n")
  
  close(concon) #close file
}

### function to clear C-style text lines: comments, spaces, empty lines, delete two or more ;;
clean.comments<-function(input)
{
  # Function parameters:
  # input      - text in C-style with comments (character)
  
  input1<-paste(input, collapse="\n") #collapse to single line
  
  ### delete all C-style comments
  singleClose<-gregexpr("\\n",input1)[[1]]; if (singleClose[1]==-1L) singleClose<-integer()
  singleOpen<-gregexpr("//",input1)[[1]]; if (singleOpen[1]==-1L) singleOpen<-integer()
  multiOpen<-gregexpr("/\\*",input1)[[1]]; if (multiOpen[1]==-1L) multiOpen<-integer()
  multiClose<-gregexpr("\\*/",input1)[[1]]; if (multiClose[1]==-1L) multiClose<-integer()
  
  df<-data.frame(number=c(singleOpen, singleClose, multiOpen, multiClose),
             type=as.factor(c(rep("singleOpen",length(singleOpen)), rep("singleClose",length(singleClose)), rep("multiOpen",length(multiOpen)), rep("multiClose",length(multiClose)) )))
  df.sort<-df[order(df$number),]
  
  flag.local=F
  flag.global=F
  start.comm<-integer()
  stop.comm<-integer()
  for (i in seq_along(df.sort$number))
  {
    if (df.sort[i,"type"]=="singleOpen" & !flag.local & !flag.global) #
    { start.comm<-c(start.comm, df.sort[i,"number"])
      flag.local=T }
    
    if (df.sort[i,"type"]=="singleClose" & flag.local & !flag.global)
    { stop.comm<-c(stop.comm, df.sort[i,"number"]-1)
      flag.local=F }
    
    if (df.sort[i,"type"]=="multiOpen" & !flag.local & !flag.global) #
    { start.comm<-c(start.comm, df.sort[i,"number"])
      flag.global=T  }
    
    if (df.sort[i,"type"]=="multiClose" & !flag.local & flag.global) #
    { stop.comm<-c(stop.comm, df.sort[i,"number"]+1)
      flag.global=F }
  }
  if (flag.global|flag.local) stop.comm<-c(stop.comm, nchar(input1))
  
  temp0<-unlist(lapply(seq_along(start.comm), function(i) start.comm[i]:stop.comm[i])) #create sequince of commented symbols 
  input2<-strsplit(input1,"")[[1]][-temp0]  #create vector of symbols and exclude commented
  input3<-paste0(input2,collapse="")  #collapse to single line
  
  ###delete spaces and tabs
  input4<-gsub("(^\\n+)| |\\t", "", input3)
  
  ### clear spare \n
  input4.5<-gsub("\\n+;", ";", input4) #delete all \n before ;
  input4.6<-gsub(";{2,}", ";", input4.5) #delete multiple ;
  input5<-gsub(";[^\n]", ";\n", input4.6) #new line after ; if is absent
  input6<-gsub("\\n{2,}", "\n", input5)  #delete multiple \n
  input8<-gsub("\n+=\n+", "=", input6)  #delete all \n around the =
  
  #output
  input100<-strsplit(input8,"\n")[[1]] #split by the end of line 
  return(input100)
}