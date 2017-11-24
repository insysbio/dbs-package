#########################################
# Calculation of confidence bands (from sampling) from DBSolve output
# for dbs-package
# 28.01.2015-15.06.2015
# A.Alekseev, E.Metelkin
#########################################

calccb<-function(input,
                  x.col,
                  x.seq,
                  y.col,
                  factor.col=c(),
                  q.seq=c(0.025,0.5,0.975),
                  nos.col="nos",
                  par_calc=FALSE,
                  cpu.cores=4,
                  silent=FALSE,
                  include.nos=c(),
                  ...)
{
# Function parameters:
# input      - simulated data frame from DBSolve output (data.frame)
# x.col      - column name or index of free variable in input (integer/character)
# x.seq      - sequence of free variable values for interpolation (numeric)
# y.col      - column name or index of simulated variable in input (integer/character)
# factor.col - column name or index of condition variable in input (integer/character)
# q.seq      - sequence of probability values for quantile (numeric between 0 and 1)
# nos.col    - column name or index of sample number in input (integer)
# par_calc   - is parallel calculaton enabled? (logical)
# cpu_cores  - number of CPU cores which used for calculation (for par_calc=TRUE), (integer)
# include.nos - on the output as a named column giving you approximation values of chosing number of samples (integer)

### Function arguments checking
#if (include.optimal & !("isopt" %in% names(input))) stop("isopt column must be in data if include.optimal=T!")
#if (!("isopt" %in% names(input))) input$isopt<-0
  
inames<-names(input)
inames_seq<-1:length(input)

# Checking x.col 
#if (missing(x.col)) stop("Column x.col is missing!\n")
if (is.character(x.col))
{
  temp1<-x.col %in% inames
  if (!all(temp1)) stop(x.col[!temp1]," column(s) is not found in input data frame!\n")
}
if (is.numeric(x.col))
{
  temp1<-x.col %in% inames_seq
	if (!all(temp1)) stop(x.col[!temp1]," column(s) is not found in input data frame!\n")
  x.col<-inames[x.col]
}
# Checking y.col
#if (missing(y.col)) stop("Column y.col is missing!\n")
if (is.character(y.col))
{
  temp1<-y.col %in% inames
  if (!all(temp1)) stop(y.col[!temp1], "column(s) is not found in input data frame!\n")
}
if (is.numeric(y.col))
{
  temp1<-y.col %in% inames_seq
	if (!all(temp1)) stop(y.col[!temp1], "column(s) is not found in input data frame!\n")
  y.col<-inames[y.col]
}
# Checking factor.col
if (!is.null(factor.col))
{ 
  if (is.character(factor.col))
  {
    temp1<-factor.col %in% inames
    if (!all(temp1)) stop(factor.col[!temp1]," column(s) not found in input data frame!")
  }
	if (is.numeric(factor.col))
	{
    temp1<-factor.col %in% inames_seq
		if (!all(temp1)) stop(factor.col[!temp1]," column(s) not found in input data frame!")
		factor.col<-inames[factor.col]
	}
}
# Checking q.seq
temp1<- q.seq>0 & q.seq<1
if (!(all(temp1)))	stop("q.seq has elements, which >1, or <0 !\n")
# Checking nos.col
#if (missing(nos.col)) stop("Column nos.col is missing!\n")
if (is.character(nos.col))
{
  temp1<-nos.col %in% inames
  if (!all(temp1)) stop(nos.col[!temp1]," column(s) is not found in input data frame!\n")
}
if (is.numeric(nos.col))
{
  temp1<-nos.col %in% inames_seq
	if (!all(temp1)) stop(nos.col[!temp1]," column(s) is not found in input data frame!\n")
  nos.col<-inames[nos.col]
}
# Checking x.seq
if (missing(x.seq)) stop("argument x.seq is missing, with no default!")
xmin<-min(input[x.col])
xmax<-max(input[x.col])
temp1<- x.seq >= xmin & x.seq <= xmax
if (!all(temp1)) stop("x.seq is out of the range!")

# Checking dataset with length==1
groupDataset <- interaction(input[,c(nos.col, factor.col)], drop="T", sep = "-")
groupLength <- tapply(groupDataset, groupDataset, length)
singleLengthGroup <- as.vector(groupLength<2)
if(any(singleLengthGroup)){
  message("Check simulations")
  singleLengthNames <- names(groupLength)[singleLengthGroup]
  problemWithData <- groupDataset %in% singleLengthNames
  show(subset(input, problemWithData))
  stop("Some of dataset has less than two points and cannot be used")
}

# Start "interaction" procedure with time measurement
input$group<-if(!is.null(factor.col)) interaction(input[,factor.col], drop="T", sep = "-") else as.factor("-")

# preparation for parallel
if (par_calc)
{
  if (!requireNamespace("doParallel", quietly=T)) stop("Install \"doParallel\" package or use \"par_calc=FALSE\".")
  if (!require("foreach")) stop("Install \"foreach\" package or use \"par_calc=FALSE\".")
  # Make cluster of local CPU cores
  cl<-parallel::makeCluster(cpu.cores)
  doParallel::registerDoParallel(cl)
}

output3<-NULL # variable for output
for (k in levels(input$group)) # loop by mainFactor
{
  if (!silent) message("Starting cond ", k, " ...") 

  sum_output2<-NULL  # summary output for some mainFac
  for (var_i in y.col) ### loop on variables
  {
    if (!silent) message("Variable: ", var_i, " ...", appendLF =F)
	
    input_k_i<-subset(input, group==k, c(x.col, var_i, nos.col))  # subset with particular group and variable 
	  time0 <- proc.time() # Time measurement
	  # Is parallel calculation enabled?
	 if (par_calc) # parallel calulation is used
	  {
		  mat_i<-foreach::foreach(i=unique(input_k_i[,3]) , .combine=cbind)%dopar%
		  {
			  mat_i.inter<-stats::approxfun(input_k_i[input_k_i[,3]==i,1], input_k_i[input_k_i[,3]==i,2])
			  mat_i.inter(x.seq)
		  } # foreach by iters
	  }	else	{  # parallel calulation is not used
		  mat_i<-NULL
      nos_col1<-NULL
      #split-apply-combine paradigma used
      input_k_i1=split(input_k_i,input_k_i$nos) #split by number of simulations
      mat_i.inter=sapply(input_k_i1, stats::approxfun) #applying approximation
      mat_i=sapply(mat_i.inter,function(f) f(x.seq)) #combine into matrix
	  }
   #search the column with isopt=1
	 #t1<-unique(input_k_i[,3:4])[,2]==1
  
	output2<-data.frame(x.seq, var_i)   																# create output 
	names(output2) <- c(x.col, "var_id") # 
  output2[,paste0("quant_",q.seq)]<-t(apply(mat_i, MARGIN=1, FUN=stats::quantile, probs = q.seq, ...)) #append quantile
  # append optimal here
  #if (include.optimal) output2[,"simulation"]<-mat_i[,t1]

  sum_output2<-rbind(sum_output2, output2) 									# add output2 to summary output (sum_output2)
	
  time3<-proc.time()-time0 																	# Calculation timer
	if (!silent) message("Calc time, sec: ", round(time3[3],2)) 
  
  } # loop on variables
  
  if (!is.null(factor.col))  sum_output2[,factor.col]<-subset(input, subset=group==k, select=factor.col)[1,]  	# Add "factor" columns
  sum_output2$group<-k																		# Add "group" column
  
  output3<-rbind(output3,sum_output2)
} # loop by mainFactor


#if include.nos option have a number of sequencing inside it
if (!is.null(include.nos)) {
  message("Minimum number of sample:", min(input$nos))
  message("Maximum number of sample:", max(input$nos))
  if (!all((include.nos %in% input$nos))) stop("Check maximum and minimum number of sequencing and try again!")
  for (ce in include.nos) {
    incl.nos<-c()
    for (c0 in factor(y.col)){
      inter_fun<-NULL
      temp=input$t[input$nos==ce]
      temp2=input[[c0]][input$nos==ce]
      inter_fun=stats::approxfun(temp,temp2)
    #colnames(result_nos)<-paste0("approx_nos_",ce)
      incl.nos=c(incl.nos,inter_fun(x.seq))
  }
    incl.nos=data.frame(incl.nos)
    incl.nos$group=unique(input$group[input$nos==ce])
    output3[,paste0("approx_nos_",ce)]=ifelse(output3$group==incl.nos$group,incl.nos[,1],NA)
}
}


if (par_calc) { parallel::stopCluster(cl) } # stop cluster (in case of parallel calculations)

# modifying the object
#output3$var_id = as.character(output3$var_id)
#output3$group = as.factor(output3$group)
  #class(output3)<-c("mod.frame","data.frame")
  #attr(output3,"col.def")<-if (include.optimal)
      #c("input", "var_id", rep("quantile",length(q.seq)), "simulation" ,rep("input",length(factor.col)), "group")
    #else
      #c("input", "var_id", rep("quantile",length(q.seq)), rep("input",length(factor.col)), "group")
#attr(output3,"col.title")<-names(output3)
#attr(output3,"var.title") <-levels(as.factor(output3$var_id))
#attr(output3,"group.title") <-levels(as.factor(output3$group))

return(output3) }

