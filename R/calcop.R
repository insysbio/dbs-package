#########################################
# interpolation simulations from DBSolve output
# for dbs-package
# 08.016.2015
# E.Metelkin
#########################################

calcop<-function(input,
                  x.col,
                  x.seq,
                  y.col,
                  factor.col=c()
                  )
{
# Function parameters:
# input      - simulated data frame from DBSolve output (data.frame)
# x.col      - column name or index of free variable in input (integer/character)
# x.seq      - sequence of free variable values for interpolation (numeric)
# y.col      - column name or index of simulated variable in input (integer/character)
# factor.col - column name or index of condition variable in input (integer/character)

### Function arguments checking
inames<-names(input)
inames_seq<-1:length(input)

# Checking x.col 
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
# Checking x.seq
if (missing(x.seq)) stop("argument x.seq is missing, with no default!")
xmin<-min(input[x.col])
xmax<-max(input[x.col])
temp1<- x.seq >= xmin & x.seq <= xmax
if (!all(temp1)) stop("x.seq is out of the range!")

# Start "interaction" procedure with time measurement
input$group<-if(!is.null(factor.col)) interaction(input[,factor.col], drop="T", sep = "-") else as.factor("-")

output3<-NULL # variable for output
for (k in levels(input$group)) # loop by mainFactor
{

  sum_output2<-NULL  # summary output for some group
  for (var_i in y.col) ### looking on variables
  {
	
    input_k_i<-subset(input, group==k, c(x.col, var_i))  # subset with particular group and variable 
	  
    inter<-stats::approxfun(input_k_i[,1], input_k_i[,2])
  
	  output2<-data.frame(x.seq, var_i, inter(x.seq))   																# create output 
	  names(output2) <- c(x.col, "var_id", "simulation") # 
    
    sum_output2<-rbind(sum_output2, output2) 									# add output2 to summary output (sum_output2)
  
  } # loop on variables
  
  if (!is.null(factor.col))  sum_output2[,factor.col]<-subset(input, subset=group==k, select=factor.col)[1,]  	# Add "factor" columns
  sum_output2$group<-k																		# Add "group" column
  
  output3<-rbind(output3,sum_output2)
} # loop by mainFactor

# modifying the object
output3$var_id = as.character(output3$var_id)
#output3$group = as.factor(output3$group)

  class(output3)<-c("mod.frame","data.frame")
  attr(output3,"col.def")<-c("input", "var_id", "simulation" ,rep("input",length(factor.col)), "group")
  attr(output3,"col.title")<-names(output3)
  attr(output3,"var.title") <-levels(as.factor(output3$var_id))
  attr(output3,"group.title") <-levels(as.factor(output3$group))

return(output3)

}