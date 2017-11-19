#########################################
# Function for generation of parameter set for DBSolve output
# for "dbs" package
# 12.10.2016-20.10.2016
# A.Kalinkin
#########################################

parsetgen<-function(cov,
                    mu,
                    transform="",
                    samples=1024
                    ) {
  # cov - variance matrix for parameters (matrix or vector)
  # mu - mean values of the parameters (numeric)
  # transform - character vector containing information of which distribution is used,default is the vector with length of samples with normal distribution(character vector)
  # samples    - number of resulted samples for each condition (integer)
  
  #Checking libraries
  if (!requireNamespace("mvtnorm", quietly=T)) stop("Install \'mvtnorm\' package!")
  #Checking arguments
  if (missing(mu)) stop("mu is missing, with no default!")
  if (!all(transform %in% c("", "log"))) stop ("transform vector may include only \"\" or \"log\" values.")
  if (length(transform)==1) transform = rep(transform[1],length.out=length(mu))
  if (length(mu)!=length(transform)) stop("length(transform) must be equal to length(mu) or one")
  if (any(mu[transform=="log"]<=0)) stop("mu values must be positive for all log marked parameters!")
  ### checking mu and cov
  if (is.vector(cov)) cov<-diag(cov)
  if (!all(attributes(cov)$dim==length(mu))) stop("number of columns and rows in covariance matrix must be the length of mu!")
  
    mu.opt<-ifelse(transform=="log",log(mu),mu)
    parset<-mvtnorm::rmvnorm(n=samples,mean=mu.opt,sigma=cov)
    colnames(parset)<-names(mu)
    parset<-data.frame(parset)
    #Back transformation
    parset[,which(transform %in% "log")] <- exp(parset[,which(transform %in% "log")])
    return(parset)
  
}

parsetgen.cond<-function(parset, cond=data.frame(), max.samples=nrow(parset), uniq.nos=FALSE){
  
  #parset - data.frame with parameters(data.frame)
  #cond - data.frame with conditions(data.frame)
  #max.samples - number of rows for condition data.frame (integer)
  #uniq.nos - number of sequence, with default it has a repeat, if TRUE nos will have unique values(boolean)
  
  # Checking arguments
  if (nrow(parset)<max.samples) stop("max.samples parameter must be equal or lower than number of samples inside parset argument")
  if (nrow(parset)>max.samples) parset<-parset[1:max.samples,] # cutting the input data.frame
  
  # calculations
  if (length(cond)==0) {   # if cond data.frame is empty
    nos<-seq_len(nrow(parset))
    parset1<-cbind(nos,parset)
  } else {
    parset1<-do.call("rbind", replicate(ncol(cond), parset, simplify=FALSE)) # repeating the parset dataframe
    cond<-cond[rep(seq_len(nrow(cond)), each=max.samples),] # condition data.frame preparation
    parset1<-cbind(parset1,cond)
    if (uniq.nos==FALSE) {
      nos<-rep(seq_len(max.samples),ncol(cond))
      parset1<-cbind(nos,parset1)
    } else {
    nos<-rep(seq_len(nrow(parset1)))
    parset1<-cbind(nos,parset1)
    }
  }
  return(parset1)
}