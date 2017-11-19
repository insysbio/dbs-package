#########################################
# Function for creating the object with
# cov: covariance matrix
# optim: optimation values
# transform: transformation ""/"log"
# for "dbs" package
# 21.10.2016-21.10.2016 A.Kalinkin
# 2016-10-23 E. Metelkin
# rename from hessian2cov to hessian2stat
# 2016-10-31 E.Metelkin
#########################################

hessian2stat<-function(hessian, optim, transform="", level=0.95, ...) {
  
  # hessian: hessian matrix for parameter set VS -2log(L)
  # optim: optimation values for parameters. Used only for "log"
  # transform: string vector, containing the markers of distribution: "" or "log"
  # level: confidence level (numeric between 0 and 1)
  # ...: other parameters passed to base::solve
  
  # Checking arguments
  if (missing(hessian)) stop("hessian is missing with no default!")
  if (!all(transform %in% c("", "log"))) stop ("transform vector may include only \"\" or \"log\" values.")
  if (any(transform!="") & missing(optim)) stop("optim is missing which required for transformed space!")
  if (length(transform)==1) transform <- rep(transform[1],length.out=length(optim))
  if (length(optim)!=length(transform)) stop("length(transform) must be equal to length(optim) or one")
  if (any(eigen(hessian)$values<=0)) stop("hessian matrix must be positive-definite!")
  if (!(is.matrix(hessian))) hessian<-as.matrix(hessian) #if readed hessian file is data.frame
  if (!all(attributes(hessian)$dim==length(optim))) stop("Size of optim does not match the covariance matrix!")
  
  # Caclucations and set names for matrix
  transform1<-ifelse(transform=="log", optim, 1)
  vmat0<-base::solve(diag(transform1) %*% (0.5*hessian) %*% diag(transform1), ...) # covariance matrix
  colnames(vmat0)<-names(optim)
  
  # set names for transformation
  names(transform)<-names(optim)
  
  # create correlation matrix
  cor<-cov2cor(vmat0)
  
  # calculation of sigma diagonal
  sigma<-sqrt(diag(vmat0))
  names(sigma)<-names(optim)
  
  # calculation of confidence intervals
  err<-qnorm(0.5+level/2)
  left<-ifelse(transform=="log", optim*exp(-err*sigma), optim-err*sigma)
  right<-ifelse(transform=="log", optim*exp(err*sigma), optim+err*sigma)
  ci=matrix(c(left,right), nrow=2, byrow=T, dimnames=list(paste0(100*c((1-level)/2, (1+level)/2),"%"), names(optim)))
    
  res<-list(muCov=vmat0, mu=optim, transform=transform, muCor=cor, se=sigma, level=level, ci=ci)
  #class(res)<-"dbs-statistics"
    
  return(res)
}
