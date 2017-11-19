#########################################
# Function for calculation of CI and other statistics
# for "dbs" package
# 27.03.2015-21.10.2016
# E.Metelkin, A. Kalinkin
#########################################

parset2stat<-function(parset,
                    transform="",
                    level=0.95,
                    norm.test=shapiro.test,
                    ...)
{
  # parset:  data.frame with parameter set (data.frame)
  # transform: string vector with distribution marker(vector)
  # level: confidence level (numeric between 0 and 1)
  # norm.test: function to check normality (function)
  # ... :  other arguments passed to quantile()
  
  if (!all(transform %in% c("", "log"))) stop ("transform vector may include only \"\" or \"log\" values.")
  if (length(transform)==1) transform = rep(transform[1],length.out=length(parset))
  if (length(parset)!=length(transform)) stop("length(transform) must be equal to length(parset) or one")
  if (any(transform=="log"))
    if (any(parset[,transform=="log"]<=0)) stop("parset values must be positive for all log marked parameters!")
  
  ### calculations
  ci<-sapply(parset, stats::quantile, probs=c((1-level)/2,(1+level)/2),...)
  median<-sapply(parset, stats::median)
    
  ### logarythmic column if needed
  parset[,which(transform %in% "log")] <- log(parset[,which(transform %in% "log")])
  
  ### variance
  cov<-stats::var(parset)
  
  # create correlation matrix
  cor<-cov2cor(cov)
  
  # set names for transformation
  names(transform)<-names(parset)
  
  ### normality test
  if (!is.null(norm.test))
  {
    normality<-sapply(parset, norm.test)
  } else {
    normality<-NULL
  }
 
  # calculation of population mean
  mean<-sapply(parset, mean)
  mean[transform=="log"]<-exp(mean[transform=="log"])
  
  # calculation of sigma diagonal
  sigma<-sqrt(diag(cov))
  names(sigma)<-names(parset)
  
  res<-list(muCov=cov, mu=mean, transform=transform, muCor=cor, se=sigma, level=level, ci=ci, median=median, normality=normality)
  
  return(res)
}
