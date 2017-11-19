#########################################
# Functions for plotting the values lower and upper than the limits and other auxilary functions
# for "dbs" package
# 03.04.2015-24.04.2015
# A.Alekseev, E.Metelkin
#########################################

nan.graphics<-new.env()

nan.plot<-function(x,y, xlim=range(x, finite = TRUE), ylim=range(y, finite = TRUE),
                   log=c("", "x", "y", "xy"), ..., 
                   force.bound=FALSE, delta=0.1)
{
  # Function parameters:
  #x    - vector to plot on x-axe (numeric)
  #y    - vector to plot on y-axe (numeric)
  #xlim - main limits of x-axe (numeric)
  #ylim - main limits of x-axe (numeric)
  #log  - logarythmic plot (character)
  #...  - other arguments passed to plot.default
  #force.bound  - always plot outer space (logical)
  #delta  - ralative size of outer space (numeric >0)
  
  log<-match.arg(log)
  
  if (!any(class(x)=="numeric")) stop("argument x is not numeric!")
  if (!any(class(y)=="numeric")) stop("argument y is not numeric!")
  if ((log %in% c("x","xy")) & any(is.infinite(log(xlim))))
  {
    xlim<-signup(exp(range(log(x), finite = T)),2)
    message("x-range has been updated because of x-log scale.")
  }
  if ((log %in% c("y","xy")) & any(is.infinite(log(ylim))))
  {
    ylim<-signup(exp(range(log(y), finite = T)),2)
    message("y-range has been updated because of y-log scale.")
  }
  
  ### calculate geometric limits of plot
  ylim1<-if(log %in% c("y","xy"))
    ylim*exp(delta*diff(log(ylim))*c(-1,1))
  else
    ylim+delta*diff(ylim)*c(-1,1)
  
  xlim1<-if(log %in% c("x","xy"))
    xlim*exp(delta*diff(log(xlim))*c(-1,1))
  else
    xlim+delta*diff(xlim)*c(-1,1)
  
    
  # equations for non-log case
  y_up <- ylim[2]+delta*diff(ylim)*(1-exp((ylim[2]-y)/diff(ylim)))/(1+exp((ylim[2]-y)/diff(ylim)))
  y_down <- ylim[1]-delta*diff(ylim)*(1-exp((y-ylim[1])/diff(ylim)))/(1+exp((y-ylim[1])/diff(ylim)))
  
  x_up <-   xlim[2]+delta*diff(xlim)*(1-exp((xlim[2]-x)/diff(xlim)))/(1+exp((xlim[2]-x)/diff(xlim)))
  x_down <- xlim[1]-delta*diff(xlim)*(1-exp((x-xlim[1])/diff(xlim)))/(1+exp((x-xlim[1])/diff(xlim)))
  
  if(log %in% c("y","xy")) # check and correct here
  {
    y_up <-   ylim[2]*exp(delta*diff(log(ylim))*(1-exp((log(ylim[2])-log(y))/diff(log(ylim))))/(1+exp((log(ylim[2])-log(y))/diff(log(ylim)))))
    y_down  <- ylim[1]/exp(delta*diff(log(ylim))*(1-exp((log(y)-log(ylim[1]))/diff(log(ylim))))/(1+exp((log(y)-log(ylim[1]))/diff(log(ylim)))))
  }
 
  if(log %in% c("x","xy")) # check and correct here
  {
    x_up <-   xlim[2]*exp(delta*diff(log(xlim))*(1-exp((log(xlim[2])-log(x))/diff(log(xlim))))/(1+exp((log(xlim[2])-log(x))/diff(log(xlim)))))
    x_down  <- xlim[1]/exp(delta*diff(log(xlim))*(1-exp((log(x)-log(xlim[1]))/diff(log(xlim))))/(1+exp((log(x)-log(xlim[1]))/diff(log(xlim)))))
    
  }
  
  yl<-y<ylim[1]
  yu<-y>ylim[2]
  y_calc<-y
  y_calc[yl]<-y_down[yl]
  y_calc[yu]<-y_up[yu]
  
  xl<-x<xlim[1]
  xu<-x>xlim[2]
  x_calc<-x
  x_calc[xl]<-x_down[xl]
  x_calc[xu]<-x_up[xu]

  ### plot
  plot.default(x=x_calc, y=y_calc, axes=F, xlim=xlim1, ylim=ylim1, log=log, ...)  
  
  #non-log
  lr_title_cord<-c(((xlim1[1])+xlim[1])/2, sum(ylim)/2)
  lowr_title_cord<-c(sum(xlim1)/2, (ylim1[1]+ylim[1])/2)
  upr_title_cord<-c(sum(xlim1)/2, (ylim1[2]+ylim[2])/2)
  rr_title_cord<-c((xlim1[2]+xlim[2])/2, sum(ylim)/2)
  
  if(log =="x") 
  {
    lr_title_cord<-c(exp((log(xlim1[1])+log(xlim[1]))/2), sum(ylim)/2)
    lowr_title_cord<-c(exp(sum(log(xlim1)/2)), (ylim1[1]+ylim[1])/2)
    upr_title_cord<-c(exp(sum(log(xlim1)/2)), (ylim1[2]+ylim[2])/2)
    rr_title_cord<-c(exp((log(xlim1[2])+log(xlim[2]))/2), sum(ylim)/2)
  }
  
  if(log =="y") 
  {
    lr_title_cord<-c(((xlim1[1])+xlim[1])/2, exp(sum(log(ylim)/2)))
    lowr_title_cord<-c(sum(xlim1)/2, exp((log(ylim1[1])+log(ylim[1]))/2))
    upr_title_cord<-c(sum(xlim1)/2, exp((log(ylim1[2])+log(ylim[2]))/2))
    rr_title_cord<-c((xlim1[2]+xlim[2])/2, exp(sum(log(ylim)/2)))
  }
  
  if(log =="xy") 
  {
    lr_title_cord<-c(exp((log(xlim1[1])+log(xlim[1]))/2), exp(sum(log(ylim)/2)))
    lowr_title_cord<-c(exp(sum(log(xlim1)/2)), exp((log(ylim1[1])+log(ylim[1]))/2))
    upr_title_cord<-c(exp(sum(log(xlim1)/2)), exp((log(ylim1[2])+log(ylim[2]))/2))
    rr_title_cord<-c(exp((log(xlim1[2])+log(xlim[2]))/2), exp(sum(log(ylim)/2)))
  }
  
  
  if (any(yl, na.rm=T) |force.bound) #plot lower rectangle
  {
    rect(xlim1[1], ylim1[1], xlim1[2], ylim[1], col="#00000033", border=NA)
    text(lowr_title_cord[1],lowr_title_cord[2], paste("<", round(ylim[1])))
  }
  if (any(yu, na.rm=T)|force.bound) #plot upper rectangle
  {
       
    rect(xlim1[1], ylim[2], xlim1[2], ylim1[2], col="#00000033", border=NA)
    text(upr_title_cord[1],upr_title_cord[2], paste(">", round(ylim[2])))
  }
  if (any(xl)|force.bound) # plot left rectangle
  {
    rect(xlim1[1], ylim1[1], xlim[1], ylim1[2], col="#00000033", border=NA)
    text(lr_title_cord[1],lr_title_cord[2],  paste("<", xlim[1]), srt=90)
  }
  if (any(xu)|force.bound) # plot right rectangle
  {
    rect(xlim[2], ylim1[1], xlim1[2], ylim1[2], col="#00000033", border=NA)
    text(rr_title_cord[1],rr_title_cord[2], paste(">", xlim[2]), srt=90)
  }
  
  ### add axis
  ax1<-axTicks(1)
  axis(side = 1,pos=ylim1[1], at=ax1[ax1>=xlim[1] & ax1<=xlim[2]])
  
  ax2<-axTicks(2)
  axis(side = 2, pos=xlim1[1], at=ax2[ax2>=ylim[1] & ax2<=ylim[2]])
  
  ### return invisible limits and borders
  par<-list(xlim=xlim,
                 ylim=ylim,
                 delta=delta,
                 xlim1=xlim1,
                 ylim1=ylim1,
                 borders=c(any(yl), any(xl), any(yu), any(xu))|force.bound,
                 lr_title_cord=lr_title_cord,
                 lowr_title_cord=lowr_title_cord,
                 upr_title_cord=upr_title_cord,
                 rr_title_cord=rr_title_cord,
                 log=log)
  assign("nan.par", par, envir=nan.graphics)
}

nan.points<-function(x,y,...)
{
  # Function parameters:
  #x    - vector to plot on x-axe (numeric)
  #y    - vector to plot on x-axe (numeric)
  #...  - other arguments passed to points.default
  
  ### checking parameters
  if (is.null(nan.graphics$nan.par)) stop ("nan.plot is not created!")
  if (!any(class(x)=="numeric")) stop("argument x is not numeric!")
  if (!any(class(y)=="numeric")) stop("argument y is not numeric!")
  
with(nan.graphics$nan.par,{  # start env here
  
  ### tranformation of values
  if(log %in% c("y","xy")) 
  {
    y_up <-   ylim[2]*exp(delta*diff(log(ylim))*(1-exp((log(ylim[2])-log(y))/diff(log(ylim))))/(1+exp((log(ylim[2])-log(y))/diff(log(ylim)))))
    y_down  <- ylim[1]/exp(delta*diff(log(ylim))*(1-exp((log(y)-log(ylim[1]))/diff(log(ylim))))/(1+exp((log(y)-log(ylim[1]))/diff(log(ylim)))))
  } else {
    y_up <- ylim[2]+delta*diff(ylim)*(1-exp((ylim[2]-y)/diff(ylim)))/(1+exp((ylim[2]-y)/diff(ylim)))
    y_down <- ylim[1]-delta*diff(ylim)*(1-exp((y-ylim[1])/diff(ylim)))/(1+exp((y-ylim[1])/diff(ylim)))
  }
  
  if(log %in% c("x","xy"))
  {
    x_up <-   xlim[2]*exp(delta*diff(log(xlim))*(1-exp((log(xlim[2])-log(x))/diff(log(xlim))))/(1+exp((log(xlim[2])-log(x))/diff(log(xlim)))))
    x_down  <- xlim[1]/exp(delta*diff(log(xlim))*(1-exp((log(x)-log(xlim[1]))/diff(log(xlim))))/(1+exp((log(x)-log(xlim[1]))/diff(log(xlim)))))
    
  } else {
    x_up <-   xlim[2]+delta*diff(xlim)*(1-exp((xlim[2]-x)/diff(xlim)))/(1+exp((xlim[2]-x)/diff(xlim)))
    x_down <- xlim[1]-delta*diff(xlim)*(1-exp((x-xlim[1])/diff(xlim)))/(1+exp((x-xlim[1])/diff(xlim)))
  }
  
  yl<-y<ylim[1]
  yu<-y>ylim[2]
  y_calc<-y
  y_calc[yl]<-y_down[yl]
  y_calc[yu]<-y_up[yu]
  
  xl<-x<xlim[1]
  xu<-x>xlim[2]
  x_calc<-x
  x_calc[xl]<-x_down[xl]
  x_calc[xu]<-x_up[xu]
  
  ### plot
  points.default(x=x_calc, y=y_calc, ...)
  
  borders.upd<-borders
  ### plot rectangles
    if (any(yl, na.rm=T) & !borders[1]) #plot lower rectangle
    {
      rect(xlim1[1], ylim1[1], xlim1[2], ylim[1], col="#00000033", border=NA)
      text(lowr_title_cord[1],lowr_title_cord[2], paste("<", round(ylim[1])))
      #nan.par$borders[1]<-T
      borders.upd[1]<-T
    }
    if (any(yu, na.rm=T) & !borders[3]) #plot upper rectangle
    {
      rect(xlim1[1], ylim[2], xlim1[2], ylim1[2], col="#00000033", border=NA)
      text(upr_title_cord[1],upr_title_cord[2], paste(">", round(ylim[2])))
      #nan.par$borders[3]<-T
      borders.upd[3]<-T
    }
    if (any(xl) & !borders[2]) # plot left rectangle
    {
      rect(xlim1[1], ylim1[1], xlim[1], ylim1[2], col="#00000033", border=NA)
      text(lr_title_cord[1],lr_title_cord[2],  paste("<", xlim[1]), srt=90)
      #nan.par$borders[2]<-T
      borders.upd[2]<-T
    }
    if (any(xu) & !borders[4]) # plot right rectangle
    {
      rect(xlim[2], ylim1[1], xlim1[2], ylim1[2], col="#00000033", border=NA)
      text(rr_title_cord[1],rr_title_cord[2], paste(">", xlim[2]), srt=90)
      #nan.par$borders[4]<-T
      borders.upd[4]<-T
    }
  
  #update nan.par here
  nan.par.upd<-nan.graphics$nan.par
  nan.par.upd$borders<-borders.upd
  assign("nan.par", nan.par.upd, envir=nan.graphics)
  }) # stop env here
}