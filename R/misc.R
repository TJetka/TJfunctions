#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
simulateSSA <- function(par,nu,x0,a,f_btl,tf){
  names(par) <- paste('p',1:parn,sep ="")
  ssa.res <-
    ssa(x0 = x0,    # initial state vector 
        a = a,        # propensity vector
        nu = nu ,     # state-change matrix
        parms = par,  # model parameters
        tf = tf,
        method = "BTL",f=f_btl)
  #ssa.plot(ssa.res)
  ## Wybierac tylko czesc danych
  return(ssa.res)
}

#' @rdname simulateSSA
plot_traj<-function(data,tf,t_dens,path){
  
  nsamp=length(data)
  nplots<-ncol(data[[1]])-1
  gplots<-list()
  timesOut<-seq(from=0,to=tf,length=t_dens)
  
  dataInt<-lapply(data, function(x){
    timesIn<-x[,1]
    outInt<-cbind(timesOut,apply(x[,2:ncol(x)],2,function(y){
      approx(x=timesIn,y=y,xout=timesOut,method="linear")$y
    }))
  })
  
  for (i in 1:nplots){
    
    data_plot<-melt(lapply(data,function(x) {
      out=data.frame(times=x[,1],variable=x[,(i+1)])
    }),id.vars=c("times"))
    
    gplots[[i]]<-ggplot(data=data_plot,aes(x=times,y=value,group=factor(L1) ))+geom_line()+ggtitle(paste('Variable - ',i,sep="") )
    
  }
  
  outF<-list()
  
  pdf(file=path)
  multiplot(gplots,cols=2)
  dev.off()
  
  outF$data<-dataInt
  outF
}