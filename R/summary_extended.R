#' Summaries functions
#' 
#' These functions provide different summary statistics of data
#' @param x is a numeric vector whose statistics are to be calculated
#' @return The summary of \code{x}
#' @keywords summary
#' @export
#' @examples 
#' summary_extended(x=rnorm(1000))
summary_extended<-function(x){
  if (!is.numeric(x)){ error('not a numeric vector')}
  
  tempsummary=summary(x)
  out<-c(tempsummary,sd(x,na.rm=TRUE),quantile(x,probs = c(0.01,0.05,0.95,0.99)))
  names(out)<-c(names(tempsummary),"sd","q0.01","q0.05","q0.95","q0.99")
  
  out
}


#' @rdname summary_extended
summary_type2<-function(x){
  if (!is.numeric(x)){ error('not a numeric vector')}
  
  tempsummary=c(length(x),mean(x,na.rm=TRUE),sd(x,na.rm=TRUE),quantile(x,probs = c(0.05,0.95),na.rm=TRUE),
                quantile(x,probs = c(0.1),na.rm=TRUE),(mean(x,na.rm=TRUE)-2*sd(x,na.rm=TRUE)))
  out<-c(tempsummary)
  names(out)<-c("N","Mean","SD","Quantile5%","Quantile95%","Wart.ref.Q10","Wart.ref.2SD")
  
  round(out,digits=2)
}


#' @rdname summary_extended
summary_type2_1<-function(x){
  if (!is.numeric(x)){ error('not a numeric vector')}
  
  tempsummary=c(length(x),mean(x,na.rm=TRUE),sd(x,na.rm=TRUE),quantile(x,probs = c(0.05,0.1),na.rm=TRUE),
                quantile(x,probs = c(0.5),na.rm=TRUE))
  out<-c(tempsummary)
  names(out)<-c("N","Mean","SD","Quantile5%","Quantile10%","Mediana")
  
  round(out,digits=2)
}


#' @rdname summary_extended
summary_type3<-function(x){
  if (!is.numeric(x)){ error('not a numeric vector')}
  
  tempsummary=c(length(x),mean(x,na.rm=TRUE),sd(x,na.rm=TRUE),quantile(x,probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99),na.rm=TRUE))
  out<-c(tempsummary)
  names(out)<-c("N","Mean","SD","Q1%","Q5%","Q10%","Q25%","Q50%","Q75%"
                ,"Q90%","Q95%","Q99%")
  
  round(out,digits=1)
}
