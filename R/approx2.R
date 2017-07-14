#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
approx2<-function(x,y,xout,type){
  yout=Hmisc::approxExtrap(x=x,y=y, method='linear', xout=xout,na.rm=TRUE)$y
  x1=head(x[!is.na(y)],1)
  xn=tail(x[!is.na(y)],1)
  xout_1=(1:length(xout))[xout<x1]
  xout_n=(1:length(xout))[xout>xn]
  if (x1<=0) {yout[xout_1]<-yout[tail(xout_1,1)+1]}
  if (x1>0)  {yout[xout_1]<-NA}
  #if (type==1) {yout[xout_n]<-yout[head(xout_n,1)-1]}
  if (type==2) {yout[xout_n]<-NA}
  yout
}

#' @rdname approx2
approx3<-function(x,y,xout){
  yout=Hmisc::approxExtrap(x=x,y=y, method='linear', xout=xout,na.rm=TRUE)$y
  x1=head(x[!is.na(y)],1)
  xn=tail(x[!is.na(y)],1)
  xout_na=(1:length(xout))[xout<x1|xout>xn]
  yout[xout_na]=mean(y,na.rm=TRUE)
  yout
}

#' @rdname approx2
approx_InvQuantile<-function(x,y,yout,xmin,xmax){
  yout=Hmisc::approxExtrap(x=c(0,y,1),y=c(xmin,x,xmax), method='linear', xout=yout,na.rm=TRUE)$y
  yout
}