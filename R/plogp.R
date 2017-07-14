#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
plogp<-function(x){
  out=x*log(x)
  out[x==0]=rep(0,sum(x==0))
  if(any(x<0) ){out="error"}
  out
}