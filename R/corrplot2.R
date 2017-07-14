#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
corrplot2<-function(corrM){
  idna=is.na(corrM)
  pM=1*idna
  corrM[idna]<-0
  if( all(corrM==0)) {
    corrplot::corrplot.mixed(corrM,p.mat=pM)
  } else {
    corrplot::corrplot.mixed(corrM,p.mat=pM,is.corr = FALSE)
  }
}