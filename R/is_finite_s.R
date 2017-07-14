#' Statistics of NAs
#' 
#' These functions calculate statistics of the number of 
#' finite and NAs in a given numerical vector
#' @param x is a numerical vector
#' @keywords data.frame
#' @export
#' @examples 
#' 
is_finite_s<-function(x){
  temp=is.finite(x)
  c(sum(temp),mean(temp))
}

#' @rdname is_finite_s
is_na_s<-function(x){
  temp=is.na(x)
  c(sum(temp),mean(temp))
}