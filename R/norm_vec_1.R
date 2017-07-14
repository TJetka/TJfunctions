#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
norm_vec_1<-function(x){
  sum(abs(x))
}

#' @rdname norm_vec_1
norm_vec_2<-function(x){
  sqrt(sum(x^2))
}