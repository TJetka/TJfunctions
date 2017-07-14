#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
subset_inv_row<-function(vec,vec_out){
  if (length(vec_out)==0) {out=vec}
  
  if (length(vec_out)>0)  {
    if (is.null(dim(vec))){
      out=vec[-vec_out]
    } else {
      out=matrix(vec[-vec_out,],ncol=ncol(vec))
    }
  }
  
  out
}