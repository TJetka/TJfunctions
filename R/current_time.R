#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
current_time=function(){
  format(Sys.time(),"%Y%m%d_%H%M")
}