#' Removing NAs observations from data frame
#' 
#' @param data is a data.frame object
#' @keywords data.frame
#' @export
#' @examples 
#' 
deleteNA_df<-function(data){
  data[apply(data,1,function(x) !any(is.na(x)) ),]
  }