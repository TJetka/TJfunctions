#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
divide_to_wide<-function(data,measure_var,signal_var,type_var,seed=123){
  
  set.seed(seed)
  
  outFinal=do.call(rbind,by(data,data[[signal_var]],function(x){
    tmp_types=as.character(x[[type_var]])
    #min_index=which.min(table(tmp_types) )
    min_value=min( table( tmp_types ) )
    ids_types=by(1:length( tmp_types ), tmp_types , function(y) sample(y,min_value,replace=FALSE))
    out<-do.call(cbind,lapply(ids_types,function(y){
      if(length(measure_var)==1){
        x[[measure_var]][y]
      } else {
        x[,measure_var][y,]
      }
    }))
    #print(x[[signal_var]][1])
    if(length(measure_var)==1){
      colnames(out)<-paste(measure_var,".",sort(unique(tmp_types)),sep="")
    } else {
      colnames(out)<-do.call(c,lapply(sort(unique(tmp_types)),function(y){
        paste(measure_var,".",y,sep="")
      }))
    }
    out<-data.frame(out,signal=rep(as.character(x[[signal_var]][1]),nrow(out)))
    out
  })
  )
  
}