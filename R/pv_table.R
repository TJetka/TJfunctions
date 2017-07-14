#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
pv_table<- function(data){
  rownum=nrow(data)
  colnum=ncol(data)
  dataout=data.frame(data[,1])
  
  for (i in 2:colnum){
    tempPvCol=c()
    dataout=cbind(dataout,data[,i])
    for (j in 1:(rownum-1) ){
      mean1=as.numeric(str_match(data[j,1],"[0-9.]+"))
      sd1=as.numeric(str_match(str_match(data[j,1],"[(][0-9.]+[)]"),"[0-9.]+"))
      n1=as.numeric(as.character(data[rownum,1]))
      
      mean2=as.numeric(str_match(data[j,i],"[0-9.]+"))
      sd2=as.numeric(str_match(str_match(data[j,i],"[(][0-9.]+[)]"),"[0-9.]+"))
      n2=as.numeric(as.character(data[rownum,i]))
      
      if (!any(is.na(c(mean1,mean2,sd1,sd2,n1,n2)))){
        tempPvCol[j]=t.test_meta(mean1,mean2,sd1,sd2,n1,n2)
      } else {
        tempPvCol[j]=NA
      }
    }
    tempPvCol[rownum]=NA
    dataout=cbind(dataout,pv=tempPvCol)
  }
  
  colnames(dataout)<-c(colnames(data)[1],paste(dename(t(replicate(2,colnames(data)[2:colnum] ))),c("","_pv"),sep=""))
  row.names(dataout)<-row.names(data)
  dataout
}