#' t-test for meta-analysis
#' 
#' This function conduct a t-test from mean-sd data
#' @param m1 is mean of 1st group
#' @param m2 is mean of 2nd group
#' @param s1 is sd of 1st group
#' @param s2 is sd of 2nd group
#' @param n1 is size of 1st group
#' @param n2 is size of 2nd group
#' @return The p-value of t-test
#' @keywords t-test
#' @export
#' @examples 
#' t_test_meta(10,11,4,5,20,23)
t_test_meta<-function(m1,m2,s1,s2,n1,n2){
  sd_pooled=sqrt( ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2) )
  coeff=sqrt( (1/n1)+(1/n2) )
  Tstatistic<-(m1-m2)/(sd_pooled*coeff)
  pv=2 *(1-pt(abs(Tstatistic),df=(n1+n2-2) ) )
  round(pv,digits=4)
}
