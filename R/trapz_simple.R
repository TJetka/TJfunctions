#' Numerical intergration by trapezoidal rule
#' 
#' This function calculate the integral of function
#' @param x is a numerical vector
#' @param y is a numerical vector
#' @keywords numerical integration
#' @export
#' @examples 
#' trapz_simple(x=1:10,y=exp(1:10))
trapz_simple<-function(x,y){
  nx=length(x)
  
  xmesh=x[(2:nx)]-x[(1:(nx-1))]
  ymesh=0.5*(y[(2:nx)]+y[(1:(nx-1))])
  
  sum(xmesh*ymesh)
}