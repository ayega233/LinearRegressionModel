#' @import ggplot2
#' @export linreg

library(ggplot2)

linreg <- function(formula,data){
  reg <- structure(list(coefficient=list()),class="linreg")

  x<-model.matrix(formula,data)
  y<-data[,all.vars(formula)[1]]
  
  reg$formula <-formula
  reg$data <-data
  reg$param <-deparse(substitute(data))
  reg$x <-x
  reg$y <-y
  
  #Q<-qr.rq(x)
  #R<-qr.R(x)

  beta<- (solve(t(x)%*%x)%*%t(x))%*%y
  reg$beta <-beta
  
  reg$filted_v <- x%*%reg$beta
  reg$resi_v <- y - reg$filted_v

  return(reg)
}

#' @export print.linreg
#' @export
print.linreg <-function(reg){
  coefficient <-t(reg$beta)
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat("Coefficients:\n")
  print(coefficient,row.names = FALSE)
}

#options(repr.plot.width = 2, repr.plot.height =3)
#' @export plot.linreg
#' @export
plot.linreg <-function(reg){

  plot_data <- data.frame(list(
    Residuals=reg$resi_v,
    Fitted=reg$filted_v
  ))

  
  
  p <- ggplot2::ggplot(data=plot_data,(mapping = aes(x = Fitted,y = Residuals )))+ylim(-1.5,1.5)
  #+theme(plot.margin = unit(c(1,10,1,1),"cm"))
  #+theme(plot.margin = unit(c(3,10,1,1),"cm"))
  p+geom_point(shape=1, size=3)+geom_smooth(method = "lm")
}



#' @export resid
#' @export
resid <- function(reg){
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat("Residuals: \n")
  print(unlist(reg$resi_v[,1]))
}

#' @export pred
#' @export
pred<-function(reg){
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat(" Predicted values: \n")
  print(unlist(reg$filted_v[,1]))
}

#' @export coef.linreg
#' @export
coef.linreg<-function(reg){
  print("coef")
  #cat("coefficient:",reg$call)
}

#' @export summary.linreg
#' @export
summary.linreg <- function(reg){
  print("summery")
  }


