#' @import ggplot2


library(ggplot2)
library(gridExtra)

#' @export linreg
#' @export
linreg <- function(formula,data){
  reg <- structure(list(),class="linreg")
  
  x<-model.matrix(formula,data)
  y<-data[,all.vars(formula)[1]]
  
  reg$formula <-formula
  reg$data <-data
  reg$param <-deparse(substitute(data))
  reg$x <-x
  reg$y <-y
  
  beta<- (solve(t(x)%*%x)%*%t(x))%*%y
  reg$beta <-beta
 
  reg$filted_v <- x%*%reg$beta
  reg$resi_v <- y - reg$filted_v
  
  reg$std_reduals <-sqrt(abs(reg$resi_v))
  
  n <-length(reg$y)
  reg$digree_of_free <- n - length(reg$beta)

  reg$resi_var <- (t(reg$resi_v)%*%reg$resi_v)/reg$digree_of_free
  
  var_of_reg_coef <- (solve(t(x)%*%x))
 
  standard_error<-sqrt(diag(var_of_reg_coef)%*%reg$resi_var)
  
  t_values <- beta/standard_error
 
  pt_values<-2*pt(abs(t_values), reg$digree_of_free, lower.tail = FALSE)
  
  reg$coefficients <- cbind(beta, standard_error, t_values, pt_values)
  dimnames(reg$coefficients) <- list(row.names(beta), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  
  return(reg)
}
mod_object <-linreg(Petal.Length~Species, data = iris)
#' @export print.linreg
#' @export
print.linreg <-function(reg){
  coefficient <-t(reg$beta)
  cat("Call:\n")
  cat(paste0("linreg(formula = ",deparse(reg$formula),", data = ",reg$param,")"))
  cat("\nCoefficients:\n")
  prmatrix(coefficient,rowlab=rep("",3))
}

#' @export plot.linreg
#' @export
plot.linreg <-function(reg){
  
  plot_data <- data.frame(list(
    Residuals=reg$resi_v,
    Fitted=reg$filted_v
  ))
  plot_data1 <- data.frame(list(
    Fitted=reg$filted_v,
    Standardizedresiduals=reg$std_reduals
  ))
  p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = Fitted,y = Residuals ))
      p+ggplot2::labs(title = "Residuals vs Fitted" ,y="Residuals",x =paste0("Fitted \n lm(Petal.Length ~ Species)"))
 # p+labs(y="Residuals",title ="Fitted \n lm(Petal.Length ~ Species)")
  p+geom_point(shape=1, size=4)+stat_summary(fun = "median",color="red",geom = "smooth")


  #p1 <- ggplot2::ggplot(data=plot_data1)+ (mapping = aes(x = Fitted,y = Standardizedresiduals ))+ggtitle("Scale−Location")
  #p1+labs(y="Residuals",title ="Fitted \n lm(Petal.Length ~ Species)")
  #p1+geom_point(shape=1, size=4)+stat_summary(fun = "median",color="red",geom = "smooth")
   
 # grid.arrange(p,p1,=1,)
 # plot_array <-c(1,2)
  #return(purrr::keep_at(list(p,p1),at = plot_array))
 # p
 # p1
}

plot(mod_object)

#' @export resid
#' @export
resid <- function(reg){
  return(unlist(reg$resi_v[,1]))
}

#' @export pred
#' @export
pred<-function(reg){
  return(unlist(reg$filted_v[,1]))
}

#' @export coef.linreg
#' @export
coef.linreg<-function(reg){
  coefficient <-t(reg$beta)
  return(unname(t(reg$beta)))
}

#' @export summary.linreg
#' @export
summary.linreg <- function(reg){
  coefficient <-t(reg$beta)
  cat(paste0("linreg(formula = ",deparse(reg$formula),", data = ",reg$param,")"))
  cat("\nCoefficients:\n")
  printCoefmat(reg$coefficients, na.print = "NA")
  cat("Residual standard error:",sqrt(reg$resi_var), "on",reg$digree_of_free,"degrees of freedom")
  
}

