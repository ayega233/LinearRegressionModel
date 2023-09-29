#'Perform Multiple Linear Regression
#'
#'This function performs multiple linear regression and calculates various statistics.
#'
#' @param formula A formula specifying the regression model.
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class 'linreg' containing regression results and statistics.
#' @export
#'
#' @examples
#' data(mtcars)
#' model <- linreg(mpg ~ wt + hp, data = mtcars)
#'
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

  beta<- (solve(t(x)%*%x)%*%t(x))%*%y
  reg$beta <-beta
  
  reg$filted_v <- x%*%reg$beta
  reg$resi_v <- y - reg$filted_v

  return(reg)
}


print.linreg <-function(reg){
  coefficient <-t(reg$beta)
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat("Coefficients:\n")
  print(coefficient,row.names = FALSE)
}

#options(repr.plot.width = 2, repr.plot.height =3)

plot.linreg <-function(reg){

  plot_data <- data.frame(list(
    Residuals=reg$resi_v,
    Fitted=reg$filted_v
  ))
  #print(plot_data$Residuals)
  #label_data <-rownames(reg$data)
  #print(lapply(label_data, function (a){ return()}))
  #p <- ggplot2::ggplot(data=reg$data)+ (mapping = aes(x = reg$filted_v,y = reg$resi_v,displ, hwy))
  
  #p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = Fitted,y = Residuals ))+scale_x_continuous(name=paste("Fitted values\n ",deparse(reg$formula))) +scale_y_continuous(name="Residuals3")
  #p+geom_point(shape=1, size=4)+ylim(-2,2)+stat_summary(aes(y = Residuals,group=1), fun.y=mean, colour="red", geom="line",group=1)
 
   #+geom_smooth(method = "lm")
  #+geom_text(check_overlap = TRUE)

 # p2 <- ggplot2::ggplot(data=reg$data)+ (mapping = aes(x = filted_v,y = resi_v))
 # p+geom_point()+geom_smooth(method = "lm")
  
  p <- ggplot2::ggplot(data=plot_data,(mapping = aes(x = Fitted,y = Residuals )))+ylim(-1.5,1.5)
  #+theme(plot.margin = unit(c(1,10,1,1),"cm"))
  #+theme(plot.margin = unit(c(3,10,1,1),"cm"))
  p+geom_point(shape=1, size=3)+geom_smooth(method = "lm")
}




resid <- function(reg){
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat("Residuals: \n")
  print(unlist(reg$resi_v[,1]))
}


pred<-function(reg){
  cat("Call:\n")
  cat("linreg(",deparse(reg$formula),", data = ",reg$param,")\n")
  cat(" Predicted values: \n")
  print(unlist(reg$filted_v[,1]))
}


coef.linreg<-function(reg){
  print("coef")
  #cat("coefficient:",reg$call)
}

summary.linreg <- function(reg){
  print("summery")
  }

#data(iris)
#mod_object <-linreg(Petal.Length~Species, data = iris)

