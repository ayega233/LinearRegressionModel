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
print(mod_object)

plot.linreg <-function(reg){

  p <- ggplot2::ggplot(data=reg$data)+ (mapping = aes(x = reg$filted_v,y = reg$resi_v,displ, hwy))
  p+geom_point()+geo()

 # p2 <- ggplot2::ggplot(data=reg$data)+ (mapping = aes(x = filted_v,y = resi_v))
 # p+geom_point()+geom_smooth(method = "lm")
}
plot(mod_object)



resid.linreg <- function(reg){
  print("sss")
  print(reg$x)

  print(resi_v)
}

pred.linreg<-function(reg){
  print("pred")
  #cat("coefficient:",reg$call)
}
coef.linreg<-function(reg){
  print("coef")
  #cat("coefficient:",reg$call)
}
summery.linreg <- function(){
  print("summery")
  }


data(iris)
mod_object <-linreg(Petal.Length~Species, data = iris)


lm(Petal.Length~Species, data = iris)






#data(iris)mod_object <- lm(Petal.Length~Species, data = iris)
#print(mod_object)

