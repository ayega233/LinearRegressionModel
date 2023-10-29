#'Ridge regrestion model
#' @name ridgereg


#' @description
#' RC type class for Ridge Regression model.
#' @field formula linear formula as symbolic model to filter data.
#' @field data should be data frame
#' @field lambda should be a numeric
#'
#' @export ridgereg
ridgereg <- setRefClass("ridgereg",
                      fields = list(formula = "formula",data = "data.frame",lambda ="numeric", params="list"),
                      methods = list(
                        initialize = function(formula, data, lambda) {
                          .self$formula<-formula
                          .self$data<-data
                          .self$lambda<-lambda
                          .self$params$param <-deparse(substitute(data))

                          x<-model.matrix(formula,data)
                          y<-data[,all.vars(formula)[1]]

                          x<- x[,-1]
                          n <- nrow(x)
                          p <- ncol(x)
                          x_mean <- apply(x,2,mean)

                          y_mean <- mean(y)
                          x<- x-rep(x_mean, rep(n, p))
                          x_square <-x^2
                          .self$params$x_square<-x_square
                          var_x<- apply(x_square,2,sum)/n
                          std_dev <- sqrt(var_x)
                          .self$params$std_dev<-std_dev
                          .self$params$var_x<-var_x

                          x<- x/rep(std_dev, rep(n, p))
                          .self$params$x_mean <-x_mean
                          .self$params$y_mean <-y_mean
                          .self$params$x <-x
                          .self$params$y <-y
                          I <- diag(p)

                          beta<- solve((t(x)%*%x)+(I*lambda))%*%t(x)%*%y
                          .self$params$beta_scaled <-drop(beta)
                          beta<-t(beta/std_dev)
                          inter <- y_mean - beta %*% x_mean

                          beta<- cbind(Intercept=inter, beta)
                          .self$params$beta <-drop(beta)

                          .self$params$filted_v <- model.matrix(formula,data)%*%.self$params$beta
                          .self$params$resi_v <- y - .self$params$filted_v
                        },
                        print = function(){
                          coefficient <-t(.self$params$beta)
                          cat("Call:\n")
                          cat(paste0("ridgereg(formula = ",deparse(.self$formula),", data = ",.self$params$param,")"))
                          cat("\nCoefficients:\n")
                          prmatrix(coefficient,rowlab=rep("",3))
                        },
                        coef = function(){
                          return(unname(t(.self$params$beta)))
                        },
                        predict = function(){
                          return(unlist(.self$params$filted_v[,1]))
                        }
                      )

)

