#'Linear regrestion model
#' @name linreg
#' @import ggplot2
#' @import stats
#' @import methods
#' @import gridExtra



#' @description
#' RC type class for Liner Regression model.
#' @field formula linear formula as symbolic model to filter data.
#' @field data should be data frame
#' @export linreg
#' @examples
#' data(iris)
#' linreg_mod_object <- linreg$new(Petal.Length~Species, data = iris)
#' linreg_mod_object$print()
#' linreg_mod_object$plot()
#' linreg_mod_object$resid()
#' linreg_mod_object$pred()
#' linreg_mod_object$coef()
#' linreg_mod_object$summary()
linreg <- setRefClass("linreg",
        fields = list(formula = "formula",data = "data.frame",params="list"),
        methods = list(
          initialize = function(formula, data) {
            .self$formula<-formula
            .self$data<-data
            x<-model.matrix(formula,data)
            y<-data[,all.vars(formula)[1]]

            .self$formula <- formula
            .self$data <-data
            .self$params$x <-x
            .self$params$y <-y

            .self$params$param <-deparse(substitute(data))

            beta<- (solve(t(x)%*%x)%*%t(x))%*%y
            .self$params$beta <-beta

            .self$params$filted_v <- x%*%.self$params$beta
            .self$params$resi_v <- y - .self$params$filted_v

            .self$params$std_reduals <-sqrt(abs(.self$params$resi_v))

            n <-length(.self$params$y)
            .self$params$digree_of_free <- n - length(.self$params$beta)

            .self$params$resi_var <- (t(.self$params$resi_v)%*%.self$params$resi_v)/.self$params$digree_of_free

            var_of_reg_coef <- (solve(t(x)%*%x))

            standard_error<-sqrt(diag(var_of_reg_coef)%*%.self$params$resi_var)

            t_values <- beta/standard_error


            pt_values<-2*pt(abs(t_values), .self$params$digree_of_free, lower.tail = FALSE)

            .self$params$coefficients <- cbind(beta, standard_error, t_values, pt_values)
            dimnames(.self$params$coefficients) <- list(row.names(beta), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
          },

   #' @details Print function.
   #' @description
   #'This function print the coefficient of regretion.
          print = function(){
            coefficient <-t(.self$params$beta)
            cat("Call:\n")
            cat(paste0("linreg(formula = ",deparse(.self$formula),", data = ",.self$params$param,")"))
            cat("\nCoefficients:\n")
            prmatrix(coefficient,rowlab=rep("",3))
          },


   #' @details Plot data
   #' @description
   #' This function plot two graph using ggplot2
          plot = function(){
            plot_data <- data.frame(list(
              Residuals=.self$params$resi_v,
              Fitted=.self$params$filted_v
            ))
            plot_data1 <- data.frame(list(
              Fitted=.self$params$filted_v,
              Standardizedresiduals=sqrt(abs(.self$params$resi_v/as.numeric(.self$params$resi_var)))
            ))
            liu_theme <- theme(
              plot.title =element_text(colour = "#6a7e91",size = 12),
              plot.margin = margin(1, 1, 1, 1, "cm"),
              panel.grid.major = element_line(colour = "grey80"),
              panel.background = element_rect(
                fill = "white",
                colour = "black",
                linewidth = 1
              )
            )
            #logo <- readPNG("liu_logo.png")
            p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = Fitted,y = Residuals ))
            p <- p+ggplot2::labs(title = "Residuals vs Fitted" ,y="Residuals",x =paste0("Fitted \n lm(Petal.Length ~ Species)"))
            p <-p+geom_point(shape=1, size=2)+stat_summary(fun = "median",color="red",geom = "smooth")+liu_theme


            p1 <- ggplot2::ggplot(data=plot_data1)+ (mapping = aes(x = Fitted,y = Standardizedresiduals ))
            p1 <- p1+labs(y=expression(sqrt(abs("Standardized residuals"))),title ="Scale-Location")
            p1 <-p1+geom_point(shape=1, size=2)+stat_summary(fun = "median",color="red",geom = "smooth")+liu_theme+scale_y_continuous(breaks = c(0.5,1,1.5,2,2.5), limits = c(0.1,3))
            gridExtra::grid.arrange(p,p1,ncol=2)

          },

   #' @details Print residuals values.
   #' @description
   #' Print the residuals values.
          resid = function(){
            return(unlist(.self$params$resi_v[,1]))
          },



   #' @details Print the  predicted values
   #' @description
   #' Print the  predicted values
          pred = function(){
            return(unlist(.self$params$filted_v[,1]))
          },


   #' @details Coefficients
   #' @description
   #' Print the coefficients as a named vector.
          coef = function(){
            return(unname(t(.self$params$beta)))
          },


   #' @details Summery
   #' @description
   #' Print the coefficients with their standard error, t-value and p-value as well as the estimate of ^σ and the degrees
   #'  of freedom in the model, same as printed of lm function.
          summary  = function(){
            coefficient <-t(.self$params$beta)
            cat(paste0("linreg(formula = ",deparse(.self$params$formula),", data = ",.self$params$param,")"))
            cat("\nCoefficients:\n")
            printCoefmat(.self$params$coefficients, na.print = "NA")
            cat("Residual standard error:",sqrt(.self$params$resi_var), "on",.self$params$digree_of_free,"degrees of freedom")

          }
        )
)

