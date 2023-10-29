data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris,lambda=5))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis,lambda=4))
})


 test_that("class is correct", {
   ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
  print(class(ridgereg_mod)[1])
   expect_true(class(ridgereg_mod)[1] == "ridgereg")
 })

 testthat::test_that("Check Ridge Regression",{
   testthat::expect_equal(ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5),ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5))
  })

 # testthat::test_that("Prediction is correct.",{
 #   ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
 #   #ridgereg1 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
 #   testthat::expect_equal(ridgereg_mod$predict(),predict(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5))
 # })

 # testthat::test_that("Corefficient correct.",{
 #   ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
 # testthat::expect_equal(ridgereg_mod$coef(),ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5))
 # })


