context("Testing other functions in the package")
test_that("alt_bind testings", code = {
  mat1 <- diag(-(1:3))
  mat2 <- matrix(diag(1:6), ncol = 3, dimnames = list(letters[1:12], letters[1:3]))
  mat3 <- matrix(diag(1:6), nrow = 3, dimnames = list(letters[13:15], letters[13:24]))
  expect_error(alt_bind(mat1, mat2, mat3, alternate = 2))
  expect_error(alt_bind(mat1, mat2, mat3, alternate = 1))
  expect_error(alt_bind(mat1, mat2, alternate = 2))
  expect_error(alt_bind(mat1, mat2, alternate = 1))
  expect_error(alt_bind(mat1, mat3, alternate = 2))
  expect_error(alt_bind(mat1, mat3, alternate = 1))
  expect_error(alt_bind(mat1, mat1, alternate = 1, sequence = 1:2))
  expect_error(alt_bind(mat1, mat2, alternate = 2, sequence = 1:2))
  expect_error(alt_bind(mat1, mat2, alternate = 1, sequence = 1:2))
  expect_error(alt_bind(mat1, mat2, alternate = 1, sequence = 1:4))
  expect_named(alt_bind(mat3, mat1, t(mat2), alternate = 2, sequence = c(4, 1, 4)), expected = NULL)
  expect_named(alt_bind(mat2, mat1, t(mat3), alternate = 1, sequence = c(4, 1, 4)), expected = NULL)
  expect_named(object = alt_bind(t(mat3), mat2, alternate = 1, sequence = c(1, 1)), expected = NULL)
  expect_named(object = alt_bind(mat3, t(mat2), alternate = 2, sequence = c(1, 1)), expected = NULL)
  
  iris$prova <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 2)
  iris$prova2 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 3)
  iris$prova3 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 5)
  bvt_list <- lapply(split(iris, iris$Species), FUN = function(x) bivarTable(X = I(Sepal.Width>3) ~ . - Sepal.Length, data = x[-5],         
                                                           rounding = 3, condense.binary.factors = FALSE,                
                                                           drop.x = TRUE, drop.y = TRUE, margin = 2:1,                   
                                                           test = "both",                                              
                                                           fit.model = list(simple = ~ .,                                
                                                                            adj = ~ . + Sepal.Length),                    
                                                           outcome = 2,                                                   
                                                           FUN.model = list(simple = c("p-value" = "getPval"),            
                                                                            adj = c("OR (95% CI)" = "verticalgetORCI",    
                                                                                    "p-value" = "verticalgetPval")),      
                                                           show.quantiles = c(0, 0.5, 1)))    
  do.call(alt_bind, bvt_list)
  
})
