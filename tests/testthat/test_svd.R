m <- matrix(c(7,6,7,4,5,4,
              6,7,NaN,4,3,4,
              0,3,3,1,1,NA,
              1,2,2,3,3,4,
              1,0,1,2,3,3), 
            nrow = 5, 
            byrow = T)

d <- defineData(m, minimum = 1, maximum = 7)

test_that("SVD", {
  
  expect_s4_class(r <- rrecsys(d, "funk", k = 3, gamma = .001, lambda = .001), "SVDclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.na(r@factors$V)))
  
  p <- predict(r)
  expect_equal(round(p[2,3], digits = 6), 4.37889)
  
  r <- rrecsys(d, "funk", k = 3, gamma = 1, lambda = .001)
  expect_true(any(is.na(r@factors$U)))
  expect_true(any(is.na(r@factors$V)))
  
  
})