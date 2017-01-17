m <- matrix(c(7,6,7,4,5,4,
              6,7,NaN,4,3,4,
              0,3,3,1,1,NA,
              1,2,2,3,3,4,
              1,0,1,2,3,3), 
            nrow = 5, 
            byrow = T)

d <- defineData(m, minimum = 1, maximum = 7)

test_that("Test Item Based:",{
  
  expect_s4_class(r <- rrecsys(d, "globalAverage"), "algAverageClass")
  expect_equal(round(r@average[1], digits = 6), 3.461538)
  expect_equal(round(r@average[10], digits = 6), 3.461538)
  
  expect_s4_class(r <- rrecsys(d, "itemAverage"), "algAverageClass")
  expect_equal(round(r@average[1], digits = 6), 3.75)
  expect_equal(round(r@average[10], digits = 6), 4.5)
  
  expect_s4_class(r <- rrecsys(d, "userAverage"), "algAverageClass")
  expect_equal(round(r@average[1], digits = 6), 5.5)
  expect_equal(round(r@average[10], digits = 6), 2)
  
  p <- predict(r)
  
  expect_equal(round(p[2,3], digits = 6), 4.8)
  
})