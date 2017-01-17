m <- matrix(c(7,6,7,4,5,4,
              6,7,NaN,4,3,4,
              0,3,3,1,1,NA,
              1,2,2,3,3,4,
              1,0,1,2,3,3), 
            nrow = 5, 
            byrow = T)

d <- defineData(m, minimum = 1, maximum = 7)

test_that("Test Item Based:",{
  
  expect_s4_class(r <- rrecsys(d, "ibknn", simFunct = 1, neigh = 3), "IBclass")
  expect_equal(round(r@sim[2], digits = 6), .983002)
  expect_equal(round(r@sim[10], digits = 6), .950807)
  
  expect_s4_class(r <- rrecsys(d, "ibknn", simFunct = 2, neigh = 3), "IBclass")
  expect_equal(round(r@sim[2], digits = 6), .735083)
  expect_equal(round(r@sim[10], digits = 6), -0.73391)
  
  expect_s4_class(r <- rrecsys(d, "ibknn", simFunct = 3, neigh = 3), "IBclass")
  expect_equal(round(r@sim[2], digits = 6), .939511)
  expect_equal(round(r@sim[10], digits = 6), .684043)
  
  p <- predict(r)
  
  expect_equal(round(p[2,3], digits = 6), 5.488903)
  
})