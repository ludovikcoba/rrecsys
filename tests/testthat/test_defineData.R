
m <- matrix(c(7,6,7,4,5,4,
              6,7,NaN,4,3,4,
              0,3,3,1,1,NA,
              1,2,2,3,3,4,
              1,0,1,2,3,3), 
            nrow = 5, 
            byrow = T)

test_that("defineData works on NaN, NA, 0 and threshold", {
  
  expect_warning(d <- defineData(m, minimum = 1, maximum = 5))
  
  expect_true(is.na(d@data[2,3]))
  expect_true(is.na(d@data[3,6]))
  expect_true(is.na(d@data[3,1]))
  expect_true(is.na(d@data[1,1]))
  expect_true(d@data[1,5] == 5)
  expect_equal(ncol(d), 6)
  expect_equal(nrow(d), 5)
  expect_equal(rowRatings(d), c(3,3,4,6,5))
  expect_equal(colRatings(d), c(2,2,3,5,5,4))
  expect_equal(numRatings(d), 21)
})

b <- defineData(m, binary = TRUE, goodRating = 5)

test_that("Binary convertion works?",{
  expect_error(defineData(m, binary = TRUE))
  expect_true(is.na(b@data[2,3]))
  expect_true(is.na(b@data[3,6]))
  expect_true(b@data[3,1] == 0)
  expect_true(b@data[1,1] == 1)
})
