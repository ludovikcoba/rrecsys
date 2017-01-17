m <- matrix(c(7,6,7,4,5,4,
              6,7,NaN,4,3,4,
              0,3,3,1,1,NA,
              1,2,2,3,3,4,
              1,0,1,2,3,3), 
            nrow = 5, 
            byrow = T)

d <- defineData(m, minimum = 1, maximum = 7)

test_that("BPR",{
  
  expect_s4_class(r <- rrecsys(d, "bpr", k = 3), "BPRclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.nan(r@factors$U)))
  
  expect_false(any(is.na(r@factors$V)))
  expect_false(any(is.nan(r@factors$V)))
  
  p <- predict(r)
  
  expect_false(any(is.na(p)))
  
})

test_that("wALS",{
  
  expect_s4_class(r <- rrecsys(d, "wals",scheme = "uni"), "wALSclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.nan(r@factors$U)))
  
  expect_false(any(is.na(r@factors$V)))
  expect_false(any(is.nan(r@factors$V)))
  
  p <- predict(r)
  
  expect_false(any(is.na(p)))
  
  expect_s4_class(r <- rrecsys(d, "wals",scheme = "uo"), "wALSclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.nan(r@factors$U)))
  
  expect_false(any(is.na(r@factors$V)))
  expect_false(any(is.nan(r@factors$V)))
  
  p <- predict(r)
  
  expect_false(any(is.na(p)))
  
  expect_s4_class(r <- rrecsys(d, "wals",scheme = "io"), "wALSclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.nan(r@factors$U)))
  
  expect_false(any(is.na(r@factors$V)))
  expect_false(any(is.nan(r@factors$V)))
  
  p <- predict(r)
  
  expect_false(any(is.na(p)))
  
  expect_s4_class(r <- rrecsys(d, "wals",scheme = "co"), "wALSclass")
  
  expect_false(any(is.na(r@factors$U)))
  expect_false(any(is.nan(r@factors$U)))
  
  expect_false(any(is.na(r@factors$V)))
  expect_false(any(is.nan(r@factors$V)))
  
  p <- predict(r)
  
  expect_false(any(is.na(p)))
  
})
