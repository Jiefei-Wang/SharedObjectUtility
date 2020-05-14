context("SharedDataFrame class")

checkEqual<- function(x,y){
  expect_equal(length(x), length(y))
  expect_equal(names(x), names(y))
  for(i in seq_along(y))
    expect_identical(x[[i]],y[[i]])
}


m1 <- runif(24)
m2 <- runif(24)
a <- DataFrame(m1=m1,m2=m2)


test_that("constructor", {
  expect_error(b <- SharedDataFrame(m1=m1,m2=m2),NA)
  ## Check shared object
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
  
  ## non-sharable object
  expect_error(b1 <- SharedDataFrame(a="a"),NA)
  expect_true(is.shared(b1))
  expect_false(is.shared(b1, internal = TRUE)[[1]])
  expect_identical(b1[[1]], "a")
  expect_identical(names(b1),"a")
})



test_that("Convert from DataFrame", {
  ## Coerce function
  expect_error(b <- as(a, "SharedDataFrame"),NA)
  ## Check shared object
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
  
  ## Standard convertion
  expect_error(b <- toSharedClass(a),NA)
  ## Check shared object
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
})

test_that("Convert from list", {
  expect_error(b <- as(as.list(a), "SharedDataFrame"),NA)
  ## Check shared object
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
  
  ## non-sharable object
  expect_error(b1 <- as(list("a"), "SharedDataFrame"),NA)
  expect_true(is.shared(b1))
  expect_false(is.shared(b1, internal = TRUE)[[1]])
  expect_identical(b1[[1]], "a")
})


test_that("assign value", {
  b <- SharedDataFrame(m1=m1,m2=m2)
  
  ## use single bracket
  ## Replace a piece of data
  x <- 3
  b[1,1] <- x
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  expect_equal(b[1,1],x)
  
  ## Replace a column
  x <- runif(nrow(b))
  b[,1] <- x
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  expect_equal(b[,1],x)
  
  ## use double bracket
  x <- runif(nrow(b))
  b[[1]] <- x
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  expect_equal(b[,1],x)
})


test_that("concatenate", {
  b1 <- SharedDataFrame(m1=m1)
  b2 <- SharedDataFrame(m2=m2)
  
  ## c
  b <- c(b1,b2)
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
  
  ## cbind
  b <- cbind(b1,b2)
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a,b)
  
  ## rbind
  d <- rbind(b1,b1)
  expect_true(all(unlist(is.shared(d,internal=TRUE))))
  checkEqual(DataFrame(m1=c(m1,m1)),d)
  
  
  ## non-sharable object
  e <- c(b, m3 = rep("a",nrow(b)))
  expect_true(is.shared(e))
  expect_false(is.shared(e, internal = TRUE)[[3]])
  checkEqual(c(a,m3=rep("a",nrow(a))),e)
})

















