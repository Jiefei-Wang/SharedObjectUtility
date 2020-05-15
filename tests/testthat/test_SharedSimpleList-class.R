context("SharedSimpleList class")

checkEqual<- function(x,y){
  expect_equal(length(x), length(y))
  expect_equal(names(x), names(y))
  for(i in seq_along(y))
    expect_identical(x[[i]],y[[i]])
}


m1 <- matrix(runif(24), ncol=3)
m2 <- matrix(runif(24), ncol=3)
a <- SimpleList(m1=m1,m2=m2)

test_that("constructor", {
  expect_error(b <- SharedSimpleList(m1=m1,m2=m2),NA)
  
  ## Check shared object
  check1 <- is.shared(b,recursive=FALSE)
  expect_length(check1, 2)
  expect_true(all(unlist(check1)))
  check2 <- is.shared(b,recursive=TRUE)
  expect_length(check2, 2)
  expect_true(all(unlist(check2)))
  checkEqual(a,b)
  
  ## non-sharable object
  expect_error(b1 <- SharedSimpleList(a="a"),NA)
  expect_false(is.shared(b1, recursive = TRUE)[[1]])
  expect_identical(b1[[1]], "a")
  expect_identical(names(b1),"a")
})



test_that("Convert from SimpleList", {
  expect_error(b <- as(a, "SharedSimpleList"),NA)
  ## Check shared object
  expect_true(all(unlist(is.shared(b,recursive=TRUE))))
  checkEqual(a,b)
})

test_that("Convert from list", {
  expect_error(b <- as(as.list(a), "SharedSimpleList"),NA)
  ## Check shared object
  expect_true(all(unlist(is.shared(b,recursive=TRUE))))
  checkEqual(a,b)
  
  ## non-sharable object
  expect_error(b1 <- as(list("a"), "SharedSimpleList"),NA)
  expect_false(is.shared(b1, recursive = TRUE)[[1]])
  expect_identical(b1[[1]], "a")
})


test_that("assign value", {
  b <- SharedSimpleList(m1=m1,m2=m2)
  
  ## use single bracket
  b[1] <- list(10:20)
  expect_true(all(unlist(is.shared(b,recursive=TRUE))))
  
  ## use double bracket
  b[[1]] <- 10:20
  expect_true(all(unlist(is.shared(b,recursive=TRUE))))
})

test_that("concatenate", {
  b <- c(SharedSimpleList(), a)
  expect_true(all(unlist(is.shared(b,recursive=TRUE))))
  checkEqual(a,b)
  
  ## non-sharable object
  b1 <- c(b, list("a"))
  expect_false(is.shared(b1, recursive = TRUE)[[3]])
  expect_identical(b1[[3]], "a")
})

















