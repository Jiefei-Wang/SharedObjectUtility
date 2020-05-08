context("share function")


nrows <- 200
ncols <- 6
counts1 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
counts2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
countList <- SimpleList(counts1=counts1,counts2=counts2)
countAssay <- as(countList,"SimpleAssays")
countSummarizedExperiment <- SummarizedExperiment(assays=countList)

test_that("share SimpleList", {
  x <- countList
  expect_error(x_shr <- share(x),NA)
  
  expect_equal(x_shr, x)
  expect_true(length(is.shared(x_shr))==2)
  expect_true(all(unlist(is.shared(x_shr))))
})

test_that("share SimpleAssays", {
  x <- countAssay
  expect_error(x_shr <- share(x),NA)
  
  expect_equal(x_shr, x)
  expect_true(length(is.shared(x_shr))==2)
  expect_true(all(unlist(is.shared(x_shr))))
})

test_that("share SummarizedExperiment", {
  x <- countSummarizedExperiment
  expect_error(x_shr <- share(x),NA)
  
  expect_equal(x_shr, x)
  expect_true(length(is.shared(x_shr))==2)
  expect_true(all(unlist(is.shared(x_shr))))
})


##################Convert to shared class##################
test_that("SharedSimpleList", {
  x <- countList
  expect_error(x_shr <- toSharedClass(x),NA)
  
  expect_true(is(x_shr, "SharedSimpleList"))
  expect_true(length(is.shared(x_shr))==1)
  expect_true(length(is.shared(x_shr,internal=TRUE))==2)
  expect_true(is.shared(x_shr))
  expect_true(all(unlist(is.shared(x_shr,internal=TRUE))))
  
  x_origin <- as(x_shr, "SimpleList")
  expect_equal(x,x_origin)
  expect_true(all(unlist(is.shared(x_origin))))
})


test_that("SharedSimpleAssays", {
  x <- countAssay
  expect_error(x_shr <- toSharedClass(x),NA)
  
  expect_true(is(x_shr, "SharedSimpleAssays"))
  expect_true(length(is.shared(x_shr))==1)
  expect_true(length(is.shared(x_shr,internal=TRUE))==2)
  expect_true(is.shared(x_shr))
  expect_true(all(unlist(is.shared(x_shr,internal=TRUE))))
  
  x_origin <- as(x_shr, "SimpleAssays")
  expect_equal(x,x_origin)
  expect_true(all(unlist(is.shared(x_origin))))
})

test_that("SharedSummarizedExperiment", {
  x <- countSummarizedExperiment
  expect_error(x_shr <- toSharedClass(x),NA)
  
  expect_true(is(x_shr, "SharedSummarizedExperiment"))
  expect_true(length(is.shared(x_shr))==1)
  expect_true(length(is.shared(x_shr,internal=TRUE))==2)
  expect_true(is.shared(x_shr))
  expect_true(all(unlist(is.shared(x_shr,internal=TRUE))))
  
  x_origin <- as(x_shr, "SummarizedExperiment")
  expect_equal(x,x_origin)
  expect_true(all(unlist(is.shared(x_origin))))
})






