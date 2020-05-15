context("share List, Assays, SummarizedExperiment objects in SummarizedExperiment and S4Vector packages")


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
    expect_length(is.shared(x_shr, recursive = TRUE),2)
    expect_true(all(unlist(is.shared(x_shr))))
})

test_that("share SimpleAssays", {
    x <- countAssay
    expect_error(x_shr <- share(x),NA)
    
    expect_equal(x_shr, x)
    expect_length(is.shared(x_shr, recursive = TRUE),2)
    expect_true(all(unlist(is.shared(x_shr))))
})

test_that("share SummarizedExperiment", {
    x <- countSummarizedExperiment
    expect_error(x_shr <- share(x),NA)
    
    expect_equal(x_shr, x)
    expect_length(is.shared(x_shr, recursive = TRUE),2)
    expect_true(all(unlist(is.shared(x_shr))))
})


##################Convert to shared class##################
test_that("SharedSimpleList", {
    x <- countList
    expect_error(x_shr <- toSharedClass(x),NA)
    
    expect_true(is(x_shr, "SharedSimpleList"))
    expect_length(is.shared(x_shr, recursive=TRUE),2)
    expect_true(all(unlist(is.shared(x_shr,recursive=TRUE))))
    
    x_origin <- as(x_shr, "SimpleList")
    expect_equal(x,x_origin)
    expect_true(all(unlist(is.shared(x_origin))))
})






