context("share List, Assays, SummarizedExperiment objects in SummarizedExperiment and S4Vector packages")


nrows <- 200
ncols <- 6
counts1 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
counts2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
counts3 <- list(counts1 = counts1, counts2 = counts2)
countList <- SimpleList(counts1=counts1, counts2=counts2, counts3=counts3)
countAssay <- as(countList[1:2],"SimpleAssays")
countSummarizedExperiment <- SummarizedExperiment(assays=countList[1:2])

test_that("share SimpleList", {
    x <- countList
    expect_error(x_shr <- share(x),NA)
    
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 3)
    expect_length(check1[[3]], 2)
    expect_true(all(unlist(check1)))
    expect_identical(names(x),names(check1))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 3)
    expect_length(check2[[3]], 1)
    expect_true(all(unlist(check2)))
    
})

test_that("share SimpleAssays", {
    x <- countAssay
    expect_error(x_shr <- share(x),NA)
    
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1,2)
    expect_true(all(unlist(check1)))
    expect_identical(names(x),names(check1))
})

test_that("share SummarizedExperiment", {
    x <- countSummarizedExperiment
    expect_error(x_shr <- share(x),NA)
    
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1,2)
    expect_true(all(unlist(check1)))
    expect_identical(assayNames(x),names(check1))
})


##################Convert to shared class##################
test_that("SharedSimpleList", {
    x <- countList
    expect_error(x_shr <- toSharedClass(x),NA)
    
    expect_true(is(x_shr, "SharedSimpleList"))
    
    ## check is.shared
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1,3)
    expect_length(check1[[3]], 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 3)
    expect_length(check2[[3]], 1)
    expect_true(all(unlist(check2)))
    
    
    x_origin <- as(x_shr, "SimpleList")
    expect_equal(x,x_origin)
    expect_true(all(unlist(is.shared(x_origin))))
})






