context("share objects in IRanges package")




m1 <- Rle(3:5,2:4)
m2 <- Rle(3:5+10,2:4)

test_that("share Rle", {
    x <- m1
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr)
    expect_length(check1, 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 2)
    expect_true(all(unlist(check2)))
})

test_that("share SimpleRleList", {
    x <- RleList(m1=m1, m2=m2, compress = FALSE)
    expect_true(is(x, "SimpleRleList"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 2)
    expect_length(check1[[1]], 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 2)
    expect_length(check2[[1]], 1)
    expect_true(all(unlist(check2)))
})

test_that("share CompressedRleList", {
    x <- RleList(m1=m1, m2=m2)
    expect_true(is(x, "CompressedRleList"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 2)
    expect_true(all(unlist(check2)))
})


test_that("share atomic list", {
    x <- IntegerList(m1=m1, m2=m2, compress = FALSE)
    expect_true(is(x, "SimpleIntegerList"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 2)
    expect_length(check1[[1]], 1)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 2)
    expect_length(check2[[1]], 1)
    expect_true(all(unlist(check2)))
})

test_that("share compressed atomic list", {
    x <- IntegerList(m1=m1, m2=m2, compress = TRUE)
    expect_true(is(x, "CompressedIntegerList"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 1)
    expect_true(check1)
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 1)
    expect_true(check1)
})

test_that("share IRanges", {
    x <- IRanges(start=11, end=rep.int(20, 5))
    expect_true(is(x, "IRanges"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 2)
    expect_true(all(unlist(check2)))
})

test_that("share GRanges", {
    x <- GRanges(Rle(c("chr2", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
                   IRanges(1:10, width=10:1))
    expect_true(is(x, "GRanges"))
    expect_error(x_shr <- share(x),NA)
    
    ## Check equality
    expect_equal(x_shr, x)
    
    ## Check is.share
    check1 <- is.shared(x_shr, recursive = TRUE)
    expect_length(check1, 3)
    expect_length(check1[[1]], 2)
    expect_true(all(unlist(check1)))
    
    check2 <- is.shared(x_shr, recursive = FALSE)
    expect_length(check2, 3)
    expect_true(all(unlist(check2)))
})



