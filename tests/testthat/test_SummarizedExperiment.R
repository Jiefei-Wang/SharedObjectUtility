context("SummarizedExperiment class")

nrows <- 200
ncols <- 6
counts1 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
counts2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
countList <- SimpleList(counts1=counts1,counts2=counts2)
rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                     IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                     strand=sample(c("+", "-"), 200, TRUE),
                     feature_id=sprintf("ID%03d", 1:200))
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                      row.names=LETTERS[1:6])

countSummarizedExperiment <- SummarizedExperiment(assays=countList)
countRangedSummarizedExperiment <- SummarizedExperiment(assays=list(counts1=counts1,counts2=counts2),
                       rowRanges=rowRanges, colData=colData)

checkObject<-function(source,sharedTarget){
  targetClass <- paste0("Shared",capitalize(class(source)[1]))
  
  expect_true(is(sharedTarget,targetClass))
  expect_true(is.shared(sharedTarget))
  expect_true(length(is.shared(sharedTarget,internal=TRUE))==2)
  expect_true(all(unlist(is.shared(sharedTarget,internal=TRUE))))
  
  ##Convert back to SimpleList
  expect_error(source2 <- as(sharedTarget,class(source)[1]),NA)
  expect_identical(source2,source)
}


test_that("convert from SummarizedExperiment by as", {
  expect_error(b <- as(countSummarizedExperiment,"SharedSummarizedExperiment"),NA)
  checkObject(countSummarizedExperiment,b)
})


test_that("convert from SummarizedExperiment by toSharedClass", {
  expect_error(b <- toSharedClass(countSummarizedExperiment),NA)
  checkObject(countSummarizedExperiment,b)
})


test_that("convert from RangedSummarizedExperiment by as", {
  expect_error(b <- as(countRangedSummarizedExperiment,"SharedRangedSummarizedExperiment"),NA)
  checkObject(countRangedSummarizedExperiment,b)
})


test_that("convert from RangedSummarizedExperiment by toSharedClass", {
  expect_error(b <- toSharedClass(countRangedSummarizedExperiment),NA)
  checkObject(countRangedSummarizedExperiment,b)
})




