SummarizedExperimentDataSlot <- "assays"
.SEdata <- function(x){
  slot(x,SummarizedExperimentDataSlot)
}

`.SEdata<-`<-function(x,value){
  slot(x,SummarizedExperimentDataSlot) <- value
  x
}


supported_class <- c("SimpleAssays")

shareAssays <- function(x,copyOnWrite,sharedSubset,sharedCopy){
  xAssays <- .SEdata(x)
  assayClass <- class(xAssays)[1]
  sharedAssayClass <- paste0("Shared",capitalize(assayClass))
  ind <-which(supported_class==assayClass)
  if(length(ind)!=0){
    #as(x_assays, supported_class[ind])
    .SEdata(x) <- do.call(sharedAssayClass,
                         list(copyOnWrite = copyOnWrite,
                              sharedSubset = sharedSubset,
                              sharedCopy = sharedCopy,
                              parentData = xAssays))
  }else{
    stop("The class <",sharedAssayClass,"> has not been supported.\n",
         "you may still able to create a shared object by calling the `share` function")
  }
  x
}


SharedSummarizedExperiment<-function(...,copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                                     sharedSubset=getSharedObjectOptions("sharedSubset"),
                                     sharedCopy=getSharedObjectOptions("sharedCopy"),
                                     parentData){
  if(missing(parentData)){
    parentData <- SummarizedExperiment(...)
  }
  x <- toSharedChildClass("SummarizedExperiment",parentData)
  x <- shareAssays(x,copyOnWrite,sharedSubset,sharedCopy)
}


SharedRangedSummarizedExperiment<-function(...,copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                                     sharedSubset=getSharedObjectOptions("sharedSubset"),
                                     sharedCopy=getSharedObjectOptions("sharedCopy"),
                                     parentData){
  if(missing(parentData)){
    parentData <- RangedSummarizedExperiment(...)
  }
  x <- toSharedChildClass("RangedSummarizedExperiment",parentData)
  x <- shareAssays(x,copyOnWrite,sharedSubset,sharedCopy)
}


setAs("SummarizedExperiment", "SharedSummarizedExperiment",function(from){
  x <- SharedSummarizedExperiment(parentData = from)
  x
})

setAs("SharedSummarizedExperiment", "SummarizedExperiment",function(from){
  x <- copyToParentClass(from)
  .SEdata(x) <- toParentClass(.SEdata(x))
  x
})

setAs("RangedSummarizedExperiment", "SharedRangedSummarizedExperiment",function(from){
  x <- SharedRangedSummarizedExperiment(parentData = from)
  x
})

setAs("SharedRangedSummarizedExperiment", "RangedSummarizedExperiment",function(from){
  x <- copyToParentClass(from)
  .SEdata(x) <- toParentClass(.SEdata(x))
  x
})



