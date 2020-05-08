###########################################
## share
###########################################
setMethod("share","SimpleList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  for(i in seq_along(x)){
    x[[i]] <- share(x[[i]],
                    copyOnWrite=copyOnWrite,
                    sharedSubset=sharedSubset,
                    sharedCopy=sharedCopy,
                    mustWork=mustWork,...)
  }
  x
})

setMethod("share","SimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x <- as(x, "SimpleList")
  xSharedList <- share(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...)
  as(xSharedList, "SimpleAssays")
})

setMethod("share","SummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  assays(x) <- tryShare(assays(x))
  x
})
setMethod("share","SharedSimpleList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedSimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})


setGeneric("toSharedClass", function(x,copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                                         sharedSubset=getSharedObjectOptions("sharedSubset"),
                                         sharedCopy=getSharedObjectOptions("sharedCopy"),...){
  standardGeneric("toSharedClass")
})

standardSharedClass<- function(sourceClass,x,copyOnWrite,sharedSubset,sharedCopy){
  xClass <- class(x)[1]
  if(xClass!=sourceClass){
    stop("The class <",xClass,"> has not been supported")
  }
  sharedClass <- paste0("Shared",capitalize(xClass))
  func <- get(sharedClass)
  do.call(
    func,
    list(copyOnWrite = copyOnWrite,
         sharedSubset = sharedSubset,
         sharedCopy = sharedCopy,
         parentData = x)
  )
}

setMethod("toSharedClass","SimpleList",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("SimpleList",x,copyOnWrite,sharedSubset,sharedCopy)
})

setMethod("toSharedClass","SimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("SimpleAssays",x,copyOnWrite,sharedSubset,sharedCopy)
})
setMethod("toSharedClass","SummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("SummarizedExperiment",x,copyOnWrite,sharedSubset,sharedCopy)
})
setMethod("toSharedClass","RangedSummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("RangedSummarizedExperiment",x,copyOnWrite,sharedSubset,sharedCopy)
})


###########################################
## is.shared
###########################################
setMethod("is.shared","List", function(x,...){
  lapply(x, function(x,...)is.shared(x,...),...)
})

setMethod("is.shared","Assays", function(x,...){
  n <- length(x)
  res <- vector("list",length(n))
  for(i in seq_len(n)){
    res[[i]] <- is.shared(getListElement(x,i),...)
  }
  res
})


setMethod("is.shared","SummarizedExperiment", function(x,...){
  is.shared(slot(x,SummarizedExperimentDataSlot),...)
})

setMethod("is.shared","SharedSimpleList", function(x,internal = FALSE,...){
  if(internal)
    callNextMethod()
  else
    TRUE
})

setMethod("is.shared","SharedSimpleAssays", function(x,internal = FALSE,...){
  if(internal)
    callNextMethod()
  else
    TRUE
})

setMethod("is.shared","SharedSummarizedExperiment", function(x,internal = FALSE,...){
  if(internal)
    callNextMethod()
  else
    TRUE
})

setMethod("is.shared","SharedRangedSummarizedExperiment", function(x,internal = FALSE,...){
  if(internal)
    callNextMethod()
  else
    TRUE
})