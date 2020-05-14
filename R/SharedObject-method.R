###########################################
## share
###########################################
# DataFrame is dispatched to this function
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
  xSharedList <- share(x,
                       copyOnWrite=copyOnWrite,
                       sharedSubset=sharedSubset,
                       sharedCopy=sharedCopy,
                       mustWork=mustWork,...)
  as(xSharedList, "SimpleAssays")
})

setMethod("share","SummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  assays(x) <- share(assays(x),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork=mustWork,...)
  x
})

setMethod("share","Rle",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  # browser()
  lengths <- share(runLength(x),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork=mustWork,...)
  values <- share(runValue(x),
                        copyOnWrite=copyOnWrite,
                        sharedSubset=sharedSubset,
                        sharedCopy=sharedCopy,
                        mustWork=mustWork,...)
  new("Rle",x,lengths=lengths,values = values)
})
setMethod("share","LLint",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x@bytes <- share(x@bytes,
                   copyOnWrite=copyOnWrite,
                   sharedSubset=sharedSubset,
                   sharedCopy=sharedCopy)
  x
})

## RleList is included
setMethod("share","CompressedAtomicList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x@unlistData <- share(x@unlistData,
                   copyOnWrite=copyOnWrite,
                   sharedSubset=sharedSubset,
                   sharedCopy=sharedCopy)
  x
})




setMethod("share","SharedSimpleList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedSimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedSummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedRangedSummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedDataFrame",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})







standardSharedClass<- function(sourceClass,x,copyOnWrite,sharedSubset,sharedCopy,sharedClass){
  xClass <- class(x)[1]
  if(xClass!=sourceClass){
    stop("The class <",xClass,"> has not been supported")
  }
  if(missing(sharedClass))
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
setMethod("toSharedClass","DataFrame",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("DataFrame",x,copyOnWrite,sharedSubset,sharedCopy)
})
setMethod("toSharedClass","DFrame",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
  standardSharedClass("DFrame",x,copyOnWrite,sharedSubset,sharedCopy,"SharedDataFrame")
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


setMethod("is.shared","Rle", function(x,...){
  list(values = is.shared(runValue(x),...),
       lengths = is.shared(runLength(x),...))
})


setMethod("is.shared","LLint", function(x,...){
  is.shared(x@bytes,...)
})

setMethod("is.shared","CompressedAtomicList", function(x,internal = FALSE,...){
  is.shared(x@unlistData,internal = internal,...)
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

setMethod("is.shared","SharedDataFrame", function(x,internal = FALSE,...){
  if(internal)
    callNextMethod()
  else
    TRUE
})
