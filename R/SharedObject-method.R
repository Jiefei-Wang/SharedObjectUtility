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
  colData(x) <- tryShare(colData(x))
  elementMetadata(x) <- tryShare(elementMetadata(x))
  metadata(x) <- tryShare(metadata(x))
  x
})
setMethod("share","SharedSimpleList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})
setMethod("share","SharedSimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
  x
})



###########################################
## is.shared
###########################################
setMethod("is.shared","List", function(x,...){
  lapply(x, is.shared)
})

setMethod("is.shared","Assays", function(x,...){
  n <- length(x)
  res <- vector("list",length(n))
  for(i in seq_len(n)){
    res[[i]] <- is.shared(getListElement(x,i))
  }
  res
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


setMethod("is.shared","SummarizedExperiment", function(x,...){
  allSlotNames <- slotNames(x)
  n <- length(allSlotNames)
  res <- vector("list",length(n))
  for(i in seq_len(n)){
    res[[i]] <- is.shared(slot(x,allSlotNames[i]))
  }
  res
})