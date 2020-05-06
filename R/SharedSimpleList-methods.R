.listData <- function(x){
  x@listData
}

`.listData<-`<-function(x,value){
  x@listData <- value
  x
}


####################### constructor ###################################

SharedSimpleList <- function(..., copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                             sharedSubset=getSharedObjectOptions("sharedSubset"),
                             sharedCopy=getSharedObjectOptions("sharedCopy")){
  rawData <- tryShare(list(...),
                      copyOnWrite=copyOnWrite,
                      sharedSubset=sharedSubset,
                      sharedCopy=sharedCopy)
  x <- .SharedSimpleList(copyOnWrite=copyOnWrite,
                         sharedSubset=sharedSubset,
                         sharedCopy=sharedCopy)
  .listData(x) <- rawData
  x
}

####################### Type conversion ###################################
.shareList <- function(from){
  rawData <- lapply(from, tryShare)
  x <- .SharedSimpleList(copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                         sharedSubset=getSharedObjectOptions("sharedSubset"),
                         sharedCopy=getSharedObjectOptions("sharedCopy"))
  .listData(x) <- rawData
  x
}

setAs("vector", "SharedSimpleList",function(from){
  from <- as.list(from)
  as(from, "SharedSimpleList")
})

setAs("list", "SharedSimpleList",function(from){
  .shareList(from)
})

setAs("List", "SharedSimpleList",function(from){
  if(is(from,"SharedSimpleList")){
    return(from)
  }
  x <- .shareList(from)
  metadata(x) <- metadata(from)
  mcols(x) <- mcols(from)
  x
})
## This code must exist for dispatching method to 
## the signature List
setAs("SimpleList", "SharedSimpleList",function(from){
  callNextMethod()
})
setAs("Assays", "SharedSimpleList",function(from){
  x <- as(from, "SimpleList")
  as(x,"SharedSimpleList")
})


####################### vector methods ###################################
## names(<-),length,lengths,vertical_slot_names,
## [(<-),c,NROW,rename,nlevels,mcols,elementMetadata(<-),values(<-)
setReplaceMethod("[","SharedSimpleList",function(x,i,j,...,value){
  value <- tryShare(value,
                    copyOnWrite=x@copyOnWrite,
                    sharedSubset=x@sharedSubset,
                    sharedCopy=x@sharedCopy)
  callNextMethod()
})
setReplaceMethod("[[","SharedSimpleList",function(x,i,value){
  value <- tryShare(value,copyOnWrite=x@copyOnWrite,
                    sharedSubset=x@sharedSubset,
                    sharedCopy=x@sharedCopy)
  callNextMethod()
})

setMethod("c","SharedSimpleList",function(x,...){
  x <- callNextMethod()
  .listData(x) <- tryShare(.listData(x),
                           copyOnWrite=x@copyOnWrite,
                           sharedSubset=x@sharedSubset,
                           sharedCopy=x@sharedCopy)
  x
})



