SimpleListDataSlot <- "listData"
.SLData <- function(x){
  slot(x,SimpleListDataSlot)
}

`.SLData<-`<-function(x,value){
  slot(x,SimpleListDataSlot) <- value
  x
}


####################### constructor ###################################

#' @export
SharedSimpleList <- function(..., copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                             sharedSubset=getSharedObjectOptions("sharedSubset"),
                             sharedCopy=getSharedObjectOptions("sharedCopy"), parentData){
  if(missing(parentData)){
    parentData <- SimpleList(...)
  }
  x <- new("SharedSimpleList", parentData)
  .SLData(x) <- tryShare(.SLData(x),
                         copyOnWrite=copyOnWrite,
                         sharedSubset=sharedSubset,
                         sharedCopy=sharedCopy)
  x@copyOnWrite <- copyOnWrite
  x@sharedSubset <- sharedSubset
  x@sharedCopy <- sharedCopy
  x
}

####################### Type conversion ###################################
.shareList <- function(from){
  rawData <- lapply(from, tryShare)
  x <- .SharedSimpleList(copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                         sharedSubset=getSharedObjectOptions("sharedSubset"),
                         sharedCopy=getSharedObjectOptions("sharedCopy"))
  .SLData(x) <- rawData
  x
}

#' @export
setAs("vector", "SharedSimpleList",function(from){
  from <- as.list(from)
  as(from, "SharedSimpleList")
})

#' @export
setAs("list", "SharedSimpleList",function(from){
  as(as(from,"SimpleList"),"SharedSimpleList")
})

#' @export
setAs("List", "SharedSimpleList",function(from){
  as(as(from,"SimpleList"),"SharedSimpleList")
})

#' @export
setAs("SimpleList", "SharedSimpleList",function(from){
  if(is(from,"SharedSimpleList")){
    return(from)
  }
  x <- SharedSimpleList(parentData = from)
  x
})

#' @export
setAs("Assays", "SharedSimpleList",function(from){
  x <- as(from, "SimpleList")
  as(x,"SharedSimpleList")
})


####################### vector methods ###################################
## names(<-),length,lengths,vertical_slot_names,
## [(<-),c,NROW,rename,nlevels,mcols,elementMetadata(<-),values(<-)
#' @export
setReplaceMethod("[","SharedSimpleList",function(x,i,j,...,value){
  if(is(value,"SimpleList")){
    .SLData(value) <- tryShare(.SLData(value),copyOnWrite=x@copyOnWrite,
                               sharedSubset=x@sharedSubset,
                               sharedCopy=x@sharedCopy)
  }else{
    value <- tryShare(value,copyOnWrite=x@copyOnWrite,
                      sharedSubset=x@sharedSubset,
                      sharedCopy=x@sharedCopy)
  }
  callNextMethod()
})
#' @export
setReplaceMethod("[[","SharedSimpleList",function(x,i,value){
  value <- tryShare(value,
                    copyOnWrite=x@copyOnWrite,
                    sharedSubset=x@sharedSubset,
                    sharedCopy=x@sharedCopy)
  callNextMethod()
})

#' @export
setMethod("c","SharedSimpleList",function(x,...){
  x <- callNextMethod()
  .SLData(x) <- tryShare(.SLData(x),
                         copyOnWrite=x@copyOnWrite,
                         sharedSubset=x@sharedSubset,
                         sharedCopy=x@sharedCopy)
  x
})



