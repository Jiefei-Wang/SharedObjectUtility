SimpleListDataSlot <- "listData"
.SLData <- function(x){
  slot(x,SimpleListDataSlot)
}

`.SLData<-`<-function(x,value){
  slot(x,SimpleListDataSlot) <- value
  x
}


####################### constructor ###################################

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

setAs("vector", "SharedSimpleList",function(from){
  from <- as.list(from)
  as(from, "SharedSimpleList")
})

setAs("list", "SharedSimpleList",function(from){
  as(as(from,"SimpleList"),"SharedSimpleList")
})

setAs("List", "SharedSimpleList",function(from){
  as(as(from,"SimpleList"),"SharedSimpleList")
})
setAs("SimpleList", "SharedSimpleList",function(from){
  if(is(from,"SharedSimpleList")){
    return(from)
  }
  x <- SharedSimpleList(parentData = from)
  x
})
setAs("Assays", "SharedSimpleList",function(from){
  x <- as(from, "SimpleList")
  as(x,"SharedSimpleList")
})


####################### vector methods ###################################
## names(<-),length,lengths,vertical_slot_names,
## [(<-),c,NROW,rename,nlevels,mcols,elementMetadata(<-),values(<-)
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
setReplaceMethod("[[","SharedSimpleList",function(x,i,value){
  value <- tryShare(value,
                    copyOnWrite=x@copyOnWrite,
                    sharedSubset=x@sharedSubset,
                    sharedCopy=x@sharedCopy)
  callNextMethod()
})

setMethod("c","SharedSimpleList",function(x,...){
  x <- callNextMethod()
  .SLData(x) <- tryShare(.SLData(x),
                         copyOnWrite=x@copyOnWrite,
                         sharedSubset=x@sharedSubset,
                         sharedCopy=x@sharedCopy)
  x
})



