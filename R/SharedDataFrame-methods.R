DataFrameDataSlot <- "listData"
.DFData <- function(x){
  slot(x,DataFrameDataSlot)
}

`.DFData<-`<-function(x,value){
  slot(x,DataFrameDataSlot) <- value
  x
}


####################### constructor ###################################

#' @export
SharedDataFrame <- function(..., copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                            sharedSubset=getSharedObjectOptions("sharedSubset"),
                            sharedCopy=getSharedObjectOptions("sharedCopy"), parentData){
  if(missing(parentData)){
    parentData <- DataFrame(...)
  }
  x <- new("SharedDataFrame", parentData)
  
  .DFData(x) <- tryShare(.DFData(x),
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
setAs("ANY","SharedDataFrame",function(from){
  as(as(from,"DataFrame"),"SharedDataFrame")
})
#' @export
setAs("vector","SharedDataFrame",function(from){
  as(as(from,"DataFrame"),"SharedDataFrame")
})
#' @export
setAs("list","SharedDataFrame",function(from){
  as(as(from,"DataFrame"),"SharedDataFrame")
})
#' @export
setAs("List","SharedDataFrame",function(from){
  as(as(from,"list"),"SharedDataFrame")
})
#' @export
setAs("DataFrame","SharedDataFrame",function(from){
  SharedDataFrame(parentData = from)
})


####################### vector methods ###################################
#' @export
setMethod("[","SharedDataFrame",function(x,i,j,..., drop = TRUE){
  res <- callNextMethod()
  if(is(res,"DataFrame")){
    SharedDataFrame(copyOnWrite=x@copyOnWrite,
                    sharedSubset=x@sharedSubset,
                    sharedCopy=x@sharedCopy,
                    parentData = res)
  }else{
    res
  }
})

#' @export
setReplaceMethod("[","SharedDataFrame",function(x,i,j,...,value){
  res <- callNextMethod()
  SharedDataFrame(copyOnWrite=x@copyOnWrite,
                  sharedSubset=x@sharedSubset,
                  sharedCopy=x@sharedCopy,
                  parentData = res)
})

#' @export
setMethod("c","SharedDataFrame",function(x, ...){
  res <- callNextMethod()
  SharedDataFrame(copyOnWrite=x@copyOnWrite,
                  sharedSubset=x@sharedSubset,
                  sharedCopy=x@sharedCopy,
                  parentData = res)
})

#' @export
setMethod("cbind","SharedDataFrame",function(..., deparse.level = 1){
  res <- callNextMethod()
  arg <- list(...)
  x <- arg[[1]]
  SharedDataFrame(copyOnWrite=x@copyOnWrite,
                  sharedSubset=x@sharedSubset,
                  sharedCopy=x@sharedCopy,
                  parentData = res)
})

#' @export
setMethod("rbind","SharedDataFrame",function(..., deparse.level = 1){
  res <- callNextMethod()
  arg <- list(...)
  x <- arg[[1]]
  SharedDataFrame(copyOnWrite=x@copyOnWrite,
                  sharedSubset=x@sharedSubset,
                  sharedCopy=x@sharedCopy,
                  parentData = res)
})



