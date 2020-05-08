SimpleAssaysDataSlot<-"data"

.SAdata <- function(x){
  slot(x,SimpleAssaysDataSlot)
}

`.SAdata<-`<-function(x,value){
  slot(x,SimpleAssaysDataSlot) <- value
  x
}

SharedSimpleAssays<-function(..., copyOnWrite=getSharedObjectOptions("copyOnWrite"),
                             sharedSubset=getSharedObjectOptions("sharedSubset"),
                             sharedCopy=getSharedObjectOptions("sharedCopy"),
                             parentData){
  args <- list(...,
               copyOnWrite=copyOnWrite,
               sharedSubset=sharedSubset,
               sharedCopy=sharedCopy)
  
  if(!missing(parentData)){
    args <- c(args, parentData=as(parentData,"SimpleList"))
  }
  
  data <- do.call("SharedSimpleList",args)
  as(data,"SharedSimpleAssays")
}


setAs("SharedSimpleList", "SharedSimpleAssays",function(from){
  x <- .SharedSimpleAssays()
  .SAdata(x) <- from
  # x <- copySlots(parentData,x,SimpleAssaysDataSlot)
  x
})
setAs("SimpleList", "SharedSimpleAssays",function(from){
  x <- as(as(from, "SharedSimpleList"),"SharedSimpleAssays")
  x
})
## You must explicitly specify this type conversion
setAs("SimpleAssays", "SharedSimpleAssays",function(from){
  as(as(from, "SimpleList"),"SharedSimpleAssays")
})
setAs("ANY", "SharedSimpleAssays",function(from){
  as(as(from, "SimpleList"),"SharedSimpleAssays")
})

setAs("SharedSimpleAssays", "SharedSimpleList",function(from){
  .SAdata(from)
})
setAs("SharedSimpleAssays", "SimpleList",function(from){
  as(as(from, "SharedSimpleList"), "SimpleList")
})
setAs("SharedSimpleAssays", "SimpleAssays",function(from){
  as(as(from, "SimpleList"),"SimpleAssays")
})
setAs("SharedSimpleAssays", "list",function(from){
  as(as(from, "SimpleList"), "list")
})



# 
# setMethod("length","SharedSimpleAssays",function(x){
#   length(x@data)
# })
# 
# setMethod("names","SharedSimpleAssays",function(x){
#   names(x@data)
# })
# 
# setReplaceMethod("names","SharedSimpleAssays",function(x,value){
#   names(x@data) <- value
#   x
# })


# setMethod("getListElement", "SharedSimpleAssays",
#           function(x, i, exact=TRUE)
#           {
#             x@data[[i,exact=exact]]
#           }
# )

# 
# setMethod("setListElement", "SharedSimpleAssays",
#           function(x, i, value)
#           {
#             x@data[[i]] <- value
#           }
# )

# 
# setMethod("dim","SharedSimpleAssays",function(x){
#   dim(x@data)
# })

# 
# setMethod("[","SharedSimpleAssays",function(x,i,j,...){
#   x <- callNextMethod()
#   x
# })
# 
# setReplaceMethod("[","SharedSimpleAssays",function(x,i,j,...,value){
#   if(missing(j)){
#     x@data[i=i,...] <- value
#   }else{
#     x@data[i=i,j=j,...] <- value
#   }
#   x
# })
