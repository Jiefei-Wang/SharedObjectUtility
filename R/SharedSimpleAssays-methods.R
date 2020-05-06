.data <- function(x){
  x@data
}

`.data<-`<-function(x,value){
  x@data <- value
  x
}

SharedSimpleAssays<-function(...){
  data <- list(...)
  as(data,"SharedSimpleAssays")
}


setAs("SharedSimpleList", "SharedSimpleAssays",function(from){
  x <- .SharedSimpleAssays()
  .data(x) <- from
  x
})
setAs("SimpleList", "SharedSimpleAssays",function(from){
  as(as(from, "SharedSimpleList"),"SharedSimpleAssays")
})

setAs("SimpleAssays", "SharedSimpleAssays",function(from){
  as(as(from, "SimpleList"),"SharedSimpleAssays")
})
setAs("ANY", "SharedSimpleAssays",function(from){
  as(as(from, "SimpleList"),"SharedSimpleAssays")
})

setAs("SharedSimpleAssays", "SharedSimpleList",function(from){
  .data(from)
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
