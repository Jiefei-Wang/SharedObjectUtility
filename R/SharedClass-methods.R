###########################################
## toSharedClass
###########################################
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
setMethod("toSharedClass","DataFrame",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
    standardSharedClass("DataFrame",x,copyOnWrite,sharedSubset,sharedCopy)
})
setMethod("toSharedClass","DFrame",function(x,copyOnWrite,sharedSubset,sharedCopy,...){
    standardSharedClass("DFrame",x,copyOnWrite,sharedSubset,sharedCopy,"SharedDataFrame")
})


###########################################
## is.shared
###########################################
# setMethod("is.shared","SharedSimpleList", function(x,recursive = FALSE,...){
#     if(recursive)
#         callNextMethod()
#     else
#         TRUE
# })
# 
# 
# setMethod("is.shared","SharedDataFrame", function(x,recursive = FALSE,...){
#     if(recursive)
#         callNextMethod()
#     else
#         TRUE
# })

