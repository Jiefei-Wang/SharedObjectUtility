###########################################
## share
###########################################
## DataFrame is dispatched to this function
## SimpleAtomicList
#' @export
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

#' @export
setMethod("share","SimpleAssays",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    x <- as(x, "SimpleList")
    xSharedList <- share(x,
                         copyOnWrite=copyOnWrite,
                         sharedSubset=sharedSubset,
                         sharedCopy=sharedCopy,
                         mustWork=mustWork,...)
    as(xSharedList, "SimpleAssays")
})

#' @export
setMethod("share","SummarizedExperiment",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    assays(x) <- share(assays(x),
                       copyOnWrite=copyOnWrite,
                       sharedSubset=sharedSubset,
                       sharedCopy=sharedCopy,
                       mustWork=mustWork,...)
    x
})

#' @export
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

#' @export
setMethod("share","LLint",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    x@bytes <- share(x@bytes,
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork = TRUE)
    x
})

## CompressedRleList is included
#' @export
setMethod("share","CompressedAtomicList",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    x@unlistData <- share(x@unlistData,
                          copyOnWrite=copyOnWrite,
                          sharedSubset=sharedSubset,
                          sharedCopy=sharedCopy,
                          mustWork,...)
    x
})

## We do not honor the accessor functions here
## Because we know the object must be valid and
## There is no need to do the validation. The
## accessor function copies the object so that
## it will turn the shared object into a regular object.
#' @export
setMethod("share","IRanges",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    x@start <- share(start(x),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork = TRUE)
    x@width <- share(width(x),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork = TRUE)
    x
})

#' @export
setMethod("share","GRanges",function(x,copyOnWrite,sharedSubset,sharedCopy,mustWork,...){
    seqnames(x) <- share(seqnames(x),
                          copyOnWrite=copyOnWrite,
                          sharedSubset=sharedSubset,
                          sharedCopy=sharedCopy,
                          mustWork = mustWork)
    ranges(x) <- share(ranges(x, use.names=TRUE, use.mcols=FALSE),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork = mustWork)
    strand(x) <- share(strand(x),
                     copyOnWrite=copyOnWrite,
                     sharedSubset=sharedSubset,
                     sharedCopy=sharedCopy,
                     mustWork = mustWork)
    mcols(x) <- share(mcols(x, use.names = FALSE),
                       copyOnWrite=copyOnWrite,
                       sharedSubset=sharedSubset,
                       sharedCopy=sharedCopy,
                       mustWork = mustWork)
    x
})


###########################################
## is.shared
###########################################
#' @export
setMethod("is.shared","List", function(x,recursive = FALSE,...){
    res <- lapply(x, function(x,...)is.shared(x,recursive=recursive, ...),...)
    if(!recursive){
        res <- lapply(res, function(x)any(unlist(x)))
    }
    res
})

#' @export
setMethod("is.shared","Assays", function(x,recursive = FALSE,...){
    n <- length(x)
    res <- vector("list",length(n))
    for(i in seq_len(n)){
        res[[i]] <- is.shared(getListElement(x,i), recursive = recursive,...)
    }
    names(res) <- names(x)
    res
})


#' @export
setMethod("is.shared","SummarizedExperiment", function(x,recursive = FALSE,...){
    res <- is.shared(assays(x,withDimnames = FALSE), recursive = recursive, ...)
    res
})


#' @export
setMethod("is.shared","Rle", function(x,...){
    list(values = is.shared(runValue(x),...),
         lengths = is.shared(runLength(x),...))
})


#' @export
setMethod("is.shared","LLint", function(x,...){
    is.shared(x@bytes,...)
})



#' @export
setMethod("is.shared","CompressedAtomicList", function(x,recursive = FALSE,...){
    is.shared(x@unlistData,recursive = recursive,...)
})


#' @export
setMethod("is.shared","IRanges", function(x,...){
    list(start = is.shared(start(x),...),
         width = is.shared(width(x),...))
})


#' @export
setMethod("is.shared","GRanges", function(x,recursive = FALSE,...){
    res <- list(seqnames = is.shared(seqnames(x), recursive = recursive,...),
         ranges = is.shared(ranges(x, use.names=TRUE, use.mcols=FALSE), recursive = recursive,...),
         strand = is.shared(strand(x), recursive = recursive,...),
         elementMetadata = is.shared(mcols(x, use.names = FALSE), recursive = recursive,...))
    res <- res[unlist(lapply(res, function(x) length(x) != 0))]
    if(!recursive){
        res <- lapply(res, function(x) any(unlist(x)))
    }
    res
})



