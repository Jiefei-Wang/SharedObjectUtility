.SharedSimpleList <- setClass("SharedSimpleList",
                      contains="SimpleList",
                      representation(
                        copyOnWrite = "logical",
                        sharedSubset = "logical",
                        sharedCopy = "logical"
                      )
)

.SharedSimpleAssays <- setClass("SharedSimpleAssays",
                              contains="SimpleAssays"
)

.SharedSimpleAssays <- setClass("SharedSummarizedExperiment",
                                contains="SummarizedExperiment"
)




