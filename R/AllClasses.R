.SharedSimpleList <- setClass("SharedSimpleList",
                      contains="SimpleList",
                      representation(
                        copyOnWrite = "logical",
                        sharedSubset = "logical",
                        sharedCopy = "logical"
                      )
)
.SharedRangedSummarizedExperiment <- setClass("SharedDataFrame",
                                              contains="DataFrame",
                                              representation(
                                                copyOnWrite = "logical",
                                                sharedSubset = "logical",
                                                sharedCopy = "logical"
                                              )
)


.SharedSimpleAssays <- setClass("SharedSimpleAssays",
                              contains="SimpleAssays"
)



.SharedSummarizedExperiment <- setClass("SharedSummarizedExperiment",
                                contains="SummarizedExperiment"
)
.SharedRangedSummarizedExperiment <- setClass("SharedRangedSummarizedExperiment",
                                        contains="RangedSummarizedExperiment"
)



