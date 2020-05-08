capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


copySlots<- function(class,source,target,excludeSlots){
  allSlots <- slotNames(class)
  allSlots <- allSlots[!allSlots%in%excludeSlots]
  for(i in allSlots){
    slot(target,i) <- slot(source,i)
  }
  target
}


getParentClass <- function(x){
  if(!is.character(x)){
    x <- class(x)[1]
  }
  classDef <- getClassDef(x)
  names(classDef@contains)[1]
}
## Call as function to do the conversion
toParentClass <- function(x){
  as(x,getParentClass(x))
}

## create a new object of parent class of x
## and copy all slots
copyToParentClass <- function(x){
  parentClass <- getParentClass(x)
  res <- new(parentClass)
  allSlots <- names(getSlots(parentClass))
  for(i in allSlots){
    slot(res,i) <- slot(x,i)
  }
  res
}

toSharedChildClass <-function(class,x){
  childClass <- paste0("Shared",class)
  res <- new(childClass)
  copySlots(class,x,res,NULL)
}
