context("SharedSimpleAssays class")


checkEqual<- function(x,y){
  expect_equal(length(x), length(y))
  expect_equal(names(x), names(y))
  for(i in seq_along(y))
    expect_identical(getListElement(x,i),getListElement(y,i))
}

m1 <- matrix(runif(24), ncol=3)
m2 <- matrix(runif(24), ncol=3)
a_list <- SimpleList(m1=m1,m2=m2)
a_assays <- as(a_List, "SimpleAssays")


test_that("convert from SimpleList", {
  expect_error(b <- as(a_list,"SharedSimpleAssays"),NA)
  
  expect_true(is(b,"SharedSimpleAssays"))
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a_list,b)
  
  ##Convert back to SimpleList
  expect_error(a_list2 <- as(b,"SimpleList"),NA)
  expect_identical(a_list,a_list2)
})

test_that("convert from SimpleAssays", {
  expect_error(b <- as(a_assays,"SharedSimpleAssays"),NA)
  
  expect_true(is(b,"SharedSimpleAssays"))
  expect_true(is.shared(b))
  expect_true(all(unlist(is.shared(b,internal=TRUE))))
  checkEqual(a_assays,b)
  
  ##Convert back to SimpleAssays
  expect_error(a_assays2 <- as(b,"SimpleAssays"),NA)
  expect_identical(a_assays,a_assays2)
})

test_that("Set Assays value", {
  expect_error(b <- as(a_assays,"SharedSimpleAssays"),NA)
  
  m3 <- matrix(runif(24), ncol=3)
  a_assays1 <- setListElement(a_assays,1,m3)
  expect_error(b1 <- setListElement(b,1,m3),NA)
  
  expect_true(is(b1,"SharedSimpleAssays"))
  expect_true(is.shared(b1))
  expect_true(all(unlist(is.shared(b1,internal=TRUE))))
  checkEqual(a_assays1,b1)
})


test_that("Subset Assays", {
  expect_error(b <- as(a_assays,"SharedSimpleAssays"),NA)
  
  a_assays1 <- a_assays[1]
  expect_error(b1 <- b[1],NA)
  
  expect_true(is(b1,"SharedSimpleAssays"))
  expect_true(is.shared(b1))
  expect_true(all(unlist(is.shared(b1,internal=TRUE))))
  checkEqual(a_assays1,b1)
})





























