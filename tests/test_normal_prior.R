context("Checking default usage")

test_that("nextItem works in default state", {
  questions = data.frame(difficulty=seq(-3,3,by=.1), discrimination=c(1), guessing=c(0), answers=c(NA))
  cat = new("CATsurv", questions=questions, priorParams=c(0,1.75))
  theta.est = estimateTheta(cat)
  expectedPV(cat, 30, theta.est)
  
  expect_that(theta.est, equals(0))
  
  next.item = nextItem(cat, theta.est)
  cat = storeAnswer(cat, next.item$next.item, 0)
  expect_that(as.numeric(next.item$next.item), equals(31))
  
  next.item = nextItem(cat)
  cat = storeAnswer(cat, next.item$next.item, 0)
  expect_that(as.numeric(next.item$next.item), equals(23))
  
  next.item = nextItem(cat)
  cat = storeAnswer(cat, next.item$next.item, 0)
  expect_that(as.numeric(next.item$next.item), equals(17))
  
  next.item = nextItem(cat)
  cat = storeAnswer(cat, next.item$next.item, 0)
  expect_that(as.numeric(next.item$next.item), equals(13))
  
  next.item = nextItem(cat)
  cat = storeAnswer(cat, next.item$next.item, 1)
  expect_that(as.numeric(next.item$next.item), equals(10))
  
  next.item = nextItem(cat)
  cat = storeAnswer(cat, next.item$next.item, 1)
  expect_that(as.numeric(next.item$next.item), equals(12))
  
  next.item = nextItem(cat)
  expect_that(as.numeric(next.item$next.item), equals(14))
})