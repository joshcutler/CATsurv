context("Testing cauchy prior")

test_that("Cauchy prior is used when param is set", {
  prior.params = c(0, .5)
  prior.name = "cauchy"
  X = c(-.5, 0, .5)
  
  questions = data.frame(difficulty=seq(-3,3,by=.1), discrimination=c(1), guessing=c(0), answers=c(NA))
  cat = new("CATsurv", questions=questions, priorParams=prior.params, priorName=prior.name)
  
  cat.prior.values = prior(cat, X, prior.name, prior.params)
  
  expect_that(cat.prior.values, equals(dcauchy(X, prior.params[1], prior.params[2])))
})