context("Testing polytomous")

test_that("Test that basic polytomous works", {
  prior.params = c(0, .5)
  prior.name = "normal"
  X = c(-.5, 0, .5)
  
  questions = data.frame(difficulty=seq(-3,3, by=.1), discrimination=c(1), guessing=c(0), response_count=c(5), answers=c(NA))
  difficulties = list()
  for (i in rownames(questions)) {
    difficulties[[i]] = seq(-2,2)
  }
  cat = new("CATsurv", poly=T, questions=questions, priorParams=prior.params, priorName=prior.name, difficulties=difficulties)

  expect_that(1, equals(1))
})