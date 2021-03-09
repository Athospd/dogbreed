library(torch)
test_that("nn_dog works", {
  nn <- nn_dog(classes = 2)

  expect_equal(class(nn), c("DogNet", "nn_module"))
  expect_equal((nn$resnet18$fc$out_feature), 2)
  expect_false(nn$resnet18$parameters$conv1.weight$requires_grad)
})
