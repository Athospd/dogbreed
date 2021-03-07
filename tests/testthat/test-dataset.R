cat(getwd())

test_that("dog_breed_dataset works", {

  train <- dog_breed_dataset(
    split = "train",
    root = "inst",
    download = TRUE
  )
  submition <- dog_breed_dataset(
    split = "submition",
    root = "inst",
    download = TRUE
  )

  train_instance <- train$.getitem(1)
  submition_instance <- submition$.getitem(1)
  expect_length(train_instance, 3)
  expect_length(submition_instance, 3)

  expect_equal(names(submition_instance$y), names(train_instance$y))

})
