test_that("dog_breed_dataset works", {

  train <- dog_breed_dataset(
    split = "train",
    root = ".",
    download = TRUE
  )
  submition <- dog_breed_dataset(
    split = "submission",
    root = ".",
    download = TRUE
  )

  train_instance <- train$.getitem(1)
  submition_instance <- submition$.getitem(1)
  expect_length(train_instance, 4)
  expect_length(submition_instance, 4)
  expect_equal(names(submition_instance$y), names(train_instance$y))

})
