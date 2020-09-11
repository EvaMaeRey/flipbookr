context("create code")

test_that("create code is character", {
  expect_true(is.character(create_code()))
  expect_true(is.character(create_code_rotate_omit()))
  expect_true(is.character(create_ggplot_code()))
  expect_true(is.character(create_injectable_code()))
  expect_true(is.character(create_left_assign_code()))
  expect_true(is.character(create_python_code_pipeline()))
  expect_true(is.character(create_python_code()))

}
)
