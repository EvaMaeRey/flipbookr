context("parse code")

test_that("create code is character", {
  expect_true(create_code() %>% code_as_table() %>% is.data.frame())

}
)
