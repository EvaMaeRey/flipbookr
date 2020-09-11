context("parse code")

test_that("create code is character", {
  expect_true(create_code() %>%
                code_parse() %>%
                parsed_return_partial_code_sequence() %>%
                is.list()
                  )
}
)
