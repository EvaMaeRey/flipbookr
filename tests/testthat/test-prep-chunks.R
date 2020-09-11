context("partial chunks template")

test_that("return partial chunks is character", {
  expect_true(return_partial_chunks_template_code() %>%
                is.character()
                  )
}
)
