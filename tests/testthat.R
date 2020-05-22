library(testthat)
library(flipbookr)

test_check("flipbookr")

test_that("code_as_table",{
  expect_equal(create_code() %>% code_as_table() %>% nrow(), 17)
  })

test_that("code_simple_parse",{
  expect_equal(create_code() %>% code_simple_parse() %>% .$user %>% sum(), 4)
  expect_equal(create_code() %>% code_simple_parse() %>% nrow(), 17)
})

test_that("code_parse",{
            expect_equal(create_code() %>% code_parse() %>% .$user %>% sum(), 4)
            expect_equal(create_code() %>% code_parse() %>% .$auto %>% sum(), 12)
            expect_equal(create_rotate_code() %>% code_parse() %>% .$rotate %>% sum(), 3)
            expect_equal(create_rotate_code() %>% code_parse() %>% .$non_seq %>% sum(), 13)
            expect_equal(create_code() %>% code_parse() %>% nrow(), 17)
})

test_that("parsed_calc_show",{
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show() %>% length(), 12)
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show(break_type = "user") %>% length() , 4)
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show(break_type = "non_seq") %>% length() , 3)
            expect_equal(create_rotate_code() %>% code_parse() %>% parsed_calc_show(break_type = "rotate") %>% length(), 3)
          })

test_that("parsed_calc_show",{
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show() %>% length(), 12)
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show(break_type = "user") %>% length() , 4)
            expect_equal(create_code() %>% code_parse() %>% parsed_calc_show(break_type = "non_seq") %>% length() , 3)
            expect_equal(create_rotate_code() %>% code_parse() %>% parsed_calc_show(break_type = "rotate") %>% length(), 3)
          })

test_that("parsed_calc_show",{
            expect_equal(create_code() %>% code_parse() %>% parsed_return_partial_code_sequence() %>% length(), 12)
            expect_equal(create_code() %>% code_parse() %>% parsed_return_partial_code_sequence(break_type = "user") %>% length() , 4)
            expect_equal(create_code() %>% code_parse() %>% parsed_return_partial_code_sequence(break_type = "non_seq") %>% length() , 3)
            expect_equal(create_rotate_code() %>% code_parse() %>% parsed_return_partial_code_sequence(break_type = "rotate") %>% length(), 3)
          })
