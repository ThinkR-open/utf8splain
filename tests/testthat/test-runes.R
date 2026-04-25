test_that("runes() has a `display` column with the actual character (#1)", {
  res <- runes("hi \U0001F30D")
  expect_true("display" %in% names(res))
  # The earth globe emoji should appear in the display column for its row.
  globe_row <- res[res$rune == "U+1F30D", , drop = FALSE]
  expect_equal(nrow(globe_row), 1L)
  expect_equal(globe_row$display, "\U0001F30D")
})

test_that("runes() display column preserves ASCII characters", {
  res <- runes("hello")
  expect_equal(res$display, c("h", "e", "l", "l", "o"))
})

test_that("runes() splits multi-code-point graphemes per code point (#1)", {
  # The French flag emoji is a Regional Indicator pair (2 code points).
  res <- runes("\U0001F1EB\U0001F1F7")
  expect_equal(nrow(res), 2L)
  expect_equal(res$rune, c("U+1F1EB", "U+1F1F7"))
  expect_equal(res$display, c("\U0001F1EB", "\U0001F1F7"))
})

test_that("print.tbl_runes still works without mutate_at (#7)", {
  r <- runes("ok")
  out <- utils::capture.output(print(r))
  expect_true(any(grepl("utf-8 encoded string with", out, fixed = TRUE)))
})
