# test `get_secret` and `op` functions

Sys.setenv(SOME_SECRET='abc123')

test_that("get_secret returns the correct secret", {
  expect_equal(get_secret('SOME_SECRET'), 'abc123')

  # test that `op` is not called when the secret is in the environment and additional arguments are *not* provided for `op`
  testval <- expect_error(get_secret('A_NONEXISTANT_SECRET'))

  # test that `op` is called when the secret is not in the environment and additional arguments are provided for `op`
  testval <- try(get_secret('ANOTHER_SECRET', 'Private', 'OPsecrets', 'test_secret'),
                 silent = TRUE)

  if(inherits(testval, "try-error"))
  {
    skip("Skipping test because `op` is not installed or the item is not in 1Password.")
  }

  expect_equal(testval, 'abc123')
})
