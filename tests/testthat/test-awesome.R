
test_that("Github readme URL", {

  url <- 'https://github.com/qinwf/awesome-r/'

  expect_condition(github_readme_url(url), "readme")
})
