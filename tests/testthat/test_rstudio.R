context("test RStudio preference API calls")
is_interactive <- interactive()

test_that("readPreference() returns an integer", {
    skip_on_cran()
    skip_on_appveyor()
    skip_on_travis()
    skip_if_not(is_interactive)
    skip_if_not(rstudioapi::isAvailable())
    expect_type(rstudioapi::readPreference('num_spaces_for_tab', 4), type="integer")

})
