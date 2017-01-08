context("test undocumented RStudio API calls")

test_that(".rs.readUiPref() returns an integer", {
    skip_on_cran()
    skip_on_appveyor()
    skip_on_travis()
    expect_type(.rs.readUiPref('num_spaces_for_tab'), type="integer")

})
