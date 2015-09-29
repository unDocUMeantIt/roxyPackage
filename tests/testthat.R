# the tests are realized using the "testthat" package
# and can be found in ../inst/tests

Sys.setenv("R_TESTS" = "") # workaround to fix https://github.com/hadley/testthat/issues/86
require(testthat)
test_check("roxyPackage")
