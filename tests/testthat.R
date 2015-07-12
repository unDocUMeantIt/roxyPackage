# the tests are realized using the "testthat" package
# and can be found in ../inst/tests

message(paste0("tests are currently commented out because they only work in a running R session,\n",
"not when called by R CMD check. there's an issue with the testthat package, triggered\n",
"by some setwd() in roxyPackage. see https://github.com/hadley/testthat/issues/86\n"))
# require(testthat)
# test_package("roxyPackage")
