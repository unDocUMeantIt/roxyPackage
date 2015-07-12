## these tests only work in a running R session (e.g., test_dir("path/to/this/dir")),
## not when called by R CMD check. roxyPackage uses some setwd() calls, triggering
## an issue with the testthat package. see https://github.com/hadley/testthat/issues/86

# force some dependencies which for unknown reasons are not loaded by testthat itself
library(roxygen2)

# define some objects to use globally
# call an internal shortcut to get the R version without patch level
RVers <- roxyPackage:::getRvers(win=TRUE)
testRoot <- file.path(tempdir(), "roxyPackageTest")
testPckRLibs <- file.path(testRoot, "R")
testPckRepo <- file.path(testRoot, "repo")
testPckRepoWin <- file.path(testPckRepo, "bin", "windows", "contrib", RVers)
if(isTRUE(RVers < "3.0")){
	testPckRepoOSX <- file.path(testPckRepo, "bin", "macosx", "leopard", "contrib", RVers)
} else {
	testPckRepoOSX <- file.path(testPckRepo, "bin", "macosx", "contrib", RVers)
}
testPckRoot <- file.path(testRoot, "examplePackage")
testPckR <- file.path(testPckRoot, "R")
testPckExmp <- file.path(testPckR, "simpleExample.R")

# define a function to generate a primitive test package
# in tempdir(), if it doesn't exist yet
generateTestPackage <- function(dir=testPckR, example=testPckExmp, RLibs=testPckRLibs){
	if(!isTRUE(file_test("-d", dir))){
		dir.create(dir, showWarnings=TRUE, recursive=TRUE)
	} else {}
	if(!isTRUE(file_test("-d", RLibs))){
		dir.create(RLibs, showWarnings=TRUE, recursive=TRUE)
	} else {}
	if(!isTRUE(file_test("-f", example))){
		cat("#' @export\nsimpleExample <- function() {NULL}\n", file=example)
	} else {}
	#missing startup.Rs!?
# 	startupWorkaround <- file.path(testPckRLibs, "..", "startup.Rs")
# 	if(!isTRUE(file_test("-f", startupWorkaround))){
# 		cat("# dummy\n", file=startupWorkaround)
# 	} else {}
	return(invisible(NULL))
}

# define package description
packageDescription <- data.frame(
	Package="examplePackage",
	Type="Package",
	Title="An R Example Package",
	Author="Ernst A. Dölle <e.a.doelle@example.com>",
	AuthorsR="c(person(given=\"Ernst\", family=\"Dölle\",
		email=\"e.a.doelle@example.com\",
		role=c(\"aut\", \"cre\")))",
	Maintainer="Ernst A. Dölle <e.a.doelle@example.com>",
	Depends="R (>= 2.10.0)",
	Description="Provides a great function to produce NULL results.",
	License="GPL (>= 3)",
	Encoding="UTF-8",
	LazyLoad="yes",
	URL="http://example.com/doelleInnovations",
	stringsAsFactors=FALSE)

changeLog <- list(
		added=c("new extra NULL feature", "new oblivion matrix"),
		fixed=c("finally resolved the reandom results bug")
	)

repoURL <- "http://example.com/doelleInnovations/repo"

# now check possible actions
context("basic packaging")
test_that("roxygenizing and building the package", {
	# initialize test environment
	generateTestPackage()

	sandbox(FALSE)
	roxy.package(
		pck.source.dir=testPckRoot,
		pck.version="0.01-1",
		R.libs=testPckRLibs,
		repo.root=testPckRepo,
		pck.description=packageDescription,
		actions=c(
			"roxy",
# 			"cite",
# 			"html",
# 			"cl2news",
# 			"news2rss",
#			"license"#,
# 			"log",
# 			"win",
# 			"macosx",
			"package"
		)#,
# 		ChangeLog=changeLog,
# 		URL=repoURL
	)

	# now we should see some more files
	expect_that(
		all(
			file_test("-f", file.path(testPckR, "examplePackage-package.R")),
			file_test("-f", file.path(testPckRoot, "DESCRIPTION")),
			file_test("-f", file.path(testPckRoot, "NAMESPACE")),
			file_test("-f", file.path(testPckRoot, "man", "examplePackage-package.Rd"))
		),
		is_true(),
		info="checking generated files in package sources"
	)
	# we should also have a repository
	expect_that(
		all(
			file_test("-f", file.path(testPckRepo, "src", "contrib", "examplePackage_0.01-1.tar.gz")),
			file_test("-f", file.path(testPckRepo, "src", "contrib", "PACKAGES")),
			file_test("-f", file.path(testPckRepo, "src", "contrib", "PACKAGES.gz"))
		),
		is_true(),
		info="checking the repository"
	)
	# and the package should have been installed
	expect_that(
		all(
			file_test("-f", file.path(testPckRLibs, "examplePackage", "R", "examplePackage.rdb")),
			file_test("-f", file.path(testPckRLibs, "examplePackage", "R", "examplePackage.rdx")),
			file_test("-f", file.path(testPckRLibs, "examplePackage", "DESCRIPTION")),
			file_test("-f", file.path(testPckRLibs, "examplePackage", "NAMESPACE")),
			file_test("-d", file.path(testPckRLibs, "examplePackage", "Meta")),
			file_test("-d", file.path(testPckRLibs, "examplePackage", "html")),
			file_test("-d", file.path(testPckRLibs, "examplePackage", "help"))
		),
		is_true(),
		info="checking successful package installation"
	)

	unlink(testRoot, recursive=TRUE)
})

test_that("packaging for windows", {
## debug:
#cat(loadedNamespaces(), file="/tmp/testNamespace.txt")
	# initialize test environment
	generateTestPackage()

## debug:
#cat(system(paste0("tree ", testRoot), intern=TRUE), file="/tmp/testRoot.txt", sep="\n")

## debug:
#sink(file="/tmp/testRun.txt")
	sandbox(FALSE)
	roxy.package(
		pck.source.dir=testPckRoot,
		pck.version="0.01-1",
		R.libs=testPckRLibs,
		repo.root=testPckRepo,
		pck.description=packageDescription,
		actions=c(
			"roxy",
 			"win",
			"package"
		)
	)

## debug:
#cat(system(paste0("tree ", testRoot), intern=TRUE), file="/tmp/testRoot.txt", sep="\n", append=TRUE)

	expect_that(
		all(
			file_test("-f", file.path(testPckRepoWin, "examplePackage_0.01-1.zip")),
			file_test("-f", file.path(testPckRepoWin, "PACKAGES")),
			file_test("-f", file.path(testPckRepoWin, "PACKAGES.gz"))
		),
		is_true()
	)

	unlink(testRoot, recursive=TRUE)
## debug:
#sink()
})

test_that("packaging for OS X", {
	# initialize test environment
	generateTestPackage()

	sandbox(FALSE)
	roxy.package(
		pck.source.dir=testPckRoot,
		pck.version="0.01-1",
		R.libs=testPckRLibs,
		repo.root=testPckRepo,
		pck.description=packageDescription,
		actions=c(
			"roxy",
			"macosx",
			"package"
		)
	)

	expect_that(
		all(
			file_test("-f", file.path(testPckRepoOSX, "examplePackage_0.01-1.tgz")),
			file_test("-f", file.path(testPckRepoOSX, "PACKAGES")),
			file_test("-f", file.path(testPckRepoOSX, "PACKAGES.gz"))
		),
		is_true()
	)

	unlink(testRoot, recursive=TRUE)
})

# test_that("debianizing the package", {
# 	# initialize test environment
# 	generateTestPackage()
# 
# 	# full package building will only run on debian systems,
# 	# so we'll only generate the debian folder
# 	roxy.package(
# 		pck.source.dir=testPckRoot,
# 		pck.version="0.01-1",
# 		R.libs=testPckRLibs,
# 		repo.root=testPckRepo,
# 		pck.description=packageDescription,
# 		actions=c(
# 			"roxy",
# 			"deb"
# 		),
# 		deb.options="deb",
# 		URL=repoURL
# 	)
# 
# 	expect_that(
# 		all(
# 			file_test("-f", file.path(testPckRepoOSX, "examplePackage_0.01-1.tgz")),
# 			file_test("-f", file.path(testPckRepoOSX, "PACKAGES")),
# 			file_test("-f", file.path(testPckRepoOSX, "PACKAGES.gz"))
# 		),
# 		is_true()
# 	)
# })

context("license file")
test_that("adding a license file", {
	# initialize test environment
	generateTestPackage()

	sandbox(FALSE)
	roxy.package(
		pck.source.dir=testPckRoot,
		pck.version="0.01-1",
		R.libs=testPckRLibs,
		repo.root=testPckRepo,
		pck.description=packageDescription,
		actions=c(
			"roxy",
			"license"
		)
	)

	expect_that(
		file_test("-f", file.path(testPckRoot, "LICENSE.txt")),
		is_true()
	)

	unlink(testRoot, recursive=TRUE)
})

context("ChangeLog")
test_that("adding a ChangeLog", {
	# initialize test environment
	generateTestPackage()

	sandbox(FALSE)
	roxy.package(
		pck.source.dir=testPckRoot,
		pck.version="0.01-1",
		R.libs=testPckRLibs,
		repo.root=testPckRepo,
		pck.description=packageDescription,
		actions=c(
			"roxy",
			"log"
		),
		ChangeLog=changeLog
	)

	expect_that(
		file_test("-f", file.path(testPckRoot, "ChangeLog")),
		is_true()
	)

	unlink(testRoot, recursive=TRUE)
})
