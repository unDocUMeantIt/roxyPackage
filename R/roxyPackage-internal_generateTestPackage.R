## internal function generateTestPackage()
# can be used to quickly setup a primitive test package and  environment
# the functions returns a named list of values to be used for testing
generateTestPackage <- function(
  rootDir=file.path(tempdir(), "roxyPackageTest"),
  version="0.01-1",
  packageDescription=data.frame(
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
    stringsAsFactors=FALSE
  ),
  changeLog=list(
    added=c("new extra NULL feature", "new oblivion matrix"),
    fixed=c("finally resolved the reandom results bug")
  ),
  deb.options=list(
    build.dir=rootDir,
    repo.name="doelle",
    deb.description=list(
      Maintainer="Ernst A. Dölle <e.a.doelle@example.com>"
    ),
    keep.build=FALSE
  ),
  URL="http://example.com/doelleInnovations/repo"
){
  result <- list()
  # call an internal shortcut to get the R version without patch level
  result[["RVers"]] <- roxyPackage:::getRvers(win=TRUE)
  result[["rootDir"]] <- rootDir
  result[["RLibs"]] <- file.path(rootDir, "R")
  result[["repo"]] <- file.path(rootDir, "repo")
  result[["repoWin"]] <- file.path(result[["repo"]], "bin", "windows", "contrib", result[["RVers"]])
  if(isTRUE(result[["RVers"]] < "3.0")){
    result[["repoOSX"]] <- file.path(result[["repo"]], "bin", "macosx", "leopard", "contrib", result[["RVers"]])
  } else {
    result[["repoOSX"]] <- file.path(result[["repo"]], "bin", "macosx", "contrib", result[["RVers"]])
  }
  result[["packageRoot"]] <- file.path(rootDir, "examplePackage")
  result[["R"]] <- file.path(result[["packageRoot"]], "R")
  
  result[["version"]] <- version
  result[["packageDescription"]] <- packageDescription
  result[["changeLog"]] <- changeLog
  result[["deb.options"]] <- deb.options
  result[["URL"]] <- URL
  
  if(!isTRUE(file_test("-d", result[["R"]]))){
    dir.create(result[["R"]], showWarnings=TRUE, recursive=TRUE)
  } else {}
  if(!isTRUE(file_test("-d", result[["RLibs"]]))){
    dir.create(result[["RLibs"]], showWarnings=TRUE, recursive=TRUE)
  } else {}

  exampleMin <- file.path(result[["R"]], "simpleExample.R")
  if(!isTRUE(file_test("-f", exampleMin))){
    cat("#' @export\nsimpleExample <- function() {NULL}\n", file=exampleMin)
  } else {}
  return(result)
}

# quietly clean up the mess
# takes the list retuend by generateTestPackage() and removes the root directory
# optionally only if the path starts with the current value of tempdir()
removeTestPackage <- function(testPackage, onlyIfTempdir=TRUE){
  stopifnot("rootDir" %in% names(testPackage))
  if(isTRUE(onlyIfTempdir)){
    currentTempDir <- tempdir()
    stopifnot(grepl(paste0("^", tempdir()), testPackage[["rootDir"]]))
  } else {}
  if(isTRUE(file_test("-d", testPackage[["rootDir"]]))){
    unlink(testPackage[["rootDir"]], recursive=TRUE)
  } else {}
  return(invisible(NULL))
}
