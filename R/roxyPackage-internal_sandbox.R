# Copyright 2011-2014 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package roxyPackage.
#
# roxyPackage is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# roxyPackage is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with roxyPackage.  If not, see <http://www.gnu.org/licenses/>.


## sandbox specific functions

## function check.sandbox()
# function to check if sandboxing is activated
check.sandbox <- function(){
  snd.config <- get.roxyEnv(name="sandbox")
  return(slot(snd.config, "active"))
} ## end function check.sandbox()


## function pck.deps()
# this function returns the *names* of packages the given package depends on.
# if recursive=TRUE, it will also check those packages' dependencies
# recursively and return all names it finds down the way.
pck.deps <- function(package, R.libs, R.version, description=NULL, recursive=TRUE,
  depLevel=c("Depends", "Imports"), initSuggests=FALSE, known.deps=c()){
  # try to get a matrix with dependencies. first column should be the package names
  # if we hav a description, use that, otherwise try to read DESCRIPTION
  if(is.null(description)){
    description <- read.dcf(file.path(R.libs, package, "DESCRIPTION"))
  } else {}
  # check if this is the first run and suggested packages should be looked up, too
  initDepLevel <- depLevel
  if(isTRUE(initSuggests) && !"Suggests" %in% depLevel){
    depLevel <- c(depLevel, "Suggests")
  } else {}
  # package.dependencies() seems to be broken, since it only checks for "Depends"
  # in the default setting. but only try depLevel if it appears in DESCRIPTION,
  # because package.dependencies() will stop with an error otherwise...
  depLevel.tmp <- depLevel[depLevel %in% colnames(description)]
  if(isTRUE(R_system_version(R.version) < "3.3")){
    pck.deps.packages <- unique(unlist(sapply(depLevel.tmp, function(thisLevel){
        pck.dep.tmp <- as.data.frame(
          tools::package.dependencies(as.matrix(description), depLevel=thisLevel),
          stringsAsFactors=FALSE)
        return(pck.dep.tmp[,1])
      })))
  } else {
    # package.dependencies() is deprecated in R >= 3.3
    pck.deps.packages <- unique(
      tools::package_dependencies(package, db=description, which=depLevel.tmp)[[1]]
    )
  }
  # clean packages from those who are not available in R.libs
  pck.deps.packages <- pck.deps.packages[file_test("-d", file.path(R.libs, pck.deps.packages))]
  if(isTRUE(recursive)){
    pck.deps.recursive <- unlist(sapply(pck.deps.packages, function(thisPck){
      # make sure we drop the initial "Suggests" here
      pck.deps(
        package=thisPck, R.libs=R.libs, R.version=R.version, description=NULL, recursive=TRUE,
        depLevel=initDepLevel, initSuggests=FALSE, known.deps=pck.deps.packages
      )
    }))
    pck.deps.packages <- unique(c(known.deps, pck.deps.recursive))
  } else {}
  return(pck.deps.packages)
} ## end function pck.deps()


## function prep.sndbx.source.dir()
# a helper function for the following prepare.sandbox() functions
# - snd.pck.source.dir: the sandbox root directory for package sources, no package name
# - pck.source.dir: the original source directory, including package name
# - package: name of the package
prep.sndbx.source.dir <- function(snd.pck.source.dir, pck.source.dir, package){
  if(!identical(snd.pck.source.dir, pck.source.dir) && !identical(snd.pck.source.dir, "")){
    createMissingDir(dirPath=snd.pck.source.dir, action="sandbox")
    target.dir <- file.path(snd.pck.source.dir, package)
    # copy sources to sandbox, but only if not already present
    if(!file_test("-d", target.dir)){
      file.copy(
        from=pck.source.dir,
        to=snd.pck.source.dir,
        overwrite=TRUE,
        recursive=TRUE)
      src.basename <- basename(pck.source.dir)
      if(!identical(src.basename, package)){
        # in case we're copying from a source path that does not end in the plain package name,
        # e.g., a release branch with version information, the target directory must be changed
        # into the package basename. otherwise, actions like binary builds will fail, because they
        # are looking in the wrong places
        file.rename(from=file.path(snd.pck.source.dir, src.basename), to=target.dir)
      } else {}
      message(paste0("sandbox: copied '", package, "' sources to ", snd.pck.source.dir))
    } else {}
    return(target.dir)
  } else {
    return(pck.source.dir)
  }
} ## end function prep.sndbx.source.dir()


## function prep.sndbx.repo.root()
# a helper function for the following prepare.sandbox() functions
prep.sndbx.repo.root <- function(snd.repo.root, repo.root){
  if(!identical(snd.repo.root, repo.root) && !identical(snd.repo.root, "")){
    createMissingDir(dirPath=snd.repo.root, action="sandbox")
    return(snd.repo.root)
  } else {
    return(repo.root)
  }
} ## end function prep.sndbx.repo.root()


## function prepare.sandbox()
# option "initSuggests" will only fetch suggested packages for the initial package
# and then drop the level to not run into a recursive loop
prepare.sandbox <- function(package, description, pck.source.dir, R.libs, R.version, repo.root,
  depLevel=c("Depends", "Imports"), initSuggests=FALSE){
  snd.config <- get.roxyEnv("sandbox")
  if(!inherits(snd.config, "roxySandbox")){
    stop(simpleError("got strange readings for sandbox settings, please check!"))
  } else {}
  snd.pck.source.dir <- slot(snd.config, "pck.source.dir")
  snd.R.libs <- slot(snd.config, "R.libs")
  snd.repo.root <- slot(snd.config, "repo.root")
  result <- list()
  # now check if the given files differ from sandbox definitions. if so
  #  - try to create the neccessary directories
  #  - return the new directories as the ones to use

  result[["pck.source.dir"]] <- prep.sndbx.source.dir(
    snd.pck.source.dir=snd.pck.source.dir,
    pck.source.dir=pck.source.dir,
    package=package)

  if(!identical(snd.R.libs, R.libs) && !identical(snd.R.libs, "")){
    # the new sandbox R root will get different folders for different R versions
    snd.R.libs.Rver <- file.path(snd.R.libs, R.version)
    createMissingDir(dirPath=snd.R.libs.Rver, action="sandbox")
    # calculate package dependecies
    pck.dep.packages <- pck.deps(package=package, R.libs=R.libs, R.version=R.version, description=description,
      recursive=TRUE, depLevel=depLevel, initSuggests=initSuggests, known.deps=c())
    # we'll assume that the developer installed all needed dependencies here,
    # but we will not abort if packages are not found -- worst case is that
    # packaging will not work
    for (thisDep in pck.dep.packages){
      pck.dep.packages.path <- file.path(R.libs, thisDep)
      # check if package is in R.libs and not already in snd.R.libs
      if(file_test("-d", pck.dep.packages.path) && !file_test("-d", file.path(snd.R.libs.Rver, thisDep))){
        file.copy(
          from=pck.dep.packages.path,
          to=snd.R.libs.Rver,
          overwrite=TRUE,
          recursive=TRUE)
        message(paste0("sandbox: copied dependency '", thisDep, "' to ", snd.R.libs.Rver))
      } else {}
      rm(pck.dep.packages.path)
    }
    result[["R.libs"]] <- snd.R.libs.Rver
  } else {
    result[["R.libs"]] <- R.libs
  }

  result[["repo.root"]] <- prep.sndbx.repo.root(
    snd.repo.root=snd.repo.root,
    repo.root=repo.root)

  return(result)
} ## end function prepare.sandbox()


## function prepare.sandbox.archive()
# this is almost the same function as above, but only used in archiving contexts
prepare.sandbox.archive <- function(repo.root, archive.root, to.dir){
  snd.config <- get.roxyEnv("sandbox")
  if(!inherits(snd.config, "roxySandbox")){
    stop(simpleError("got strange readings for sandbox settings, please check!"))
  } else {}
  snd.repo.root <- slot(snd.config, "repo.root")
  snd.archive.root <- slot(snd.config, "archive.root")
  snd.to.dir <- slot(snd.config, "to.dir")
  result <- list()
  # now check if the given files differ from sandbox definitions. if so
  #  - try to create the neccessary directories
  #  - return the new directories as the ones to use
  result[["repo.root"]] <- prep.sndbx.repo.root(
    snd.repo.root=snd.repo.root,
    repo.root=repo.root)
  if(!identical(snd.archive.root, archive.root) && !identical(snd.archive.root, "")){
    snd.archive <- file.path(snd.archive.root, snd.to.dir)
    createMissingDir(dirPath=snd.archive, action="sandbox")
    result[["archive.root"]] <- snd.archive.root
    result[["to.dir"]] <- snd.to.dir
  } else {
    result[["archive.root"]] <- archive.root
    result[["to.dir"]] <- to.dir
  }
  return(result)
} ## end function prepare.sandbox.archive()

## function prepare.sandbox.deb()
# this is almost the same function as above, but only used in debianzing contexts
# it can work without "description" and "package", but must be called after a DESCRIPTION was generated,
# *excpept* if the package name is given!
prepare.sandbox.deb <- function(pck.source.dir, repo.root, package=NULL){
  snd.config <- get.roxyEnv("sandbox")
  if(!inherits(snd.config, "roxySandbox")){
    stop(simpleError("got strange readings for sandbox settings, please check!"))
  } else {}
  snd.pck.source.dir <- slot(snd.config, "pck.source.dir")
  snd.repo.root <- slot(snd.config, "repo.root")
  result <- list()

  if(is.null(package)){
    # get the package description, mainly to read the package name from it
    description <- read.dcf(file.path(pck.source.dir, "DESCRIPTION"))
    package <- getDescField(desc=description, field="Package", stopOnErr=TRUE)
  } else {}

  result[["pck.source.dir"]] <- prep.sndbx.source.dir(
    snd.pck.source.dir=snd.pck.source.dir,
    pck.source.dir=pck.source.dir,
    package=package)

  result[["repo.root"]] <- prep.sndbx.repo.root(
    snd.repo.root=snd.repo.root,
    repo.root=repo.root)

  return(result)
} ## end function prepare.sandbox.deb()
