# Copyright 2011-2016 Meik Michalke <meik.michalke@hhu.de>
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


## function debianPkgVersion()
debianPkgVersion <- function(version="0.01", revision="1", epoch=NULL){
  deb.pckg.vers <- paste(version, revision, sep="-")
  if(!is.null(epoch)){
    deb.pckg.vers <- paste(epoch, deb.pckg.vers, sep=":")
  } else {}
  return(deb.pckg.vers)
} ## end function debianPkgVersion()


## function debianPkgName()
# origin: string or vector
debianPkgName <- function(package, origin=NULL, version=NULL, replace.dots=FALSE){
    package <- tolower(package)
    if(isTRUE(replace.dots)){
      package <- gsub("\\.", "-", package)
    } else {}

    if(is.null(version)){
      result <- package
    } else {
      result <- paste(package, version, sep=" ")
    }

    if(!is.null(origin)){
      result <- paste("r", origin, result, sep="-", collapse=" | ")
    } else {}

    return(result)
} ## end function debianPkgName()


## function splitDepends()
# takes a dependency vector (e.g., "foo (>> 3.0), bar, baz") and splits it into a matrix with
# one column for each package name and one for a version number, if present
# returns NULL if there are no dependencies
splitDepends <- function(dep){
  # just a precaution -- replace NA values
  dep[is.na(dep)] <- ""
  # remove names, trim & split
  dep <- trim(unlist(strsplit(as.character(dep), ",")))
  dep.length <- length(dep)
  if(dep.length > 0){
    results <- matrix(data="", nrow=dep.length, ncol=2, dimnames=list(NULL,c("package", "version")))
    for (thisDep.num in 1:dep.length){
      thisDep <- dep[thisDep.num]
      # split into <name> and (<version>)
      depParts <- unlist(strsplit(thisDep, split="[[:space:]]*\\("))
      results[thisDep.num,"package"] <- depParts[1]
      if(!is.na(depParts[2])){
        results[thisDep.num,"version"] <- paste0("(", depParts[2])
      } else {}
    }
  } else {
    results <- NULL
  }
  return(results)
} ## end function splitDepends()


## function debianizeDepends()
# origin: string or vector
# origin.alt: *named* list(pckgname=c("origin.string"))
# R: the package name for R, usually not "r-cran-<something>". if set to NULL, R will be dropped completely if present
# forceRVersion: omitted if NULL, i.e., the R version string is used as defined in DESCRIPTION (if any)
# collapse: if NULL returns a named vector (names are the original R package names, values the debian package names)
# drop.version: if TRUE returns only the package names (overwritten by forceRVersion!)
# append: a character string/vector to append additional debian package names to the results
debianizeDepends <- function(dep, origin="cran", origin.alt=list(), R="r-base-core",
  base=c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tools", "tcltk", "utils"),
  forceRVersion=getRvers(), collapse=", ", drop.version=FALSE, append=NULL, replace.dots=FALSE){

  dep.split <- splitDepends(dep)
  if(is.null(dep.split)){
    return("")
  } else {}
  names(origin.alt) <- tolower(names(origin.alt))
  other.origin.names <- names(origin.alt)

  base.packages <- tolower(base)

  dep.list <- list()
  # minimum requirements?
  if(!is.null(forceRVersion) && !is.null(R)){
    dep.list[["R"]] <- list(package=R, version=paste0("(>= ", forceRVersion, ")"), origin=NULL)
  } else {}

  for (thisDep in 1:nrow(dep.split)){
    dep.lst.key <- dep.split[thisDep, "package"]
    dep.lst.package <- tolower(dep.lst.key)

    # check for special cases:
    # - packages from base should be dropped
    if(dep.lst.package %in% base.packages){
      next
    } else {}

    if(nchar(dep.split[thisDep, "version"]) > 0 && !isTRUE(drop.version)){
      dep.lst.version <- dep.split[thisDep, "version"]
    } else {
      dep.lst.version <- NULL
    }

    # - R should be translated into the value of "R", or be dropped?
    if(dep.lst.package == "r"){
      if(!is.null(forceRVersion) || is.null(R)){
        next
      } else {
        dep.lst.package <- R
        dep.list[["R"]] <- list(package=R, version=dep.lst.version, origin=NULL)
        next
      }
    } else {}

    dep.lst.origin <- origin
    # check if this package should come from another origin
    if(dep.lst.package %in% other.origin.names){
      dep.lst.origin <- origin.alt[[dep.lst.package]]
    } else {}
    dep.list[[dep.lst.key]] <- list(package=dep.lst.package, version=dep.lst.version, origin=dep.lst.origin)
  }
  
  # re-combine to <origin>-<name> (<version>)
  results <- sapply(dep.list, function(thisDep){
      debianPkgName(package=thisDep[["package"]], origin=thisDep[["origin"]], version=thisDep[["version"]], replace.dots=replace.dots)
    })

  if(!is.null(append)){
    results <- c(results, append)
  } else {}

  if(is.null(collapse)){
    return(results)
  } else {
    return(paste(results, collapse=collapse))
  }
} ## end function debianizeDepends()


## function deb.check.sources()
# checks if we're dealing with a sources directory, a tarball or need to download something
deb.check.sources <- function(src,
  dl.dir=file.path(tempdir(),"roxyPackge","downloads"),
  local.src.dir=file.path(tempdir(),"roxyPackge","local_sources"),
  action="deb"){

  if(file_test("-d", src)){
    return(src)
  } else if(grepl("^http://|^https://|^ftp://|^sftp://", src, ignore.case=TRUE)){
    # seems to be a URL for downloading
    short.file.name <- gsub("(.*/)([^/]*)", "\\2", src, perl=TRUE)
    message(paste0(action, ": preparing for download ", short.file.name,  "..."))
    createMissingDir(dirPath=dl.dir, action="deb")
    dl.local.path <- file.path(dl.dir, short.file.name)
    download.file(url=src, destfile=dl.local.path)
    message(paste0(action, ": downloaded package to ", dl.local.path))
    full.path <- dl.local.path
  } else if(grepl("\\.gz$|\\.tgz$", src, ignore.case=TRUE)){
    # seems to be a local tarfile
    short.file.name <- gsub("(.*/)([^/]*)", "\\2", src, perl=TRUE)
    full.path <- normalizePath(src)
  } else {
    stop(simpleError(paste0("deb: unable to deal with ", paste0(src, collapse=", "))))
  }
  createMissingDir(dirPath=local.src.dir, action="deb")
  message(paste0(action, ": unpacking ", short.file.name,  "..."))
  # now we need to know the dirname which will be created upon untaring
  # we'll assume we get exactly one directory -- if not, stop with an error
  # to be on the safe side...
  all.files <- untar(full.path, list=TRUE)
  untar.dir <- unique(gsub("([^/]*)(.*)", "\\1", all.files, perl=TRUE))
  if(length(untar.dir) > 1 || nchar(untar.dir) < 1){
    stop(simpleError(paste0(action, ": sorry, couldn't determine a sane directory name from the archive!")))
  } else {}
  untar(full.path, exdir=local.src.dir)
  return(file.path(local.src.dir, untar.dir))
} ## end function deb.check.sources()


## function check.installed.deps()
# tries to find out whether a needed dependency is already installed
# returns either TRUE/FALSE, or the package name(s) found
# dep: can be a vector, but they will be treated as *alternatives*, that is, result will be TRUE if
#   any one of the packages is found (as an alternative to do "package1 | package2" checks)
check.installed.deps <- function(dep, value=FALSE, action="deb"){
  if(!isUNIX()){
    stop(simpleError(paste0(action, ": this doesn't seem to be a UNIX system, so i assume it's not Debain as well!")))
  } else {}

  dpkg <- Sys.which("dpkg")
  if("" %in% dpkg){
    stop(simpleError(paste0(action, ": can't find dpkg -- are you sure this is a Debain system?!")))
  } else {}

  get.installed.syscall <- paste0(dpkg, " --get-selections")
  all.installed <- system(get.installed.syscall, intern=TRUE)

  # look for the dependency in all installed packages
  whatWasFound <- grep(paste0("^(", paste0(gsub("[[:space:]]", "", dep), collapse="|"), ")[[:space:]]*install"), all.installed, value=TRUE)
  if(isTRUE(value)){
    return(gsub("[[:space:]]*install", "", whatWasFound))
  } else {
    if(length(whatWasFound) > 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
} ## end function check.installed.deps()


## function check.deb.availability()
# checks if a given debian package is available in the configured repositories
# the debian package name can be generated from the R package name
check.deb.availability <- function(deb=NULL, R=NULL, origin="cran", replace.dots=FALSE, action="deb"){
  if(!isUNIX()){
    stop(simpleError(paste0(action, ": this doesn't seem to be a UNIX system, so i assume it's not Debain as well!")))
  } else {}

  if(is.null(deb)){
    stopifnot(!is.null(R))
    deb <- debianPkgName(package=R, origin=origin, version=NULL, replace.dots=replace.dots)
  } else {}

  aptCache <- Sys.which("apt-cache")
  if("" %in% aptCache){
    stop(simpleError(paste0(action, ": can't find apt-cache -- are you sure you've set up everything?")))
  } else {}

  # look for the package with a regexp
  get.avails.syscall <- paste0(aptCache, " search ^", deb, "$")
  all.avails <- system(get.avails.syscall, intern=TRUE)

  if(length(all.avails) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
} ## end function check.deb.availability()


## function dl.missing.dep.src()
# a function to download R source packages of missing dependencies
# deps: character vector of R package names
# subdir/order: character and numeric, can be used to sort the downloads indow several subdirectories;
#   especially helpful if recursive=TRUE, so you know which packages must be dealt with in what order (highest numbers first)
# all: if TRUE, attempts to download all sources (even for packages which are not missing)
# previous: previous downloads by parent calls, to check if some packages are already downloaded (used in recursive loops)
dl.missing.dep.src <- function(deps, destdir=file.path(tempdir(),"roxyPackge","downloads"), subdir=deps[1], order=NULL, recursive=FALSE,
  repos=getOption("repos"), all=FALSE, check.deb=TRUE, origin="cran", origin.alt=list(), previous=list(), available=NULL, action="deb"){

  # first of all, check for a debian package, if desired, because if there is one, the rest is obsolete
  deb.packages <- previous[["deb"]]
  if(isTRUE(check.deb)){
    other.origin.names <- names(origin.alt)
    for (thisDep in deps){
      dep.lst.origin <- origin
      # check if this package should come from another origin
      if(thisDep %in% other.origin.names){
        dep.lst.origin <- origin.alt[[thisDep]]
      } else {}
      for (thisOrigin in dep.lst.origin){
        thisDebPackage <- debianPkgName(package=thisDep, origin=thisOrigin)
        is.available <- check.deb.availability(deb=thisDebPackage)
        if(isTRUE(is.available)){
          # ok, there is a debian package, add it to the list
          deb.packages <- c(deb.packages, thisDebPackage)
          names(deb.packages)[length(deb.packages)] <- thisDep
          # remove the dependency from the ones to check
          deps <- deps[!deps %in% thisDep]
          break
        } else {}
      }
    }
  } else {}
  # are we done already?
  if(length(deps) == 0 || isTRUE(nchar(deps) == 0)){
    return(list(order=order, dl.result=previous[["dl"]], deb=deb.packages))
  } else {}

  destdir.first <- destdir
  if(!is.null(subdir)){
    destdir <- file.path(destdir, subdir)
  } else {}
  if(is.numeric(order)){
    destdir <- file.path(destdir, sprintf("%03.f", order))
  } else {}

  createMissingDir(dirPath=destdir, action="deb")

  # create result and sub-result objects, appended to previous results if present
  if(is.null(available)){
    available <- available.packages(contriburl=contrib.url(repos))
  } else {}
  # check if the package is known
  unknown.packages <- !deps %in% available[,"Package"]
  if(any(unknown.packages)){
    warning(paste0(action, ": some R packages cannot be found in the configured repositories and were skipped, please check:\n  ",
      paste0(deps[unknown.packages], collapse=", ")))
    deps <- deps[!unknown.packages]
  } else {}
  dl.result <- download.packages(pkgs=deps, destdir=destdir, available=available, repos=repos)
  final.results <- rbind(previous[["dl"]], dl.result)
  if(isTRUE(recursive)){
    for (thisSrc in 1:nrow(dl.result)){
      thisPackageName <- dl.result[thisSrc,1]
      thisPackageLocation <- dl.result[thisSrc,2]
      DESC.temp <- tempfile(paste0(thisPackageName, "DESCRIPTION"), tmpdir=destdir)
      createMissingDir(dirPath=DESC.temp, action="deb", quiet=TRUE)
      on.exit(unlink(DESC.temp, recursive=TRUE))
      # look at the DESCRIPTION of each downloaded package and check Depends/Imports
      untar(thisPackageLocation, files=file.path(thisPackageName, "DESCRIPTION"), exdir=DESC.temp)
      thisDepends <- read.dcf(file.path(DESC.temp, thisPackageName, "DESCRIPTION"), fields=c("Depends","Imports"))
      thisDepends <- as.character(thisDepends[!is.na(thisDepends)])
      # debianize the names, dropping R
      this.debDeps <- debianizeDepends(thisDepends, R=NULL, collapse=NULL, drop.version=TRUE, origin=origin, origin.alt=origin.alt)
      if(length(this.debDeps) > 0 && nchar(this.debDeps) > 0){
        # run check.installed.deps()
        for (this.debDeps.num in seq_along(this.debDeps)){
          this.debDeps.R <- names(this.debDeps)[this.debDeps.num]
          this.debDeps.deb <- this.debDeps[this.debDeps.num]
          if(isTRUE(all)){
            # we shall get all sources, so this will always be FALSE no matter what
            chk.result <- FALSE
          } else {
            chk.result <- check.installed.deps(this.debDeps.deb)
          }
          # if missing, run recursively to fetch the sources
          if(!isTRUE(chk.result)){
            message(paste0(thisPackageName, " depends on ", this.debDeps.deb, "..."))
            # see if we should prefer existing debian packages
            if(isTRUE(check.deb)){
              is.available <- check.deb.availability(deb=this.debDeps.deb)
              if(isTRUE(is.available)){
                # ok, there is a debian package, add it to the list
                deb.packages <- c(deb.packages, this.debDeps.deb)
                next
              } else {}
            } else {}
            # increase "order" if not NULL
            if(is.numeric(order)){
              order <- order + 1
            } else {}
            # check if this dependency has already been downloaded
            if(!this.debDeps.R %in% final.results[,1]){
              sub.result <- dl.missing.dep.src(deps=this.debDeps.R, destdir=destdir.first, subdir=subdir, order=order,
                recursive=recursive, repos=repos, all=all, check.deb=check.deb, origin=origin, origin.alt=origin.alt,
                previous=list(dl=final.results, deb=deb.packages), available=available)
              order <- sub.result[["order"]]
              deb.packages <- sub.result[["deb"]]
              final.results <- sub.result[["dl.result"]]
            } else {
              # if we're in "order" mode, rename the download folder to the current number
              # other wise we're fine already
              if(is.numeric(order)){
                current.entry <- final.results[final.results[,1] %in% this.debDeps.R,2]
                entry.file.name <- basename(current.entry)
                entry.dir.old <- dirname(current.entry)
                entry.dir.new <- file.path(dirname(entry.dir.old), sprintf("%03.f", order))
                # just a precaution, make sure we only try to rename stuff in the very download folder
                if(isTRUE(grepl(destdir.first, entry.dir.old))){
                  rename.success <- file.rename(from=entry.dir.old, to=entry.dir.new)
                  if(isTRUE(rename.success)){
                    final.results[final.results[,1] %in% this.debDeps.R,2] <- file.path(entry.dir.new, entry.file.name)
                  }
                } else {}
              } else {}
            }
          } else {}
        }
      } else {}
    }
  } else {}
  if(!is.null(order)){
    # bring entries into order
    final.results <- final.results[order(final.results[,2]),]
  } else {}
  results <- list(order=order, dl.result=final.results, deb=deb.packages)
  return(results)
} ## end function dl.missing.dep.src()


## function check.append()
# checks if there's an "append" element in an object and returns either that or NULL (or TRUE/FALSE)
check.append <- function(dep, check="append", value=FALSE){
  if(isTRUE(check %in% names(dep[1]))){
    if(isTRUE(value)){
      result <- dep[1][[check]]
    } else {
      result <- TRUE
    }
  } else {
    if(isTRUE(value)){
      result <- NULL
    } else {
      result <- FALSE
    }
  }
  return(result)
} ## end function check.append()


## function queryDescription()
# little helper to get clean dependency info from DESCRIPTION objects
queryDescription <- function(description, dep=c("Depends")){
  description <- as.data.frame(description)
  foundDeps <- c()
  for (thisDep in dep){
    if(thisDep %in% colnames(description)){
      foundDeps <- c(foundDeps, trim(unlist(strsplit(as.character(description[[thisDep]]), ","))))
    } else {}
  }
  return(paste0(foundDeps, collapse=", "))
} ## function queryDescription()


## function deb.prepare.description()
# basic checks of the description file with ability to autogenerate some fields
# - isRpackage: if FALSE, it's assumed to become a GnuPG keyring package
deb.prepare.description <- function(deb.description=NULL, R.description=NULL, origin="cran", origin.alt=list(),
  arch="all", defaults=list(Section="gnu-r", Priority="optional"), replace.dots=FALSE,
  maintainer=NULL, isRpackage=TRUE, action=ifelse(isRpackage, "deb", "deb-key")){
  if(all(isRpackage, is.null(deb.description), is.null(R.description))){
    stop(simpleError(paste0(action, ": can't prepare dependencies, because neither R nor debian description was given!")))
  } else {}

  if(is.null(deb.description)){
    deb.description <- list()
  }

  if(isTRUE(isRpackage)){
    thisRVers <- paste(getRvers(R.homes=R.home(), win=TRUE), "0", sep=".")

    if(is.null(deb.description[["Depends"]]) || check.append(queryDescription(deb.description, dep="Depends"))){
      # generate alternative debian package dependencies
      deb.description[["Depends"]] <- debianizeDepends(
        dep=queryDescription(R.description, dep=c("Depends", "Imports")), origin=origin,
        origin.alt=origin.alt, forceRVersion=thisRVers, append=check.append(queryDescription(deb.description, dep="Depends"), value=TRUE), replace.dots=replace.dots)
    } else {
      deb.description[["Depends"]] <- queryDescription(deb.description, dep="Depends")
    }
    # if arch is "all", use Build.Depends.Indep, else Build.Depends
    build.dep.field <- ifelse(identical(arch, "all"), "Build.Depends.Indep", "Build.Depends")
    if(is.null(deb.description[[build.dep.field]]) || check.append(queryDescription(deb.description, dep=build.dep.field))){
      build.depends <- paste0("debhelper (>> 7.0.0), r-base-dev (>= ", thisRVers, "), cdbs")
      r.base.core <- gsub("r-base-core[^,]*[[:space:]]*[,]*[[:space:]]*", "", deb.description[["Depends"]])
      if(!identical(r.base.core, "")){
        build.depends <- paste(build.depends, r.base.core, sep=", ")
      } else {}
      appends <- check.append(deb.description[[build.dep.field]], value=TRUE)
      if(!is.null(appends)){
        build.depends <- paste(build.depends, paste0(appends, collapse=", "), sep=", ")
      } else {}
      deb.description[[build.dep.field]] <- build.depends
    } else {
      deb.description[[build.dep.field]] <- paste(deb.description[[build.dep.field]], collapse=", ")
    }
    if(is.null(deb.description[["Suggests"]]) || check.append(queryDescription(deb.description, dep="Suggests"))){
      if(!is.null(R.description[["Suggests"]])){
        deb.description[["Suggests"]] <- debianizeDepends(dep=queryDescription(R.description, dep="Suggests"), origin=origin,
            origin.alt=origin.alt, forceRVersion=NULL, append=check.append(queryDescription(deb.description, dep="Suggests"), value=TRUE), replace.dots=replace.dots)
      } else {
        deb.description[["Suggests"]] <- paste(check.append(queryDescription(deb.description, dep="Suggests"), value=TRUE), collapse=", ")
      }
    } else {
      deb.description[["Suggests"]] <- queryDescription(deb.description, dep="Suggests")
    }
  } else {}

  if(is.null(maintainer)){
    if(is.null(deb.description[["Maintainer"]])){
      # try to generate a half-way reasonable maintainer ID
      system.info <- Sys.info()
      system.login <- Sys.getenv("LOGNAME")
      deb.description[["Maintainer"]] <- paste0(system.info[["user"]], " <", system.login, "@", system.info[["nodename"]], ">")
      warning(paste0(action, ": you didn't specify a Debian package maintainer, it was set to \"", deb.description[["Maintainer"]], "\"!"), call.=FALSE)
    } else {}
  } else {
    deb.description[["Maintainer"]] <- maintainer
  }

  for(thisDefault in names(defaults)){
    if(is.null(deb.description[[thisDefault]])){
      deb.description[[thisDefault]] <- defaults[[thisDefault]]
    } else {}
  }

  return(deb.description)
} ## function deb.prepare.description()


## function deb.basic.checks()
#  - is this a UNIX system?
#  - can the needed tools be found?
#  - can we use the build dir or need to create a temp dir?
#  - what is the debian repo path?
#  - do we need to rename found tools (e.g., to be able to call for gpg2 still as "gpg")
deb.basic.checks <- function(
  pck.source.dir,
  repo.root,
  build.dir=tempdir(),
  actions=c("bin","src"),
  neededTools=c("dpkg-buildpackage", "fakeroot", "dpkg-source", "dpkg-genchanges",
    "apt-ftparchive", "tar", "dch", "dpkg-parsechangelog"),
  renameTools=NULL,
  repo.name=NULL,
  msg.action="deb"){
  buildTools <- NULL
  repo.deb.path <- file.path(repo.root, "deb")
  if(any(c("bin","src") %in% actions)){
    # basic checks
    # can this be a debian system at all?
    if(!isUNIX()){
      stop(simpleError(paste0(msg.action, ": this doesn't seem to be a UNIX system, so i assume it's not Debain as well!")))
    } else {}
    # are all needed programs available?
    buildTools <- checkTools(neededTools)
    # need some elements renamed?
    if(!is.null(renameTools)){
      for (thisTool in renameTools){
        if(thisTool %in% names(buildTools)){
          renameTo <- names(renameTools)[match(thisTool, renameTools)]
          names(buildTools)[match(thisTool, names(buildTools))] <- renameTo
        } else {}
      }
    } else {}
    # check for existance of build dir
    if(file_test("-d", build.dir)){
      BD.content <- list.files(build.dir, all.files=TRUE, include.dirs=TRUE, no..=TRUE)
      if(length(BD.content) > 0){
        # build dir is not empty -- create a temp dir to be sure
        build.dir <- tempfile(tmpdir=build.dir)
        if(!file_test("-d", build.dir)){
          stopifnot(dir.create(build.dir, recursive=TRUE))
          message(paste0(msg.action, ": created ", build.dir, "."))
        } else {}
      } else {}
    } else {}
  } else {}

  if(!is.null(repo.name)){
    if(identical(repo.name, "roxypackage")){
      warning(paste0(msg.action, ": you should *really* set \"repo.name\" to a custom value, unless this is just a local test!"),
        call.=FALSE
      )
    } else {}
  } else {}

  return(list(
    build.dir=build.dir,
    repo.deb.path=repo.deb.path,
    buildTools=buildTools
  ))
} ## end function deb.basic.checks()


## function deb.gen.compat()
deb.gen.compat <- function(compat=7, deb.dir, overwrite=FALSE, action="deb"){
  file <- file.path(deb.dir, "compat")
  if(!file_test("-f", file) | isTRUE(overwrite)){
    stopifnot(is.numeric(compat))
    cat(compat, "\n", file=file)
    message(paste0(action, ": created ", file, " (set to level ", compat, ")."))
  } else {}
  return(TRUE)
} ## end function deb.gen.compat()


## function deb.gen.control()
deb.gen.control <- function(srcs.name, deb.name, description, R.dscrptn, deb.dir, overwrite=TRUE, arch="all",
  fullDescription=NULL, isRpackage=TRUE, action=ifelse(isRpackage, "deb", "deb-key")){
  file <- file.path(deb.dir, "control")
  if(!file_test("-f", file) | isTRUE(overwrite)){
    deb.txt.control.src <- data.frame(
      Source=srcs.name,
      Section=description[["Section"]],
      Priority=description[["Priority"]],
      Maintainer=description[["Maintainer"]],
      stringsAsFactors=FALSE)
    # workaround, didn't manage to escape the special chars otherwise
    if("Build.Depends.Indep" %in% names(description)){
      deb.txt.control.src$`Build-Depends-Indep` <- description[["Build.Depends.Indep"]]
    } else {}
    if("Build.Depends" %in% names(description)){
      deb.txt.control.src$`Build-Depends` <- description[["Build.Depends"]]
    } else {}
    deb.txt.control.src$`Standards-Version` <- "3.9.3.1"

    if(is.null(fullDescription)){
      fullDescription <- as.character(R.dscrptn[["Description"]])
    } else {}
    if(isTRUE(isRpackage)){
      fullDescription <- paste0("GNU R package: ", fullDescription)
    } else {}

    deb.txt.control.pck <- data.frame(
      Package=deb.name,
      Architecture=arch,
      Section=description[["Section"]],
      Depends=description[["Depends"]],
      Description=fullDescription,
      stringsAsFactors=FALSE)

    # additional valid fields
    if("Homepage" %in% names(description)){
      deb.txt.control.src[["Homepage"]] <- deb.txt.control.pck[["Homepage"]] <- description[["Homepage"]]
    } else if("URL" %in% names(R.dscrptn)){
      deb.txt.control.src[["Homepage"]] <- deb.txt.control.pck[["Homepage"]] <- as.character(R.dscrptn[["URL"]])
    } else {}
    if("Essential" %in% names(description)){
      deb.txt.control.pck[["Essential"]] <- description[["Essential"]]
    } else {}
    if("Suggests" %in% names(description)){
      deb.txt.control.pck[["Suggests"]] <- description[["Suggests"]]
    } else {}
    for (theRest in names(description)[!names(description) %in% c(names(deb.txt.control.pck), names(deb.txt.control.src), "Build.Depends.Indep", "Build.Depends")]){
      deb.txt.control.pck[[theRest]] <- description[[theRest]]
    }

    # write the control file
    write.dcf(deb.txt.control.src, file=file, append=FALSE, indent=1)
    cat("\n", file=file, append=TRUE)
    write.dcf(deb.txt.control.pck, file=file, append=TRUE, indent=1)
    message(paste0(action, ": debian/control updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.control()

# this is being re-used also for the README.md file
copyright.text <- function(license, package, year, author, deb=TRUE){
  if(isTRUE(deb)){
    # different formatting needed
    id <- "  "    # indentation
    el <- "  .\n" # empty lines
  } else {
    id <- ""
    el <- "\n"
  }
  if(checkLicence(license)){
    licenseInfo <- checkLicence(license, deb=TRUE, logical=FALSE)
    licenseText <- paste0(id , package, " Copyright (C) ", year, " ", author, ", released under the\n", id,
      licenseInfo[["name"]],
      if(!is.na(licenseInfo[["version"]])){
        paste0(" version ", licenseInfo[["version"]])
      } else {},
      if(grepl(">", license)){
        paste0(" or (at your option) any later version")
      } else {},
      ".\n", el,
       id, "This software is distributed in the hope that it will be useful, but\n",
       id, "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n",
       id, "or FITNESS FOR A PARTICULAR PURPOSE.\n", el,
      if(isTRUE(deb)){
        paste0(
           id, "You should have received a copy of the license with your Debian system,\n",
           id, "in the file /usr/share/common-licenses/", licenseInfo[["file"]], ", or with the\n",
           id, "source package as the file COPYING or LICENSE.\n"
        )
      } else {
        paste0(
           id, "You should have received a copy of the license with the\n",
           id, "source package as the file COPYING or LICENSE.\n"
        )
      }
    )
  } else {
    licenseText <- paste0( id, package, " Copyright (C) ", year, " ", author, ", released under the\n",
       id, "terms of the ", license, " license.\n", el,
       id, "This software is distributed in the hope that it will be useful, but\n",
       id, "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n",
       id, "or FITNESS FOR A PARTICULAR PURPOSE.\n", el,
       id, "You should have received a copy of the license with the\n",
       id, "source package as the file COPYING or LICENSE.\n"
    )
  }
  return(licenseText)
}

## function deb.gen.copyright()
deb.gen.copyright <- function(R.dscrptn, deb.name, description, year, deb.dir,
  overwrite=TRUE, repo.name=NULL, isRpackage=TRUE, action=ifelse(isRpackage, "deb", "deb-key"),
  URL=NULL){
  file <- file.path(deb.dir, "copyright")
  if(is.null(R.dscrptn[["Authors@R"]])){
    maintainer <- as.character(R.dscrptn[["Maintainer"]])
    author <- as.character(R.dscrptn[["Author"]])
    author.nomail <- gsub("[[:space:]]*<[^>]*>", "", author)
  } else {
    pck.persons <- as.character(R.dscrptn[["Authors@R"]])
    maintainer <- paste(
      format(get.by.role(eval(parse(text=pck.persons)), "cre"),
        include=c("given", "family", "email"), braces=list(email=c("<", ">"))),
      collapse=", "
    )
    author <- paste(
      format(get.by.role(eval(parse(text=pck.persons)), "aut"),
        include=c("given", "family", "email"), braces=list(email=c("<", ">"))),
      collapse=", "
    )
    author.nomail <- paste(
      format(get.by.role(eval(parse(text=pck.persons)), "aut"),
        include=c("given", "family")),
      collapse=", "
    )
  }
  license <- as.character(R.dscrptn[["License"]])
  R.name <- as.character(R.dscrptn[["Package"]])

  if(!file_test("-f", file) | isTRUE(overwrite)){
    includeLicense <- copyright.text(
      license=license,
      package=R.name,
      year=year,
      author=author.nomail,
      deb=TRUE
    )
    if(isTRUE(isRpackage)){
      txt.copyright <- paste0(
        "Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n",
        "Upstream-Name: ", R.name, "\n",
        "Upstream-Contact: ", maintainer, "\n",
        if("Homepage" %in% names(description)){
          paste0("Source: ", description[["Homepage"]], "\n")
        } else {},
        "Comment:\n",
        if(identical(author, maintainer)){
          paste0("  The R library ", R.name, " was originally written and is maintained by ", author, ".\n  .\n")
        } else {
          paste0("  The R library ", R.name, " was originally written by ", author, "\n",
          "  and is maintained by ", maintainer, ".\n  .\n")
        },
        "  This Debian package was put together by ", maintainer, ",\n",
        "  using the GNU R package roxyPackage.\n  .\n",
        "  The package was renamed from its upstream name '" ,R.name, "' to\n",
        "  '" , deb.name, "' in harmony with the R packaging policy to indicate\n",
        "  that the package is external to the CRAN or BioC repositories.\n  .\n",
        "  The copyright information given here was offered by the GNU R package\n",
        "  DESCRIPTION. The DESCRIPTION file for the original GNU R package can be\n",
        "  found in /usr/lib/R/site-library/", R.name, "/DESCRIPTION.\n\n",
        "Files: *\n",
        "Copyright: ", year, " ", author.nomail, "\n",
        "License: ", license, "\n",
        includeLicense
      )
    } else {
      txt.copyright <- paste0(
        "Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n",
        "Upstream-Name: ", R.name, "\n",
        "Upstream-Contact: ", maintainer, "\n",
        if(!is.null(URL)){
          paste0("Source: ", URL, "\n")
        } else {},
        "Comment:\n",
        "  This is the OpenPGP keyring for packages hosted at the\n  ", repo.name, " repository",
        if(is.null(URL)){
          paste0(".\n  .\n")
        } else {
          paste0(": ", URL, "\n  .\n")
        },
        "  This Debian package was put together by ", description[["Maintainer"]], ".\n  .\n",
        "  The keys in the keyring don't fall under any copyright. Everything\n",
        "  else in the package is licensed as follows.\n\n",
        "Files: *\n",
        "Copyright: ", year, " ", author.nomail, "\n",
        "License: ", license, "\n",
        includeLicense,
        "\nFiles: keyrings/*.gpg\n",
        "Copyright: public-domain\n",
        "License: public-domain\n"
      )
    }

    # write the copyright file
    cat(txt.copyright, file=file)
    message(paste0(action, ": debian/copyright updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.copyright()


## function deb.gen.rules()
deb.gen.rules <- function(deb.name, maintainer, year, origin, deb.dir, overwrite=TRUE,
  isRpackage=TRUE, keyringFiles=NULL, action=ifelse(isRpackage, "deb", "deb-key")){
  file <- file.path(deb.dir, "rules")
  if(!file_test("-f", file) | isTRUE(overwrite)){
    if(isTRUE(isRpackage)){
      txt.rules <- paste0(
        "#!/usr/bin/make -f\n",
        "# -*- makefile -*-\n",
        "# debian/rules file for the Debian/GNU Linux ", deb.name," package\n",
        "# Copyright ", year, " by ", maintainer, "\n\n",
        "debRreposname := ", origin, "\n\n",
        "include /usr/share/R/debian/r-cran.mk\n"
      )
    } else {
      txt.rules <- paste0(
        "#!/usr/bin/make -f\n",
        "# -*- makefile -*-\n",
        "# debian/rules file for the Debian/GNU Linux ", deb.name," package\n",
        "# Copyright ", year, " by ", maintainer, "\n\n",
        "%:\n",
        "\tdh $@\n\n",
        "override_dh_install:\n",
        paste0("\tdh_install ", keyringFiles, " usr/share/keyrings/", collapse="\n"), "\n\n",
        "override_dh_builddeb:\n",
        "\tdh_builddeb -- -Zxz\n"
      )
    }

    # write the rules file
    cat(txt.rules, file=file)
    # set executable permissions
    Sys.chmod(file, mode="0755")
    message(paste0(action, ": debian/rules updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.rules()


## function deb.append.clog()
# use this function to append changelog entries individually
deb.append.clog <- function(logs, file, deb.env, dch=Sys.which("dch"), dpkg.parsechangelog=Sys.which("dpkg-parsechangelog")){
  for(thisLog in logs){
    # first check whether the entry already exists
    current.clog <- system(paste0(dpkg.parsechangelog, " -l", file, " --count1 -SChanges"), intern=TRUE)
    items <- findItemsInChangeLog(current.clog, item="[[:space:]]*\\*")
    if(!thisLog %in% items){
      append.changelog <- paste0(dch, " --changelog=\"", file,
        "\" --append \"", thisLog, "\"")
      system(paste0(deb.env, append.changelog), intern=TRUE)
    } else {}
  }
  return(invisible(TRUE))
} ## end function deb.append.clog()


## function deb.gen.changelog()
# export DEBFULLNAME="<name>" ; export DEBEMAIL="name@example.com" ; dch --create --empty --changelog <file> --package <srcs.name> --newversion="0.0X-XX-X" --distribution <distribution> --urgency <urgency>
# export DEBFULLNAME="<name>" ; export DEBEMAIL="name@example.com" ; dch --newversion="0.0X-XX-X" "<text>"
# export DEBFULLNAME="<name>" ; export DEBEMAIL="name@example.com" ; dch --append "<text>"
deb.gen.changelog <- function(srcs.name, version, maintainer, logs, distribution, urgency, deb.dir,
  dch=Sys.which("dch"), dpkg.parsechangelog=Sys.which("dpkg-parsechangelog"), overwrite=TRUE, action="deb"){
  # set environment variables for the developer
  # 'maintainer' should be in the form of "firstname name <e@mail.foo>"
  deb.env <- paste0("export DEBEMAIL=\"", maintainer, "\" ; ")
  file <- file.path(deb.dir, "changelog")

  if(!file_test("-f", file)){
    create.changelog <- paste0(dch, " --create --empty --changelog=\"", file,
      "\" --package=\"", srcs.name,
      "\" --newversion=\"", version,
      "\" --distribution=\"", distribution,
      "\" --urgency=\"", urgency, "\" ", logs[1])
    system(paste0(deb.env, create.changelog), intern=TRUE)
    if(length(logs) > 1){
      deb.append.clog(logs=logs[-1], file=file, deb.env=deb.env, dch=dch)
    } else {}
    message(paste0(action, ": created initial debian/changelog."))
  } else if(isTRUE(overwrite)){
    current.clog.version <- package_version(system(paste0(dpkg.parsechangelog, " -l", file, " --count1 -SVersion"), intern=TRUE))
    if(identical(current.clog.version, package_version(version))){
      deb.append.clog(logs=logs, file=file, deb.env=deb.env, dch=dch)
    } else {
      newversion.changelog <- paste0(dch, " --changelog=\"", file,
        "\" --package=\"", srcs.name,
        "\" --newversion=\"", version,
        "\" --distribution=\"", distribution,
        "\" --urgency=\"", urgency, "\" ", logs[1])
      system(paste0(deb.env, newversion.changelog), intern=TRUE)
      if(length(logs) > 1){
        deb.append.clog(logs=logs[-1], file=file, deb.env=deb.env, dch=dch)
      } else {}
    }
    message(paste0(action, ": debian/changelog updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.changelog()


## function deb.gen.format()
deb.gen.format <- function(deb.dir, action="deb"){
  deb.dir.source <- file.path(deb.dir, "source")
  deb.file.format <- file.path(deb.dir.source, "format")
  if(!file_test("-d", deb.dir.source)){
    stopifnot(dir.create(deb.dir.source, recursive=TRUE))
    message(paste0("deb: created ", deb.dir.source, "."))
  } else {}
  if(!file_test("-f", deb.file.format)){
    cat("3.0 (quilt)\n", file=deb.file.format)
    message(paste0(action, ": created ", deb.file.format, " (set to quilt format)."))
  } else {}
  return(TRUE)
} ## end function deb.gen.format()


## function deb.gen.postinst()
deb.gen.postinst <- function(deb.dir, keyringFiles, action="deb-key", overwrite=FALSE){
  deb.file.postinst <- file.path(deb.dir, "postinst")
  if(!file_test("-f", deb.file.postinst) | isTRUE(overwrite)){
    postinst.txt <- paste0(
      "#!/bin/sh\n\n",
      "set -e\n\n",
      "case \"$1\" in\n",
      "\tconfigure)\n",
      "\t\tif [ -x /usr/bin/apt-key ]; then\n",
      paste0("\t\t\tapt-key add /usr/share/keyrings/", keyringFiles, collapse="\n"), "\n",
      "\t\tfi\n",
      "\t\t;;\n\n",
      "\tabort-upgrade|abort-remove|abort-deconfigure)\n",
      "\t\t;;\n\n",
      "\t*)\n",
      "\t\techo \"postinst called with unknown argument \\\"$1\\\"\" >&2\n",
      "\t\texit 1\n",
      "\t\t;;\n",
      "esac\n\n",
      "# dh_installdeb might auto-include code here:\n\n",
      "#DEBHELPER#\n\n",
      "exit 0\n"
    )
    cat(postinst.txt, file=deb.file.postinst)
    message(paste0(action, ": debian/postinst updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.postinst()


## function deb.gen.prerm()
deb.gen.prerm <- function(deb.dir, key, action="deb-key", overwrite=FALSE){
  deb.file.prerm <- file.path(deb.dir, "prerm")
  if(!file_test("-f", deb.file.prerm) | isTRUE(overwrite)){
    prerm.txt <- paste0(
      "#!/bin/sh\n\n",
      "set -e\n\n",
      "case \"$1\" in\n",
      "\tremove|purge)\n",
      "\t\tif [ -x /usr/bin/apt-key ]; then\n",
      paste0("\t\t\tif [ ! -z \"$(apt-key list | grep ", key, " 2>/dev/null)\" ]; then\n",
      "\t\t\t\tapt-key del ", key, "\n",
      "\t\t\tfi", collapse="\n"), "\n",
      "\t\tfi\n",
      "\t\t;;\n\n",
      "\tupgrade|deconfigure)\n",
      "\t\t;;\n\n",
      "\tfailed-upgrade)\n",
      "\t\t;;\n\n",
      "\t*)\n",
      "\t\techo \"prerm called with unknown argument \\\"$1\\\"\" >&2\n",
      "\t\texit 1\n",
      "\t\t;;\n",
      "esac\n\n",
      "# dh_installdeb might auto-include code here:\n\n",
      "#DEBHELPER#\n\n",
      "exit 0\n"
    )
    cat(prerm.txt, file=deb.file.prerm)
    message(paste0(action, ": debian/prerm updated."))
  } else {}
  return(TRUE)
} ## end function deb.gen.prerm()


## function last.dir.name()
last.dir.name <- function(path){
  path.parts <- unlist(strsplit(path, .Platform$file.sep))
  last.name <- path.parts[length(path.parts)]
  return(last.name)
} ## end function last.dir.name()


## function deb.prepare.buildDir()
deb.prepare.buildDir <- function(source, build, tar=Sys.which("tar")){
  old.wd <- getwd()
  on.exit(setwd(old.wd))
  setwd(file.path(source, ".."))
  # copy the source files to build dir
  if(!identical(build, source)){
    # unfortunately, file.copy() cannot exclude patterns, so we'll circumvent this by using tar
    tmp.tar.dest <- file.path(build, "energize.tar")
    tar(tmp.tar.dest, files=last.dir.name(source), tar=tar,
      compression="none", extra_flags="-h --exclude=*\\~ --exclude-vcs")
    setwd(build)
    untar("energize.tar")
    stopifnot(file.remove("energize.tar"))
  } else {}
} ## function deb.prepare.buildDir()


## function deb.gen.package.index()
# updates Packages/Sources files after changes were done to a repository
deb.gen.package.index <- function(repo, binary=TRUE, distribution="unstable", component="main", arch="all",
  repo.all.arch=c("binary-i386","binary-amd64"), apt.ftparchive=Sys.which("apt-ftparchive")){
  prev.wd <- getwd()
  setwd(file.path(repo))
  if(isTRUE(binary)){
    af.command <- " packages "
    af.file <- "Packages"
    repo.rel.path <- repo.rel.pseudo.path <- file.path("dists", distribution, component, "all")
    repo.arch.rel.paths <- file.path("dists", distribution, component, repo.all.arch)
    repo.arch.paths <- file.path(repo, repo.arch.rel.paths)
  } else {
    af.command <- " sources "
    af.file <- "Sources"
    repo.rel.path <- file.path("dists", distribution, component, "source")
    repo.rel.pseudo.path <- file.path("source", distribution)
    repo.real.path <- file.path(repo, repo.rel.pseudo.path)
  }
  # update packages/sources information; paths must be relative to the debian repo root
  dpkg.scan.call <- paste0(apt.ftparchive, af.command, repo.rel.pseudo.path, " > ", repo.rel.path, "/", af.file, " && \\\n",
  "cat ", repo.rel.path, "/", af.file, " | gzip -9 > ", repo.rel.path, "/", af.file, ".gz && \\\n",
  "cat ", repo.rel.path, "/", af.file, " | bzip2 -9 > ", repo.rel.path, "/", af.file, ".bz2")
  system(dpkg.scan.call, intern=TRUE)
  if(isTRUE(binary)){
    for (this.path in c(repo.arch.paths)){
      repo.all.pckgs.files <- c("Packages", "Packages.gz", "Packages.bz2")
      file.copy(file.path(repo.rel.path, repo.all.pckgs.files), file.path(this.path, repo.all.pckgs.files), overwrite=TRUE)
    }
  } else {}
  setwd(prev.wd)
} ## end function deb.gen.package.index()


## function deb.build.sources()
# - compression: either "xz" or "gzip"
deb.build.sources <- function(srcs.name, build, src.dir.name, version,
  repo, distribution="unstable", component="main", compression="xz", keep.existing.orig=FALSE,
  tar=Sys.which("tar"), dpkg.source=Sys.which("dpkg-source"), apt.ftparchive=Sys.which("apt-ftparchive"),
  action="deb"){

    if(!compression %in% c("xz", "gzip")){
      stop(simpleError(paste0(action, ": unknown compression \"", compression, "\"!")))
    } else {}

    repo.src.pseudo.rel.path <- file.path("dists", distribution, component, "source")
    repo.src.pseudo.path <- file.path(repo, repo.src.pseudo.rel.path)
    repo.src.real.rel.path <- file.path("source", distribution)
    repo.src.real.path <- file.path(repo, repo.src.real.rel.path)

    for (this.path in c(repo.src.pseudo.path, repo.src.real.path)){
      if(!file_test("-d", this.path)){
        stopifnot(dir.create(this.path, recursive=TRUE))
        message(paste0(action, ": created ", this.path, " (repository)."))
      } else {}
    }

    prev.wd <- getwd()
    # first clean up
    setwd(file.path(build, src.dir.name))
    system("fakeroot debian/rules clean")
    setwd(file.path(build))
    if(identical(compression, "xz")){
      orig.file.name <- paste0(srcs.name, "_", version, ".orig.tar.xz")
    } else {
      orig.file.name <- paste0(srcs.name, "_", version, ".orig.tar.gz")
    }
    if(isTRUE(keep.existing.orig) & file.exists(file.path(repo.src.real.path, orig.file.name))){
      message(paste0(action, ": keeping existing *.orig.tar.[gz|xz] file."))
      # copy existing file over for dpkg-source
      file.copy(file.path(repo.src.real.path, orig.file.name), ".", overwrite=TRUE)
      system(paste0(dpkg.source, " -Z", compression, " -b ", src.dir.name))
      src.files.to.move <- list.files(pattern="*.dsc$|*.debian.tar.gz$|*.debian.tar.xz$")
      file.copy(src.files.to.move, file.path(repo.src.real.path, src.files.to.move), overwrite=TRUE)
      message(paste0(action, ": copied *.dsc and *.debian.tar.[gz|xz] files to debian source repository."))
    } else {
      # --exclude-vcs doesn't seem to work :-/
      tar.extraFlags <- excludeVCSDirs(
        src=file.path(build, src.dir.name),
        exclude.dirs=c(".svn", "CVS", ".git", "_darcs", ".hg"),
        action="deb", target="*.orig.tar.[gz|xz]"
      )
      tar(orig.file.name, files=src.dir.name, tar=tar,
        compression=compression, extra_flags=paste0("-h --exclude=", src.dir.name, "/debian --exclude=*\\~ ", tar.extraFlags))
      system(paste0(dpkg.source, " -Z", compression, " -b ", src.dir.name))
      src.files.to.move <- list.files(pattern="*.dsc$|*.debian.tar.gz$|*.orig.tar.gz$|*.debian.tar.xz$|*.orig.tar.xz$")
      file.copy(src.files.to.move, file.path(repo.src.real.path, src.files.to.move), overwrite=TRUE)
      message(paste0(action, ": copied *.dsc, *.orig.tar.[gz|xz] and *.debian.tar.[gz|xz] files to debian source repository."))
    }
    # update sources information
    deb.gen.package.index(
      repo=repo, binary=FALSE, distribution=distribution, component=component, apt.ftparchive=apt.ftparchive
    )
    setwd(prev.wd)
} ## end function deb.build.sources()


## function deb.build.binary()
deb.build.binary <- function(deb.name, build, src.dir.name, options, version, repo,
  revision=1, distribution="unstable", component="main", arch="all", repo.all.arch=c("binary-i386","binary-amd64"),
  dpkg.buildpackage=Sys.which("dpkg-buildpackage"), dpkg.genchanges=Sys.which("dpkg-genchanges"),
  apt.ftparchive=Sys.which("apt-ftparchive"), deb.name.lower=deb.name, action="deb"){

  repo.arch.rel.paths <- file.path("dists", distribution, component, repo.all.arch)
  repo.arch.paths <- file.path(repo, repo.arch.rel.paths)
  repo.bin.rel.path <- file.path("dists", distribution, component, "all")
  repo.bin.path <- file.path(repo, repo.bin.rel.path)

  for (this.path in c(repo.bin.path, repo.arch.paths)){
    if(!file_test("-d", this.path)){
      stopifnot(dir.create(this.path, recursive=TRUE))
      message(paste0(action, ": created ", this.path, " (repository)."))
    } else {}
  }

  prev.wd <- getwd()
  dpkg.build.call <- paste0(dpkg.buildpackage, " ", options)
  dpkg.gench.call <- paste0(dpkg.genchanges, " -b > ../", deb.name, "_", version, "-", revision, "_", arch, ".changes")
  bin.build.dir <- file.path(build, src.dir.name)
  setwd(bin.build.dir)
  # work around probably buggy r-cran.mk by creating a symlink
  if(!identical(deb.name.lower, deb.name)){
    setwd(file.path(bin.build.dir, "debian"))
    system(paste0("ln -s ", deb.name, " ", deb.name.lower), intern=TRUE)
    setwd(bin.build.dir)
    message(paste0(action, ": created workaround symlink for r-cran.mk."))
  } else {}
  system(dpkg.build.call, intern=TRUE)
  system(dpkg.gench.call, intern=TRUE)
  # copy built files
  setwd(file.path(build))
  bin.files.to.move <- list.files(pattern="*.changes$|*.deb$")
  file.copy(bin.files.to.move, file.path(repo.bin.path, bin.files.to.move))
  message(paste0(action, ": copied *.changes and *.deb files to debian binary repository."))
  # update package information
  deb.gen.package.index(
    repo=repo, binary=TRUE, distribution=distribution, component=component, arch=arch,
    repo.all.arch=repo.all.arch, apt.ftparchive=apt.ftparchive
  )
  setwd(prev.wd)
} ## end function deb.build.binary()


## function GPGversion()
GPGversion <- function(key=NULL, version=2){
  result <- NULL
  if(!is.null(key)){
    if(is.null(version) || identical(as.numeric(version), 1)){
      result <- "gpg"
    } else {
      result <- paste0("gpg", version)
    }
  } else {}
  return(result)
} ## end function GPGversion()


## function GPGwriteKey()
GPGwriteKey <- function(key, file, gpg=Sys.which("gpg"), overwrite=FALSE, keyring=NULL, action="deb"){
  if(!is.null(keyring)){
    add.options <- paste0(" --keyring ", keyring)
  } else {
    add.options <- NULL
  }
  if(file_test("-f", file)){
    if(isTRUE(overwrite)){
      add.options <- paste0(add.options, " --yes")
    } else {
      message(paste0(action, ": OpenPGP key file already present, no overwrite demanded: kept as-is."))
      return(invisible(NULL))
    }
  } else {}
  
  gpg.copy.call <- paste0(gpg, add.options, " --armor --output ", file, " --export ", key)
  system(gpg.copy.call, intern=TRUE)
  message(paste0(action, ": updated OpenPGP key file: ", key))
} ## end function GPGwriteKey()


## function GPGsign()
GPGsign <- function(key, fileToSign, signatureFile, gpg=Sys.which("gpg"), keyring=NULL){
  if(!is.null(keyring)){
    add.options <- paste0(" --keyring ", keyring)
  } else {
    add.options <- NULL
  }
  # --no-tty --yes is mandatory, otherwise gpg will stop with an error
  # because it will try to get password information from /dev/tty and/or
  # ask if files should be re-signed
  gpg.sign.call <- paste0(gpg, add.options, " --no-tty --yes --default-key ", key,
    " -abs -o ", signatureFile, " ", fileToSign)
  system(gpg.sign.call, intern=TRUE)
} ## end function GPGsign()


## function deb.keyring.in.repo()
deb.keyring.in.repo <- function(repo.root, gpg.key=NULL, keyring.options=NULL,
  gpg=Sys.which("gpg"), overwrite=FALSE, keyring=NULL, action="deb-key"){
    if(!is.null(keyring.options)){
      writeKey <- FALSE
      if(isTRUE(overwrite)){
        writeKey <- TRUE
      } else {
        # is the keyring there?
        repo.name <- keyring.options[["repo.name"]]
        keyname <- eval(keyring.options[["keyname"]])
        if(!is.null(keyname)){
        message(paste0(action, ": look for OpenPGP key in Debian repository."))
          keyInRepo <- deb.search.repo(
            pckg=keyname,
            repo=file.path(repo.root, "deb"),
            distribution=keyring.options[["distribution"]],
            component=keyring.options[["component"]],
            arch="all",
            boolean=FALSE)
          if(!is.null(keyInRepo)){
            message(paste0(action, ": found key in repo, check version."))
            KeyVersions <- package_version(keyInRepo[["Version"]])
            message(paste0(action, ": found following version(s): ", paste0(KeyVersions, collapse=", ")))
            versionInQuestion <- package_version(paste0(keyring.options[["version"]], "-", keyring.options[["revision"]]))
            if(!all(KeyVersions >= versionInQuestion)){
              message(paste0(action, ": ok, will build new version: ", versionInQuestion))
              writeKey <- TRUE
            } else {
              message(paste0(action, ": key already in repo: skipping."))
            }
          } else {
            message(paste0(action, ": key not in repo yet, build keyring package."))
            writeKey <- TRUE
          }
        } else {}
      }
      if(isTRUE(writeKey)){
        # invoke debianization of the keyring
        formals(debianizeKeyring) <- keyring.options
        debianizeKeyring()
      } else {}
    } else {
      # fall back to single key file?
      repo.gpg.key.file <- file.path(repo.root, paste0(gpg.key, ".asc"))
      GPGwriteKey(
        key=gpg.key,
        file=repo.gpg.key.file,
        gpg=gpg,
        overwrite=overwrite,
        keyring=keyring)
    }
} ## end function deb.keyring.in.repo()


## function deb.update.release()
deb.update.release <- function(repo.root, repo=file.path(repo.root, "deb"), gpg.key=NULL,
  distribution="unstable", component="main", arch=c("i386", "amd64", "source"),
  apt.ftparchive=Sys.which("apt-ftparchive"), gpg=Sys.which("gpg"), keyring=NULL, action="deb"){
  repo.release.path <- file.path(repo, "dists", distribution)
  prev.wd <- getwd()
  setwd(file.path(repo))
  dpkg.relse.call <- paste0(apt.ftparchive,
    # Origin, Label, Suite, Version, Codename, Date, Valid-Until, Architectures, Components, Description
    "\\\n  -o=APT::FTPArchive::Release::Suite=\"", distribution, "\"",
    "\\\n  -o=APT::FTPArchive::Release::Components=\"", component, "\"",
    "\\\n  -o=APT::FTPArchive::Release::Architectures=\"", paste0(arch, collapse=" "), "\"",
    "\\\n  release ", repo.release.path, " > ", repo.release.path, "/Release")
  system(dpkg.relse.call, intern=TRUE)
  # sign release file
  if(!is.null(gpg.key)){
    GPGsign(
      key=gpg.key,
      fileToSign=file.path(repo.release.path, "Release"),
      signatureFile=file.path(repo.release.path, "Release.gpg"),
      gpg=gpg,
      keyring=keyring)
    message(paste0(action, ": signed Release file with key ", gpg.key, "."))
  } else {}
  setwd(prev.wd)
} ## end function deb.update.release()


## function deb.explode.sources()
# takes the "Files" column from debian Sources files and
# hacks it into separate columns for the respective dsc, orig and debian files
deb.explode.sources <- function(sources, column="Files"){
  if(nrow(sources) < 1){
    return(invisible(NULL))
  } else {}
  sources <- as.data.frame(sources, stringsAsFactors=FALSE)
  for (thisEntry in 1:nrow(sources)){
    # each row in one element
    thisFiles <- unlist(strsplit(trim(sources[thisEntry,column]), "\n"))
    # now split each row
    for(thisRow in thisFiles){
      if(grepl("*.dsc$", thisRow)){
        newRowName <- "Files.dsc"
      } else if(grepl("*.orig\\.*", thisRow)){
        newRowName <- "Files.orig"
      } else if(grepl("*.debian\\.*", thisRow)){
        newRowName <- "Files.debian"
      } else {
        newRowName <- "Files.unknown"
      }
      splitRows <- unlist(strsplit(trim(thisRow), "[[:space:]]"))
      # add directory path
      splitRows[3] <- file.path(sources[thisEntry,"Directory"], splitRows[3])
      newRowNames <- paste(newRowName, c("hash", "size", "name"), sep=".")
      names(splitRows) <- newRowNames
      for (thisRowName in newRowNames){
        sources[thisEntry,thisRowName] <- splitRows[thisRowName]
      }
    }
  }
  return(sources)
} ## end function deb.explode.sources()


## function deb.check.changes()
# takes a Packages data.frame and searches for respective *.changes files of each package
deb.check.changes <- function(packages, repo.root, column="Filename"){
  if(nrow(packages) < 1){
    return(invisible(NULL))
  } else {}
  packages <- as.data.frame(packages, stringsAsFactors=FALSE)
  allPackageFiles <- packages[[column]]
  nameStubs <- gsub("_[[:alnum:]]*.deb", "", allPackageFiles)
  for (thisStubNum in seq_along(nameStubs)){
    thisStub <- nameStubs[thisStubNum]
    directory <- gsub("/[^/]*$", "", thisStub)
    fileRoot <- file.path(repo.root, directory)
    fileName <- gsub("^.*/([^/]*)$", "\\1", thisStub, perl=TRUE)
    foundChanges <- list.files(path=fileRoot, pattern=paste0("*", fileName, ".*changes"), full.names=FALSE)
    for (thisChanges in foundChanges){
      # there might be multiple changes files for different architectures
      arch <- gsub("\\.changes$", "", gsub(paste0("*", fileName, "_"), "", thisChanges))
      packages[thisStubNum, paste0("changes.", arch)] <- file.path(directory, thisChanges)
    }
  }
  return(packages)
} ## end function deb.check.changes()


## function deb.list.packages.dirs()
# takes the root of a debian repository and returns all directories which
# contain "Packages" or "Sources" files
# - repo: root of the *debian* repository
# - binary: logical, whether to look our for "Packages" (TRUE) or "Sources" (FALSE) files
deb.list.packages.dirs <- function(repo, binary=TRUE, fullPath=FALSE){
  if(isTRUE(binary)){
    lookFor <- "Packages[.[:alnum:]]*$"
  } else {
    lookFor <- "Sources[.[:alnum:]]*$"
  }
  results <- list.files(repo, pattern=paste0("^", lookFor), recursive=TRUE, full.names=fullPath)
  results <- unique(gsub(paste0(.Platform$file.sep, lookFor), "", results))
  return(results)
} ## end function deb.list.packages.dirs()


## function deb.search.repo()
# searches for packages in a debian repository
# - pckg: a package name without version string etc.; if NULL, all packages are returned (well, unique)
# - repo: root of the *debian* repository
# - boolean: if FALSE, returns the matching entries, if any
# - binary: if TRUE, lists *.db, if FLASE, lists sources
# - checkChanges: Packages don't list *.changes files; if TRUE, add them to the output
# - searchPath: path starting with the "dists" folder and ending with the folder containing
#     either "Packages" or "Sources" files to examine. if NULL, will be constructed from
#     distribution, component and arch
deb.search.repo <- function(pckg=NULL, repo, distribution="unstable", component="main",
  arch="all", boolean=TRUE, binary=TRUE, checkChanges=FALSE, searchPath=NULL, action="deb"){
  if(isTRUE(binary)){
    origin <- "Packages"
    if(is.null(searchPath)){
      searchPath <- file.path(repo, "dists", distribution, component, arch)
    } else {
      searchPath <- file.path(repo, searchPath)
    }
    thirdOrder <- "Filename"
  } else {
    origin <- "Sources"
    if(is.null(searchPath)){
      searchPath <- file.path(repo, "dists", distribution, component, "source")
    } else {
      searchPath <- file.path(repo, searchPath)
    }
    thirdOrder <- "Files"
  }
  if(file.exists(file.path(searchPath, origin))){
    PackagesFile <- file(file.path(searchPath, origin), open="r")
  } else if(file.exists(file.path(searchPath, paste0(origin, ".gz")))){
    PackagesFile <- gzfile(file.path(searchPath, paste0(origin, ".gz")), open="r")
  } else if(file.exists(file.path(searchPath, paste0(origin, ".bz2")))){
    PackagesFile <- bzfile(file.path(searchPath, paste0(origin, ".bz2")), open="r")
  } else {
    stop(simpleError(paste0(action, ": argh, no ", origin, "[.gz|.bz2] file found in repo!")))
  }
  # read available info on packages
  PackagesContent <- as.data.frame(read.dcf(PackagesFile), stringsAsFactors=FALSE)
  close(PackagesFile)
  if(is.null(pckg)){
    foundEntries <- rep(TRUE, nrow(PackagesContent))
  } else {
    foundEntries <- PackagesContent[,"Package"] %in% pckg
  }
  if(isTRUE(boolean)){
    return(any(foundEntries))
  } else {
    if(any(foundEntries)){
      result <- unique(PackagesContent[foundEntries,])
      if(isTRUE(binary)){
        if(isTRUE(checkChanges)){
          result <- deb.check.changes(packages=result, repo.root=repo, column="Filename")
        } else {}
      } else {
        result <- deb.explode.sources(result)
      }
      result <- result[order(
        result[,"Package"],
        result[,"Version"],
        result[,thirdOrder],
        decreasing=TRUE),]
    } else {
      result <- NULL
    }
  }
  return(result)
} ## end function deb.search.repo()

## function deb.archive.packages()
# behaves similar to archive.packages() -- and is indeed called by it --, but specialises
# on debian binary and source packages. since this is like a repo-in-a-repo, the functionality
# is outsourced to a function of its own.
# return value is logical to indicate if any packages were moved/deleted
deb.archive.packages <- function(repo.root, to.dir="Archive", keep.versions=1, keep.revisions=2, package=NULL,
  archive.root=repo.root, overwrite=FALSE, reallyDoIt=FALSE, justDelete=FALSE, graceful=FALSE){
  didArchiveSomething <- FALSE
  # a specialty is that we need to take care of revisions: there might be several revisions,
  # but only *one* source.orig tarball!
  # also, we must check *everything* below the "dists" directory

  # append "deb" directory to archive path
  to.dir <- file.path(to.dir, "deb")
  
  # plan: run deb.list.packages.dirs() to discover dirs containig "Packages*" or "Sources*" files
  packageDirsBin <- deb.list.packages.dirs(repo=repo.root, binary=TRUE)
  packageDirsSrc <- deb.list.packages.dirs(repo=repo.root, binary=FALSE)

  # and run them through deb.search.repo()
  packagesBinAllDirs <- deb.search.repo(pckg=package, repo=repo.root,
    boolean=FALSE, binary=TRUE, checkChanges=TRUE, searchPath=packageDirsBin[1], action="deb")
  if(length(packageDirsBin) > 1){
    for (thisBinDir in 2:length(packageDirsBin)){
      packagesBinAllDirs <- rbind(
        packagesBinAllDirs,
        deb.search.repo(pckg=package, repo=repo.root,
          boolean=FALSE, binary=TRUE, checkChanges=TRUE, searchPath=packageDirsBin[thisBinDir], action="deb")
      )
    }
  }
  packagesBinAllDirs <- unique(packagesBinAllDirs)
  # sources
  packagesSrcAllDirs <- deb.search.repo(pckg=package, repo=repo.root,
    boolean=FALSE, binary=FALSE, checkChanges=TRUE, searchPath=packageDirsSrc[1], action="deb")
  if(length(packageDirsSrc) > 1){
    for (thisSrcDir in 2:length(packageDirsSrc)){
      packagesSrcAllDirs <- rbind(
        packagesSrcAllDirs,
        deb.search.repo(pckg=package, repo=repo.root,
          boolean=FALSE, binary=FALSE, checkChanges=TRUE, searchPath=packageDirsSrc[thisSrcDir], action="deb")
      )
    }
  }
  packagesSrcAllDirs <- unique(packagesSrcAllDirs)

  for (thisPackages in list(packagesBinAllDirs, packagesSrcAllDirs)){
    # split version and revision
    thisPackages[["FullVersion"]] <- thisPackages[["Version"]]
    thisPackages[["Version"]] <- gsub("-[[:digit:]]+$", "", thisPackages[["FullVersion"]])
    thisPackages[["Revision"]] <- gsub("(^.*)-([[:digit:]]+)$", "\\2", thisPackages[["FullVersion"]], perl=TRUE)
    
    # now iterate through found package names, check respective versions and move files, if neccessary
    for (this.package in unique(thisPackages[,"Package"])){
      presentPackages <- archiveSubset(data=thisPackages, var="Package", values=this.package)

      presentVersions <- unique(presentPackages[,"Version"])
      presentVersions <- presentVersions[order(package_version(presentVersions), decreasing=TRUE)]
      if(keep.versions > 0){
        keepVersions <- presentVersions[1:keep.versions]
        keepFullVersions <- c()
        # check revisions
        for (thisVersion in keepVersions){
          presentPackageVersion <- archiveSubset(data=presentPackages, var="Version", values=thisVersion)
          if(!is.null(keep.revisions) & keep.revisions > 0){
            allRevisions <- presentPackageVersion[["Revision"]]
            allRevisions <- allRevisions[order(allRevisions, decreasing=TRUE)]
            keepFullVersions <- c(
              keepFullVersions,
              archiveSubset(data=presentPackageVersion, var="Revision", values=allRevisions[1:keep.revisions])[["FullVersion"]]
            )
          } else {
            # keep all revisions of this version
            keepFullVersions <- c(keepFullVersions, presentPackageVersion[["FullVersion"]])
          }
        }
      } else {
        keepFullVersions <- ""
      }
      presentFullVersions <- unique(presentPackages[,"FullVersion"])
      moveVersions <- presentFullVersions[!presentFullVersions %in% keepFullVersions]
      moveSourceOrig <- presentVersions[!presentVersions %in% keepVersions]
      debNamesAll <- archiveSubset(presentPackages, var="FullVersion", values=moveVersions)
      debNamesSrcAll <- archiveSubset(presentPackages, var="Version", values=moveSourceOrig)
      if(length(moveVersions) > 0){
        if("Files.orig.name" %in% names(thisPackages)){
          if(length(debNamesAll[["Files.dsc.name"]]) > 0){
            mvToArchive(this.package, repo=repo.root, archive=file.path(archive.root, to.dir),
              type="deb", overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=justDelete,
              deb.names=debNamesAll[["Files.dsc.name"]], graceful=graceful
            )
            didArchiveSomething <- TRUE
          } else {}
          if(length(debNamesAll[["Files.debian.name"]]) > 0){
            mvToArchive(this.package, repo=repo.root, archive=file.path(archive.root, to.dir),
              type="deb", overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=justDelete,
              deb.names=debNamesAll[["Files.debian.name"]], graceful=graceful
            )
            didArchiveSomething <- TRUE
          } else {}
          if(length(unique(debNamesSrcAll[["Files.orig.name"]])) > 0){
            mvToArchive(this.package, repo=repo.root, archive=file.path(archive.root, to.dir),
              type="deb", overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=justDelete,
              deb.names=unique(debNamesSrcAll[["Files.orig.name"]]), graceful=graceful
            )
            didArchiveSomething <- TRUE
          } else {}
        } else {
          if(length(debNamesAll[["Filename"]]) > 0){
            mvToArchive(this.package, repo=repo.root, archive=file.path(archive.root, to.dir),
              type="deb", overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=justDelete,
              deb.names=debNamesAll[["Filename"]], graceful=graceful
            )
            didArchiveSomething <- TRUE
          } else {}
          for(thisChanges in names(debNamesAll)[grepl("^changes", names(debNamesAll))]){
            # some packages don't provide all changes files, so we'll exclude the NAs right away
            deb.names <- debNamesAll[[thisChanges]][!is.na(debNamesAll[[thisChanges]])]
            if(length(deb.names) > 0){
              mvToArchive(this.package, repo=repo.root, archive=file.path(archive.root, to.dir),
                type="deb", overwrite=overwrite, reallyDoIt=reallyDoIt, justDelete=justDelete,
                deb.names=deb.names, graceful=graceful
              )
              didArchiveSomething <- TRUE
            } else {}
          }
        }
      } else {}
    }
  }
  return(didArchiveSomething)
} ## end function deb.archive.packages()
