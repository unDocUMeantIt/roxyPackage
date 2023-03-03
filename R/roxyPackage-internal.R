# Copyright 2011-2023 Meik Michalke <meik.michalke@hhu.de>
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

# empty environment for experimental tweaks
.roxyPackage.env <- new.env()

# internal helper functions to get/set values in that environment
get.roxyEnv <- function(name=NULL){
  if(is.null(name)){
    # if no name was specified, return the whole environment as a list
    result <- as.list(.roxyPackage.env)
  } else {
    result <- get(name, envir=.roxyPackage.env)
  }
  return(result)
}
set.roxyEnv <- function(name="roxygenVersion", value=2){
  assign(name, value, envir=.roxyPackage.env)
}

# set default function to roxygenize docs
set.roxyEnv(name="roxyFunction", value=roxygen2::roxygenize)
# set default version of roxygen to use
set.roxyEnv(name="roxygenVersion", value=2)
# set default for 'Rdevel'
set.roxyEnv(name="Rdevel", value=FALSE)
# sandboxing defaults to false
set.roxyEnv(name="sandbox", value=new("roxySandbox", active=FALSE))

## work around some limitations regarding private functions
installMD5sums <- getFromNamespace(".installMD5sums", "tools")
news2Rd <- getFromNamespace("news2Rd", "tools")

# gegerate some XML functions (requires XiMpLe >= 0.11-1)
gen_tag_functions(
  tags=c(
    "![CDATA["
    , "a"
    , "atom:link"
    , "blockquote"
    , "body"
    , "br"
    , "channel"
    , "code"
    , "description"
    , "generator"
    , "guid"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "head"
    , "html"
    , "img"
    , "item"
    , "language"
    , "li"
    , "link"
    , "meta"
    , "p"
    , "pre"
    , "pubDate"
    , "rss"
    , "span"
    , "table"
    , "td"
    , "title"
    , "tr"
    , "ul"
  ),
  envir=parent.env(.roxyPackage.env),
  func_rename=c(
    "?xml_"="xml_"
    , "!--_"="comment_"
    , "![CDATA[_"="CDATA_"
    , "!DOCTYPE_"="DOCTYPE_"
    , "atom:link_"="atomlink_"
  )
)


## function trim()
# cuts off space at start and end of a character string
trim <- function(char){
  char <- gsub("^[[:space:]]*", "", char)
  char <- gsub("[[:space:]]*$", "", char)
  return(char)
} ## end function trim()


## function isUNIX()
# if isUNIX=TRUE only checks if this is a unix OS
isUNIX <- function(){
  thisOS <- base::.Platform[["OS.type"]]
  return(
    switch(thisOS,
      windows=FALSE,
      unix=TRUE)
  )
} ## end function isUNIX()


## function checkTools()
# checks if the listed tools are available
# returns a named vector if all went well
checkTools <-function(...){
  tools <- c(...)
  foundTools <- Sys.which(tools)
  missingTools <- foundTools %in% ""
  if(any(missingTools)){
    stop(simpleError(paste0("can't find these tools:\n    '",
      paste(tools[missingTools], collapse="'\n    '"),
      "'\n  please check your environment!")))
  }
  return(foundTools)
} ## end function checkTools()


## function getRvers()
# this looks like too complicated, why not simply read R.version$major/minor?
# well, it's because we need to support R versions which are not currently running!
getRvers <- function(R.homes=R.home(), win=FALSE){
  if(isTRUE(get.roxyEnv("Rdevel"))){
    # special case: someone's trying R-devel which doesn't have a version number set
    R.Version.full <- get.roxyEnv("Rversion")
  } else {
    R.bin <- file.path(R.homes, "bin", "R")
    if(isUNIX()){
      R.Version.full <- system(paste0(R.bin, " --version"), intern=TRUE)
    } else {
      R.Version.full <- shell(paste0(R.bin, " --version"), translate=TRUE, intern=TRUE)
    }
    R.Version.full <- R.Version.full[grep("R version ([[:digit:]]+).([[:digit:]]+)", R.Version.full)]
    R.Version.full <- gsub("R version ([[:digit:]]+).([[:digit:]]+)([.]?)([[:digit:]]+)(.*)", "\\1.\\2\\3\\4", R.Version.full, perl=TRUE)
  }
  if(isTRUE(win)){
    R.Version.full <- gsub("([[:digit:]]+).([[:digit:]]+)(.*)", "\\1.\\2", R.Version.full, perl=TRUE)
  }
  return(R.Version.full)
} ## end function getRvers()


## function listRDirs()
# tries to find directories named after R versions, which is used by archive.packages()
listRDirs <- function(path, full.path=TRUE){
  if(grepl("^file:(/)+", path)){
    path <- gsub("^file:(/)+", "/", path)
    add.file.prefix <- "file:/"
  } else {
    add.file.prefix <- ""
  }
  path <- gsub("/$", "", path)
  all.dirs <- dir(path, full.names=FALSE, recursive=FALSE)
  R.dirs <- all.dirs[grepl("^([[:digit:]]+).([[:digit:]]+)$", all.dirs)]
  if(isTRUE(full.path)){
    return(file.path(paste0(add.file.prefix, path, collapse=""), R.dirs))
  } else {
    return(R.dirs)
  }
} ## end function listRDirs()


## function filter.repo.packages()
# simple function to strip irrelevant packages
filter.repo.packages <- function(pkg.list, packages){
  # inpuit needs to be a matrix
  if(!is.matrix(pkg.list)){
    pkg.list <- t(as.matrix(pkg.list))
  }
  if(!is.null(packages)){
    relevant.pkgs <- pkg.list[,"Package"] %in% packages
    pkg.list <- pkg.list[relevant.pkgs,]
  } else {}
  # even if there's only one package, ensure output is still a matrix
  if(!is.matrix(pkg.list)){
    pkg.list <- t(as.matrix(pkg.list))
  }
  return(pkg.list)
} ## end function filter.repo.packages()


## function get.by.role()
# filters a vector with person objects by roles
get.by.role <- function(persons, role="aut"){
  role.filter <- function(x){is.null(r <- x$role) | role %in% r}
  filtered.persons <- Filter(role.filter, persons)
  return(filtered.persons)
} ## end function get.by.role()


## function get.authors()
get.authors <- function(description, maintainer=TRUE, contributor=FALSE, copyright=FALSE, all.participants=FALSE, check.orcid=FALSE){
  if("Authors@R" %in% names(description)){
    gotPersons <- TRUE
    authorsFromDescription <- description[["Authors@R"]]
  } else if("Author@R" %in% names(description)){
    gotPersons <- TRUE
    authorsFromDescription <- description[["Author@R"]]
  } else {
    gotPersons <- FALSE
  }
  
  if(isTRUE(gotPersons)){
    got.aut <- paste(format(get.by.role(eval(parse(text=authorsFromDescription))), include=c("given", "family")), collapse=", ")
    got.cre <- ifelse(isTRUE(maintainer),
      paste(format(get.by.role(eval(parse(text=authorsFromDescription)), role="cre"), include=c("given", "family", "email")), collapse=", "),
      "")
    got.ctb <- ifelse(isTRUE(contributor),
      paste(format(get.by.role(eval(parse(text=authorsFromDescription)), role="ctb"), include=c("given", "family")), collapse=", "),
      "")
    got.cph <- ifelse(isTRUE(copyright),
      paste(format(get.by.role(eval(parse(text=authorsFromDescription)), role="cph"), include=c("given", "family")), collapse=", "),
      "")
    if(isTRUE(all.participants)){
      AuthorsR_parsed <- eval(parse(text=authorsFromDescription))
      if(isTRUE(check.orcid)){
        # try to turn ORCIDs into a link
        # this is a bit back and forth between unevaluated code and evaluated persons,
        # but so far there doesn't seem to be a more robust way of altering person objects
        AuthorsR_updated <- paste0("c(",
          paste0(
            sapply(AuthorsR_parsed, function(x){
              if("ORCID" %in% names(x$comment)){
                x <- person(
                  given=x$given,
                  family=paste0(x$family, " <a href=\"https://orcid.org/", x$comment[["ORCID"]] ,"\" target=\"_blank\" style=\"text-decoration:none;\"><img alt=\"ORCID iD\" src=\"../orcid.svg\" style=\"width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle;\" /></a>"),
                  role=x$role,
                  comment=x$comment[!names(x$comment) %in% "ORCID"]
                )
              } else {}
              return(paste0(format(x, style="R"), collapse=" "))
            }),
            collapse=", "
          ),
          ")"
        )
        AuthorsR_parsed <- eval(parse(text=AuthorsR_updated))
      } else {}
      got.participants <- format(AuthorsR_parsed, include=c("given", "family", "role", "comment"))
    } else {
      got.participants <- ""
    }
  } else {
    got.aut <- description[["Author"]]
    got.cre <- ifelse(isTRUE(maintainer),
      description[["Maintainer"]],
      "")
    # contributors should already be named in got.aut
    got.ctb <- ""
    got.cph <- ""
    got.participants <- ifelse(isTRUE(all.participants), got.aut, "")
  }
  got.cre.clean <- gsub("<([^@]*)@([^>]*)>", "\\\\email{\\1@@\\2}", gsub("\n[[:space:]]*", "\n#' ", got.cre))
  # append contributors
  if(isTRUE(contributor) && got.ctb != ""){
    got.aut <- paste0(got.aut, ", with contributions from ", got.ctb)
  } else {}
  gotAuthors <- list(aut=got.aut, cre=got.cre, cre.clean=got.cre.clean, ctb=got.ctb, cph=got.cph, participants=got.participants)
  return(gotAuthors)
} ## end function get.authors()

## function getDescField()
# extracts fields from description, including checks
# if "field" is a vector, they are tested in order, befor an error is returned
getDescField <- function(desc, field, stopOnErr=TRUE){
  valid.fields <- field %in% colnames(desc)
  if(any(valid.fields)){
    # give a warning if the first alternative didn't check out
    if(!isTRUE(valid.fields[1])){
      warning(paste0("field \"", field[1],"\" missing in DESCRIPTION file, used \"", field[valid.fields][1], "\" as fallback!"), call.=FALSE)
    } else {}
    this.field <- desc[,field[valid.fields]][1]
  } else {
    this.field <- NULL
  }
  if(is.null(this.field) & isTRUE(stopOnErr)){
    stop(simpleError(paste0("fields missing in DESCRIPTION file: \"", paste(field, collapse="\", \""), "\"")))
  } else {}
  return(as.character(this.field))
} ## end function getDescField()


## function dateRFC2822()
dateRFC2822 <- function(date=Sys.time()){
  if(!inherits(date, c("POSIXct", "POSIXt"))){
    date <- as.Date(date)
  } else {}
  # for valid RFC2822 date conversions, always force english locale (temporarily)
  orig.locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale="C")
  on.exit(Sys.setlocale(category = "LC_TIME", locale=orig.locale))
  return(format(date, "%a, %d %b %Y %H:%M:%S %z"))
} ## end function dateRFC2822()


## function checkLicence()
checkLicence <- function(license, logical=TRUE, deb=TRUE){
  # for easier matching, remove spaces and turn into lowercase
  normaLice <- gsub("[[:space:]>=()]", "", tolower(license))

  knownLicenses <- list(
    agpl="3",
    apache="2",
    artistic=c("1","2"),
    bsd="",
    gfdl=c("1.2","1.3"),
    lgpl=c("2.1","2","3"),
    gpl=c("1","2","3")
  )

  # these licenses are also part of roxyPackage, in the file common-licenses.zip
  debianLicenses <- list(
    agpl=c(
      file="AGPL-3",
      name="GNU Affero General Public License",
      version="3"),
    agpl3=c(
      file="AGPL-3",
      name="GNU Affero General Public License",
      version="3"),
    apache=c(
      file="Apache-2.0",
      name="Apache License",
      version="2.0"),
    apache2=c(
      file="Apache-2.0",
      name="Apache License",
      version="2.0"),
    artistic=c(
      file="Artistic-1.0",
      name="Artistic License",
      version="1.0"),
    artistic1=c(
      file="Artistic-1.0",
      name="Artistic License",
      version="1.0"),
    artistic2=c(
      file="Artistic-2.0",
      name="Artistic License",
      version="2.0"),
    bsd=c(
      file="BSD",
      name="BSD License",
      version=NA),
    gfdl=c(
      file="GFDL-1.3",
      name="GNU Free Documentation License (GFDL)",
      version="1.3"),
    gfdl1.2=c(
      file="GFDL-1.2",
      name="GNU Free Documentation License (GFDL)",
      version="1.2"),
    gfdl1.3=c(
      file="GFDL-1.3",
      name="GNU Free Documentation License (GFDL)",
      version="1.3"),
    lgpl=c(
      file="LGPL-3",
      name="GNU Lesser General Public License (LGPL)",
      version="3"),
    lgpl2.1=c(
      file="LGPL-2.1",
      name="GNU Lesser General Public License (LGPL)",
      version="2.1"),
    lgpl2=c(
      file="LGPL-2",
      name="GNU Lesser General Public License (LGPL)",
      version="2"),
    lgpl3=c(
      file="LGPL-3",
      name="GNU Lesser General Public License (LGPL)",
      version="3"),
    gpl=c(
      file="GPL-3",
      name="GNU General Public License (GPL)",
      version="3"),
    gpl1=c(
      file="GPL-1",
      name="GNU General Public License (GPL)",
      version="1"),
    gpl2=c(
      file="GPL-2",
      name="GNU General Public License (GPL)",
      version="2"),
    gpl3=c(
      file="GPL-3",
      name="GNU General Public License (GPL)",
      version="3")
  )

  knownLicNames <- names(knownLicenses)
  licFound <- FALSE
  licName <- ""
  licVers <- ""
  licIter <- 1
  while(!isTRUE(licFound) & licIter <= length(knownLicNames)){
    thisLic <- knownLicNames[licIter]
    if(grepl(thisLic, normaLice)){
      licFound <- TRUE
      licName <- thisLic
      versFound <- FALSE
      versIter <- 1
      licVersions <- knownLicenses[[thisLic]]
      if(length(licVersions) > 0){
        while(!isTRUE(versFound) & versIter <= length(licVersions)){
          thisVers <- licVersions[versIter]
          if(grepl(thisVers, normaLice)){
            versFound <- TRUE
            licVers <- thisVers
          } else {
            versIter <- versIter + 1
          }
        }
      } else {}
    } else {
      licIter <- licIter + 1
    }
  }
  licenseString <- paste(licName, licVers, sep="")

  if(!identical(licenseString, "")){
    if(isTRUE(logical)){
      return(TRUE)
    } else if(isTRUE(deb)){
      return(debianLicenses[[licenseString]])
    } else {
      return(licenseString)
    }
  } else {
    if(isTRUE(logical)){
      return(FALSE)
    } else {
      warning("license: sorry, didn't recognize license string!", call.=FALSE)
      return(licenseString)
    }
  }
} ## end function checkLicence()


## function copyLicence()
copyLicence <- function(license, destFile, overwrite=FALSE){
  if(!file_test("-f", destFile) | isTRUE(overwrite)){
    stopifnot(checkLicence(license))
    licenseInfo <- checkLicence(license, deb=TRUE, logical=FALSE)
    # get location of roxyPackage installation
    stopifnot("roxyPackage" %in% rownames(installed.packages()))
    rPHome <- installed.packages()["roxyPackage","LibPath"]
    cLPath <- file.path(rPHome, "roxyPackage","common-licenses.zip")
    if(!file.exists(cLPath)){
      stop(simpleError(paste("file not found:", cLPath)))
    } else {}
#    unzip(LCC.path, files=LCC.zip.wanted, junkpaths=TRUE, exdir=tmp.path)
    licenseText <- readLines(licenseCon <- unz(cLPath, filename=file.path("common-licenses", licenseInfo[["file"]])))
    close(licenseCon)
    writeLines(licenseText, con=destFile)
    message(paste0("license: saved a copy of the ", licenseInfo[["name"]], " as LICENSE"))
  } else {
    message(paste("skipping, license file exists:", destFile))
  }
  return(invisible(NULL))
} ## end function copyLicence()


## function file.mv()
file.mv <- function(from, to, overwrite=recursive, recursive=FALSE){
  stopifnot(file.copy(from=from, to=to, overwrite=overwrite, recursive=recursive))
  stopifnot(file.remove(from))
  return(invisible(NULL))
} ## end function file.mv()


## function createMissingDir()
createMissingDir <- function(dirPath, action="files", quiet=FALSE){
  if(!file_test("-d", dirPath)){
    stopifnot(dir.create(dirPath, recursive=TRUE))
    if(!isTRUE(quiet)){
      message(paste0(action, ": created ", dirPath, "."))
    } else {}
  } else {}
  return(invisible(NULL))
} ## end function createMissingDir()


## function removeIfExists()
removeIfExists <- function(filePath){
  if(file.exists(filePath)){
    stopifnot(file.remove(filePath))
  } else {}
  return(invisible(NULL))
} ## end function removeIfExists()


## function mvToArchive()
# - deb.name: debian package names are fully available via deb.archive.packages(), so they
#       have their own argument 
mvToArchive <- function(package, repo, archive, versions=NA, type=NA, file=NA, overwrite=FALSE,
  reallyDoIt=FALSE, justDelete=FALSE, deb.names=NULL, graceful=FALSE){

  repo <- gsub("^file:(/)+", "/", repo)
  archive <- gsub("^file:(/)+", "/", archive)

  if(isTRUE(reallyDoIt) && !isTRUE(justDelete)){
    createMissingDir(dirPath=archive, action="archive")
    if(!is.null(deb.names)){
      deb.dirs <- file.path(archive, unique(dirname(deb.names)))
      createMissingDir(dirPath=deb.dirs, action="archive")
    } else {}
  } else {}

  if(is.null(deb.names)){
    file.ending <- switch(type,
      source=".tar.gz",
      win.binary=".zip",
      mac.binary=".tgz"
    )
    pkg.names <- paste0(package, "_", versions, file.ending)
  } else {
    pkg.names <- deb.names
  }
  sapply(pkg.names, function(this.package){
    pkg.from <- normalizePathByOS(file.path(repo, this.package))
    pkg.to <- normalizePathByOS(file.path(archive, this.package))
    if(file.exists(pkg.from)){
      # don't archive, just remove files
      if(isTRUE(justDelete)){
        if(isTRUE(reallyDoIt)){
          message(paste0("archive: deleting file ", pkg.from))
          removeIfExists(pkg.from)
        } else {
          message(paste0("archive: deleting file ", pkg.from, " (NOT RUN!)"))
        }
      } else {
        if(isTRUE(reallyDoIt)){
          message(paste0("archive: moving ", pkg.from, " to ", pkg.to))
          file.mv(from=pkg.from, to=pkg.to, overwrite=overwrite)
        } else {
          message(paste0("archive: moving ", pkg.from, " to ", pkg.to, " (NOT RUN!)"))
        }
      }
    } else {
      if(isTRUE(graceful)){
        warning(paste0("file doesn't exist, skipping:\n  ", pkg.from))
      } else {
        stop(simpleError(paste0("file doesn't exist:\n  ", pkg.from)))
      }
    }
  })
  return(invisible(NULL))
} ## end function mvToArchive()


## function normalizePathByOS()
# path: the root path to be normalized
# unix.OS: logical value to set the OS
# filePrefix: if TRUE, file:/// is prefixed
normalizePathByOS <- function(path, is.unix=isUNIX(), mustWork=FALSE, filePrefix=FALSE){
  # normalize a given root path
  # this may give bogus results on windows, so we'll encapsulate
  # it in shortPathName(), which is not available on all OSs
  if(isTRUE(is.unix)){
    # if some function already added file:///, let's strip it off
    if(grepl("^file:(/)+", path)){
      path <- gsub("^file:(/)+", "/", path)
    } else {}
    result <- normalizePath(path, mustWork=mustWork)
    slashes <- "//"
  } else {
    result <- shortPathName(normalizePath(path, mustWork=mustWork))
    slashes <- "///"
  }
  if(isTRUE(filePrefix) && !grepl("^file://", result)){
    result <- paste0("file:", slashes, result)
  } else {}
  return(result)
} ## end function normalizePathByOS()


## function sanitizeRdFiles()
# tries to automatically clean up Rd files generated by
# roxygenise(): its automatic usage sections can turn out to be
# longer than 90 characters which will result in a complaint
# from R CMD check, and subsequently no release on CRAN
# 
# the function either returns FALSE if no changes were done,
# or TRUE if the file was changed
sanitizeRdFiles <- function(
  RdFile,
  root.dir=NULL,
  maxlength=90,
  preserveCommentChar="%"
){
  file_changed <- FALSE
  RdFileFullPath <- ifelse(is.null(root.dir), RdFile, file.path(root.dir, RdFile))
  # read in the Rd file
  origFile <- readLines(RdFileFullPath)
  # we'll use the autoLineBreak() function defined in 01_methods_02_ChangeLog_internal.R:
  newFile <- autoLineBreak(origFile, lineEnd=maxlength, breakAt=c(","), breakBy=",\n     ", preserveCommentChar=preserveCommentChar)
  # just in case no comma split was possible
  # newFile <- autoLineBreak(newFile, lineEnd=maxlength, breakAt=c(" "), breakBy="\n     ")
  # only overwrite the file if changes were made at all
  if(!identical(origFile, newFile)){
    result <- paste0(newFile, "\n", collapse="")
    cat(result, file=RdFileFullPath)
    file_changed <- TRUE
  } else {}
  return(file_changed)
} ## end function sanitizeRdFiles()


## function configFile()
# used to set or remove .Rbuildignore and .Rinstignore
# type: either "inst" or "build"
# content: function does nothing if NULL, will remove the file if ""
configFile <- function(root, type="inst", content=NULL){
  # simply quit if content is NULL
  if(is.null(content)){
    return(invisible(NULL))
  } else {}
  # supported file types
  fileName <- switch(type,
      "inst"=".Rinstignore",
      "build"=".Rbuildignore",
      stop(simpleError(paste("invalid config file type:", type)))
    )
  filePath <- file.path(root, fileName)
  # check if the file is there
  fileExists <- file.exists(filePath)

  if(identical(content, "")){
    if(fileExists){
      stopifnot(file.remove(filePath))
      message(paste0("config: removed file ", fileName))
    } else {
      return(invisible(NULL))
    }
  } else {
    # ok, content is neither NULL nor "", write it to requested file
    if(fileExists){
      message(paste0("config: replacing file ", fileName))
    } else {
      message(paste0("config: writing file ", fileName))
    }
    cat(content, "", sep="\n", file=filePath)
  }
  return(filePath)
}
## end function configFile()


## function mergeOptions()
# takes a function and a set of custom options, replaces defaults with custom settings
# (if given as a named list or vector) and returns a merged set of options for external use
# - newDefaults: override the actual defaults of someFunction, before applying customOptions
mergeOptions <- function(someFunction, customOptions=NULL, newDefaults=NULL){
  defaults <- formals(someFunction)
  args.def <- names(defaults)
  args.new <- names(newDefaults)
  args.set <- names(customOptions)
  # first set new defaults
  defaults[args.new[args.new %in% args.def]] <- newDefaults[args.new[args.new %in% args.def]]
  # now apply custom user settings
  defaults[args.set[args.set %in% args.def]] <- customOptions[args.set[args.set %in% args.def]]
  return(defaults)
}
## end function mergeOptions()


## function binPackageLinks()
# takes a package name and repository root, and looks up all binary packages compiled against different R versions
# returns a list with the results
binPackageLinks <- function(package, version, repo.root, type="win"){
  pckg.basename <- paste0(package, "_", version)
  clean.repo.root <- gsub("/$", "", repo.root)
  win.results <- mac.results <- list()

  if("win" %in% type){
    pckg.name.win <- paste0(pckg.basename, ".zip")
    repo.win <- listRDirs(file.path(clean.repo.root,  "bin", "windows", "contrib"), full.path=TRUE)
    for (this.repo in repo.win){
      if(file.exists(file.path(this.repo, pckg.name.win))){
        R.vers <- basename(this.repo)
        win.results[[R.vers]] <- pckg.name.win
      } else {}
    }
    
  } else {}

  if("mac" %in% type){
    pckg.name.mac <- paste0(pckg.basename, ".tgz")
    repo.mac <- listRDirs(file.path(clean.repo.root, "bin", "macosx", "contrib"), full.path=TRUE)
    for (this.repo in repo.mac){
      if(file.exists(file.path(this.repo, pckg.name.mac))){
        R.vers <- basename(this.repo)
        mac.results[[R.vers]] <- pckg.name.mac
      } else {}
    }
  } else {}
  
  result <- list(
    win=win.results,
    mac=mac.results
  )
  return(result)
}
## end function binPackageLinks()


## function archiveSubset()
# called by the archive functions to get proper subsets of data
archiveSubset <- function(data, var, values){
  result <- data[data[,var] %in% values,]
  # even if there's only one package, ensure it's still a matrix
  if(!is.matrix(result) & !is.data.frame(result)){
    result <- t(as.matrix(result))
  }
  return(result)
} ## end function archiveSubset()

## function excludeVCSDirs()
# since not all tar implementations (especially the BSD default on Mac OS X) support --exclude-vcs,
# we'll exclude these manually
# - src: path to the directory to archive
excludeVCSDirs <- function(src, exclude.dirs=c(".svn", "CVS", ".git", "_darcs", ".hg"), action="repo", target="mac binary"){
  VCS.allDirs <- list.dirs(src)
  VCS.excludeDirs <- VCS.allDirs[grepl(paste(paste0(".*", exclude.dirs, "$"), collapse="|"), VCS.allDirs)]
  # clean up the paths
  path.to.dir <- dirname(src)
  VCS.excludeDirs <- gsub("^/", "", gsub(path.to.dir, "", VCS.excludeDirs))
  if(length(VCS.excludeDirs) > 0){
    tar.extraFlags <- paste(paste0(" --exclude='", VCS.excludeDirs, "'"), collapse="")
    message(paste0(action, ": excluded these directories from ", target, ":\n  ", paste(VCS.excludeDirs, collapse="\n  ")))
  } else {
    tar.extraFlags <- ""
  }
  return(tar.extraFlags)
} ## end function excludeVCSDirs()


## function getURL()
# takes the value of the URL argument and returns either NULL, if no value was set,
# or the URL to use for the given purpose, in case multiple URLs were defined
getURL <- function(URL=NULL, purpose="default", deb.dir="deb"){
  URLnames <- names(URL)
  validPurposes <- c("default", "debian", "mirror.list", "debian.path")
  stopifnot(purpose %in% validPurposes)
  if(any(is.null(URL), is.character(URL) & isTRUE(length(URL) == 1))){
    # these are invalid/meaningless here
    if(identical(purpose, "mirror.list")){
      result <- NULL
    } else if(identical(purpose, "debian.path")){
      result <- paste0("/", deb.dir)
    } else {
      if(any(is.null(URLnames), identical(URLnames, "default"))){
        result <- as.character(URL)
      } else {
        stop(simpleError(paste0("if you only use one named 'URL' value, the name \"default\" is mandatory!")))
      }
    }
  } else if(is.character(URL) & isTRUE(length(URL) > 1)){
    if(
      all(
        isTRUE(length(URLnames) == length(URL)),
        all(URLnames %in% validPurposes),
        "default" %in% URLnames
      )
    ){
      if(purpose %in% URLnames){
        result <- URL[[purpose]]
        if(
          all(
            identical(purpose, "debian.path"),
            !grepl("^/", result)
          )
        ){
          stop(simpleError("if you set \"debian.path\" in 'URL', it must start with a slash!"))
        } else {}
      } else {
        # check a special cases
        if(identical(purpose, "mirror.list")){
          result <- NULL
        } else if(identical(purpose, "debian.path")){
          result <- paste0("/", deb.dir)
        } else {
          result <- URL[["default"]]
        }
      }
    } else {
      stop(simpleError(paste0("for 'URL' values, only use valid names (\"", paste0(validPurposes, collapse="\", \""), "\"), where \"default\" is mandatory!")))
    }
  } else {
    stop(simpleError("if you set the 'URL' argument, it must a character vector with >= 1 values!"))
  }
  return(result)
} ## end function getURL()

## function XML.value()
# checks if 'tag' is actually not a tag but value/content/data. returns TRUE or FALSE
# simplified from a function originally in the XiMpLe package
XML.value <- function(tag){
  return(sapply(
    tag,
    function(this.tag){
      return(grepl("^[[:space:]]*[^<]", this.tag))
    },
    USE.NAMES=FALSE
  ))
} ## end function XML.value()


## function split.chars()
# used to split a character string into parts at each occurrence of the start and end of a regex pattern
# this private function was originally a part of the XiMpLe package and copied here in a simplified version
split.chars <- function(txt, pattern){
  found.pattern <- gregexpr(pattern, text=txt, perl=TRUE)
  found.pattern.start <- found.pattern[[1]]
  found.pattern.end <- found.pattern.start + attr(found.pattern[[1]], "match.length") - 1
  # returned -1 if pattern wasn't found
  if(found.pattern.start[1] == -1){
    return(txt)
  } else {
    txt.length <- nchar(txt)
    num.found.patterns <- length(found.pattern.start)
    result <- unlist(sapply(
      0:num.found.patterns,
      function(pat.idx){
        # 0: chars before first match
        if(pat.idx == 0){
          if(found.pattern.start[1] > 1){
            return(substr(txt, 1, found.pattern.start[1] - 1))
          } else {}
        } else {
          result.match <- substr(txt, found.pattern.start[pat.idx], found.pattern.end[pat.idx])
          # check if there's stuff between two matches
          aft.match <- found.pattern.end[pat.idx] + 1
            if(pat.idx < num.found.patterns){
              nxt.match <- found.pattern.start[pat.idx + 1]
            } else {
              nxt.match <- txt.length + 1
            }
          if(aft.match < nxt.match){
            result.aft.match <- trim(substr(txt, aft.match, nxt.match - 1))
            # remove empty space
            if(!identical("", result.aft.match)){
              result.match <- c(result.match, result.aft.match)
            } else {}
          } else {}
          return(result.match)
        }
      },
      USE.NAMES=FALSE
    ), use.names=FALSE)
    return(result)
  }
} ## end function split.chars()


## function XML.single.tags()
# Splits one character string or vector with an XML tree into a vector with its single tags.
# - tree: The XML tree, must be character.
# this private function was originally a part of the XiMpLe package and copied here in a simplified version
XML.single.tags <- function(tree){
  if(!is.character(tree)){
    stop(simpleError("'tree' must be character!"))
  } else {}
  if(length(tree) > 1) {
    # force tree into one string
    tree <- paste(tree, collapse="")
  } else {}
  # remove space at beginning (and end)
  tree <- trim(tree)

  ## the main splitting process
  # we don't need CDATA or comments
  tree <- gsub(
    pattern="<!\\[CDATA\\[((?s).*?)\\]\\]>|/\\*[[:space:]]*<!\\[CDATA\\[[[:space:]]*\\*/((?s).*?)/\\*[[:space:]]*\\]\\]>[[:space:]]*\\*/|<!--((?s).*?)-->",
    replacement="",
    x=tree,
    perl=TRUE
  )
  # now do the splitting
  single.tags <- sapply(
    tree,
    function(this.tree){
      these.tags <- unlist(split.chars(txt=this.tree, "<((?s).*?)>"), use.names=FALSE)
      # remove probably troublesome content like newlines
      these.tags[!XML.value(these.tags)] <- gsub("[[:space:]]+", " ", these.tags[!XML.value(these.tags)])
      return(these.tags)
    },
    USE.NAMES=FALSE
  )
  single.tags <- unlist(single.tags, use.names=FALSE)
  single.tags <- as.character(single.tags)
  return(single.tags)
} ## end function XML.single.tags()


## function keep_files()
# takes a vector of files and the supposed location of .install_extras,
# which should be a collection of perl regexes, and return only those
# files that match one of the rules. if the .install_extras files
# is not found, all files are returned
keep_files <- function(files, install_extras){
  if(file.exists(install_extras)){
    vign_inst_extras <- readLines(install_extras, warn=FALSE)
    # we'll create a logical vector with the same length as
    # files, and the value indicates whether at least one
    # regex was matched by that file
    keep <- sapply(
      files,
      function(thisFile){
        return(any(sapply(
          vign_inst_extras,
          function(thisRegex){
            return(grepl(thisRegex, thisFile, perl=TRUE))
          }
        )))
      },
      USE.NAMES=FALSE
    )
    result <- files[keep]
  } else {
    result <- files
  }
  return(result)
} ## end function keep_files()


## function pandoc()
# mimics tools:::.pandoc_md_for_CRAN()
# returns TRUE/FALSE to indicate whether the output file does exist
pandoc <- function(infile, outfile, title, args="-s --mathjax --email-obfuscation=references --self-contained"){
  pandoc_bin <- Sys.which("pandoc")
  if(isTRUE(pandoc_bin == "")){
    warning("no pandoc installation found. skipped compilation of markdown documents.")
  } else {
    pandoc_call <- paste0(pandoc_bin, " ", shQuote(infile), " ", args, " --metadata pagetitle=", shQuote(title), " -o ", shQuote(outfile))
    if(isTRUE(isUNIX())){
      pandoc_call <- paste0("cd ", shQuote(dirname(infile)), " && ", pandoc_call)
      system(pandoc_call, intern=TRUE)
    } else {
      pandoc_call <- paste0("cd /d ", shQuote(dirname(infile)), " & ", pandoc_call)
      shell(pandoc_call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
    }
  }
  return(file.exists(outfile))
} ## end function pandoc()


## function check_orcid_icon()
check_orcid_icon <- function(pckg, repo, filename="orcid.svg"){
  icon_file <- file.path(repo, filename)
  pckg.authors <- get.authors(pckg, maintainer=FALSE, all.participants=TRUE, check.orcid=TRUE)
  if(any(grepl("<a href=\"https://orcid.", pckg.authors[["participants"]]))){
    if(any(!file.exists(icon_file))){
      warning(
        paste0(
          "ORCID icon does not yet exist in your repository, try this call to fix this:\n    ",
          "orcid_icon_fetcher(\"", repo, "\")"
        ),
        call.=FALSE
      )
    } else {}
  }
  return(invisible(NULL))
} ## end function check_orcid_icon()


## function fetch_inst_from_package()
# when find.package() is run from a sandbox, path.package() which is called internally might
# return the sandboxed source directory as the library location, which messes up "inst" paths.
# can only happen if pck is roxyPackage itself. so just in case, try to fix the issue by checking paths
fetch_inst_from_package <- function(dir, pckg="roxyPackage"){
  libloc <- find.package(pckg)
  std_dir <- file.path(libloc, dir)
  src_dir <- file.path(libloc, "inst", dir)
  if(dir.exists(std_dir)){
    return(std_dir)
  } else if(dir.exists(src_dir)) {
    return(src_dir)
  } else {
    stop(simpleError(paste0("none of these directories found:\n  ", std_dir, "\n  ", src_dir)))
  }
} ## end function fetch_inst_from_package()
