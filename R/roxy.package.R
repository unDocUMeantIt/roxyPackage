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


#' Automatic doc creation, package building and repository update
#'
#' This function should help to create R packages with full documentation and updates to a local repository.
#' It supports source and binary packaging (Windows and Mac OS X; see Note section on the limitations).
#'
#' For the documentation \code{roxygen2}[1] is used. Next to the actual in-line documentation of the package's contents, you only need to
#' prepare a data.frame to be used to write a package \code{DESCRIPTION} file. See the example section for details on that. This means
#' that you \emph{neither} edit the \code{DESCRIPTION} \emph{nor} the \code{*-package.R} file manually, they will both be created \emph{automatically}
#' by this function with contents according to these settings!
#' 
#' @section Sandboxing:
#' If you want to check out the effects of roxy.package() without touching you actual package sources, try \code{\link[roxyPackage:sandbox]{sandbox}}
#' to set up a safe testing environment.
#'
#' @section Repository layout:
#' The repository will have this directory structure, that is, below the defined \code{repo.root}:
#'
#' \describe{
#'    \item{\code{./src/contrib}}{Here go the source packages}
#'    \item{\code{./bin/windows/contrib/$RVERSION}}{Here go the Windows binaries}
#'    \item{\code{./bin/macosx/contrib/$RVERSION}}{Here go the Mac OS X binaries (see \code{OSX.repo} for further options)}
#'    \item{\code{./pckg/index.html}}{A global package index with links to packages' index files, if actions included \code{"html"}}
#'    \item{\code{./pckg/web.css}}{A CRAN-style CSS file, if actions included \code{"html"}}
#'    \item{\code{./pckg/$PACKAGENAME}}{Here go documentation PDF and vignette, as well as a \code{ChangeLog} file, if found.
#'      and an \code{index.html} with package information, if actions included \code{"html"}.
#'      This is probably a bit off-standard, but practical if you several packages.}
#' }
#'
#' @section Converting ChangeLogs into NEWS:
#' See \code{\link[roxyPackage:cl2news]{cl2news}} for details.
#' 
#' @section Build for several R versions:
#' The options \code{R.libs} and \code{R.homes} can actually take more than one string, but a vector of strings. This can be used
#' to build packages for different R versions, provided you installed them on your system. If you're running GNU/Linux, an easy way
#' of doing so is to fetch the R sources from CRAN, calling \code{"./configure"} with something like \code{"--prefix=$HOME/R/<R version>"},
#' so that \code{"make install"} installs to that path. Let's assume you did that with R 3.2.2 and 3.1.3, you could then call \code{roxy.package}
#' with options like \code{R.homes=c("home/user/R/R-3.1.3", "home/user/R/R-3.2.2")} and \code{R.libs=c("home/user/R/R-3.1.3/lib64/R/library",}
#' \code{"home/user/R/R-3.2.2/lib64/R/library")}. \code{roxy.package} will then call itself recursively for each given R installation.
#' 
#' One thing you should be aware of is that \code{roxy.package} will not perform all actions each time. That is because some of them, namely
#' \code{"roxy"}, \code{"cite"}, \code{"license"}, \code{"doc"}, \code{"cl2news"} and \code{"news2rss"}, should produce identical
#' results anyway, so they are only considered during the first run. You should always place the R version which should be linked to from the
#' HTML index last in line, because \code{"html"} will overwrite previous results. For a similar reason, the \code{"deb"} action will only actually
#' build a binary package during the last run, but debianizing it will be done during the first.
#'
#' @section Windows: On Windows, the actions \code{"doc"} and \code{"check"} will only work correctly if you have installed and configured LaTeX
#' accordingly, and you will also need Rtools set up for packaging.
#'
#' @section CRAN compliance: The CRAN policies can sometimes be very strict. This package should allow you to produce packages which are suitable
#' for release on CRAN. But some steps have to be taken care of by yourself. For instance, CRAN does currently not allow copies of common licenses
#' in a source package, nor a \code{debian} folder. Therefore, if your package is supposed to be released on CRAN, you should include
#' \code{Rbuildignore=c("debian", "LICENSE")} to the function call.
#'
#' @note The binary packaging is done simply by zipping (Windows) or targzipping (Mac OS X) the built and installed package. This should
#' do the trick as long as your package is written in pure R code. It will most likely not produce usable packages if it contains
#' code in other languages like C++.
#'
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources.
#' @param pck.version Character string, defining the designated version number. Can be omitted if actions don't
#'    include \code{"roxy"}, then this information is read from the present DESCRIPTION file.
#' @param pck.description Data frame holding the package description (see Examples section).
#' @param R.libs Character string, valid path to the R library where the package should be installed to.
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param pck.date Character string of the release date in YYYY-MM-DD format. Defaults to \code{Sys.Date()}. If actions don't
#'    include \code{"roxy"}, then this information is read from the present DESCRIPTION file.
#' @param actions Character vector, must contain at least one of the following values:
#'    \describe{
#'      \item{"roxy"}{Roxygenize the docs}
#'      \item{"cite"}{Update CITATION file}
#'      \item{"license"}{Update LICENSE file}
#'      \item{"readme"}{Generate initial README.md file}
#'      \item{"check"}{Do a full package check, calling \code{R CMD check}}
#'      \item{"package"}{Build & install the package, update source repository, calling \code{R CMD build} and \code{R CMD INSTALL}}
#'      \item{"binonly"}{Like \code{"package"}, but doesn't copy the source package to the repository, to enable binary-only rebuilds}
#'      \item{"cl2news"}{Try to convert a ChangeLog file into an NEWS.Rd file}
#'      \item{"news2rss"}{Try to convert \code{inst/NEWS.Rd} into an RSS feed. You must also set
#'        \code{URL} accordingly.}
#'      \item{"doc"}{Update PDF documentation and vignette (if present), \code{R CMD Rd2pdf} (or \code{R CMD Rd2dvi} for R < 2.15)}
#'      \item{"html"}{Update HTML index files}
#'      \item{"win"}{Update the Windows binary package}
#'      \item{"macosx"}{Update the Mac OS X binary package}
#'      \item{"log"}{Generate initial ChangeLog or update a present ChangeLog file}
#'      \item{"deb"}{Update the Debian binary package with \code{\link[roxyPackage:debianize]{debianize}} (works only on Debian systems;
#'        see \code{deb.options}, too). \code{URL} must also be set to generate Debian repository information.}
#'      \item{"cleanRd"}{Insert line breaks in Rd files with lines longer than 90 chars}
#'    }
#'    Note that \code{"cl2news"} will write the \code{NEWS.Rd} file to the \code{inst} directory of your sources, which will overwrite
#'    an existing file with the same name! Also note that if both a \code{NEWS/NEWS.Rd} and \code{ChangeLog} file are found, only
#'    news files will be linked by the \code{"html"} action.
#' @param cleanup Logical, if \code{TRUE} will remove backup files (matching \code{.*~$} or \code{.*backup$}) from the source directory.
#' @param rm.vignette Logical, if \code{TRUE} and a vignette PDF was build during the \code{"doc"} action, it will not be kept
#'    in the source package but just be moved to the \code{./pckg/$PACKAGENAME} directory of the repository.
#' @param R.homes Path to the R installation to use. Can be set manually to build packages for other R versions than the default one,
#'    if you have installed them in parallel. Should probably be used together with \code{R.libs}.
#' @param html.index A character string for the headline of the global index HTML file.
#' @param html.title A character string for the title tag prefix of the package index HTML file.
#' @param Rcmd.options A named character vector with options to be passed on to the internal calls of \code{R CMD build},
#'    \code{R CMD INSTALL}, \code{R CMD check} and \code{R CMD Rd2pdf} (or \code{R CMD Rd2dvi} for R < 2.15). Change these only if you know what you're doing!
#'    Will be passed on as given here. To deactivate, options must explicitly be se to \code{""}, missing options will be used with the default values.
#' @param URL A character string defining the URL to the root of the repository (i.e., which holds the directories \code{src}
#'    etc.). This is not the path to the local file system, but should be the URL to the repository as it is available
#'    via internet. This option is neccessary for (and only interpreted by) the actions \code{"news2rss"}, \code{"deb"}, and possibly \code{"html"} --
#'    if \code{flattrUser} is also set in \code{readme.options}, a Flattr button will be added to the HTML page, using this value.
#' @param deb.options A named list with parameters to pass through to \code{\link[roxyPackage:debianize]{debianize}}. By default, \code{pck.source.dir}
#'    and \code{repo.root} are set to the values given to the parameters above. As for the other options, if not set, the defaults of \code{debianize}
#'    will be used.
#' @param readme.options A named list with parameters that add optional extra information to an initial README.md file, namely instructions to install the package
#'    directly from a GitHub repository, and a Flattr button. Ignore this if you don't use either. Theoretically, you can overwrite all values of the internal
#'    function \code{readme_text} (e.g., try \code{formals(roxyPackage:::readme_text)}). But in practice, these two should be all you need to set:
#'    \describe{
#'      \item{\code{githubUser}}{Your GitHub user name, can be used both to contruct the GitHub repo URL as well as the Flattr URL}
#'      \item{\code{flattrUser}}{Your Flattr user name, also used by the \code{"html"} action in combination with \code{URL}}
#'    }
#'    All other missing values are then guessed from the other package information. It is then assumed that the GitHub repo has the same name as the package.
#' @param ChangeLog A named list of character vectors with log entry items. The element names will be used as section names in the ChangeLog entry,
#'    and each character string in a vector will be pasted as a log item. The news you provide here will be appended to probably present news, while
#'    trying to prevent duplicate entries to appear. If you need more control, don't use the \code{"log"} action, but have a look at
#'    \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}. Also note that the date of altered entries will be updated automatically, unless
#'    you don't call the \code{"roxy"} action, too.
#' @param Rbuildignore A character vector to be used as lines of an \code{.Rbuildignore} file. If set, this will replace an existing \code{.Rbuildignore}
#'    file. Setting it to an empty string (\code{""}) will remove the file, the default value \code{NULL} will simply keep the file, if one is present.
#' @param Rinstignore A character vector to be used as lines of an \code{.Rinstignore} file. If set, this will replace an existing \code{.Rinstignore}
#'    file. Setting it to an empty string (\code{""}) will remove the file, the default value \code{NULL} will simply keep the file, if one is present.
#' @param OSX.repo A named list of character vectors, one named \code{"main"} defines the main directory below \code{./bin/macosx/} where packages for
#'    Mac OS X should be copied, and the second optional one named \code{"symlink"} can be used to set symbolic links, e.g., \code{symlinks="mavericks"}
#'    would also make the repository available via \code{./bin/macosx/mavericks}. Symbolic links will be ignored when run on on Windows. If you use them,
#'    make sure they're correctly transferred to your server, where applicable.
#' @param ... Additional options passed through to \code{roxygenize}.
#' @references
#' [1] \url{http://cran.r-project.org/package=roxygen2}
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to run roxy.package() in a sandbox.
#' @import roxygen2
#' @export
#' @examples
#' \dontrun{
#' ## package description as data.frame:
#' pckg.dscrptn <- data.frame(
#'   Package="SquareTheCircle",
#'   Type="Package",
#'   Title="Squaring the circle using Heisenberg compensation",
#'   Author="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   AuthorR="c(person(given=\"Ernst\", family=\"Dölle\",
#'   email=\"doelle@@eternalwondermaths.example.org\", role=c(\"aut\", \"cre\")))",
#'   Maintainer="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   Depends="R (>= 2.10.0),heisenberg (>= 0.23),tools",
#'   Enhances="rkward",
#'   Description="This package squares the circle using Heisenberg compensation.
#'       The code came from a meeting with Yrla Nor that i had in a dream. Please
#'       don't forget to chain your computer to the ground, because these
#'       algorithms might make it fly.",
#'   License="GPL (>= 3)",
#'   Encoding="UTF-8",
#'   LazyLoad="yes",
#'   URL="http://eternalwondermaths.example.org",
#'   stringsAsFactors=FALSE)
#' # hint no. 1: you *don't* specify version number and release date here,
#' #   but all other valid fields for DESCRIPTION files must/can be defined
#' # hint no. 2: most of this rarely changes, so you can add this to the
#' #   internals of your package and refer to it as
#' #   roxy.package(pck.description=SquareTheCircle:::pckg.dscrptn, ...)
#' # hint no. 3: use "AuthorR" for the "Author@@R" field, or "AuthorsR" for
#' # R >= 2.14, to work around naming problems
#'
#' roxy.package(pck.source.dir="~/my_R_stuff/SquareTheCircle",
#'   pck.version="0.01-2",
#'   pck.description=pckg.dscrptn,
#'   R.libs="~/R",
#'   repo.root="/var/www/repo",
#'   actions=c("roxy", "package", "doc"))
#' }

roxy.package <- function(
  pck.source.dir,
  pck.version,
  pck.description,
  R.libs,
  repo.root,
  pck.date=Sys.Date(),
  actions=c("roxy", "package"),
  cleanup=FALSE,
  rm.vignette=FALSE,
  R.homes=R.home(),
  html.index="Available R Packages",
  html.title="R package",
  Rcmd.options=c(
    install="--install-tests",
    build="--no-manual --no-build-vignettes",
    check="--as-cran",
    Rd2pdf="--pdf --no-preview"),
  URL=NULL,
  deb.options=NULL,
  readme.options=NULL,
  ChangeLog=list(changed=c("initial release"), fixed=c("missing ChangeLog")),
  Rbuildignore=NULL,
  Rinstignore=NULL,
  OSX.repo=list(main="contrib", symlinks="mavericks"),
  ...){

  # avoid some NOTEs from R CMD check
  AuthorR <- AuthorsR <- Author.R <- Authors.R <- NULL

  # check the OS first
  unix.OS <- isUNIX()

  # let's check if packages are to be build for several R versions
  R.versions <- length(R.homes)
  R.libraries <- length(R.libs)
  if(R.versions > 1){
    if(R.libraries != R.versions){
      stop(simpleError("If you specify more than one R.home, you must also define as many R.libs!"))
    } else {}
    # if so, iterate recursively through it and then end
    for (this.R in seq_along(R.homes)){
      this.home <- R.homes[this.R]
      this.libs <- R.libs[this.R]
      this.actions <- actions
      this.deb.options <- deb.options
      if("deb" %in% actions){
        if(this.R == 1){
          # for the time being, debianizing the package once is enough
          # the actual binary package build will only be done at the last run
           this.deb.options[["actions"]] <- deb.options[["actions"]][deb.options[["actions"]] %in% "deb"]
        } else if(this.R == R.versions){
          # we'll only do it the *last* run
          this.deb.options[["actions"]] <- deb.options[["actions"]][deb.options[["actions"]] %in% c("bin", "src")]
        } else {
          # all other cases: no debianizing
          this.deb.options[["actions"]] <- ""
        }
      } else {}
      if(this.R > 1){
        # well, the same is true for some other actions
        # we'll only perform them during the *first* run
        this.actions <- actions[!actions %in% c("roxy", "cite", "license", "doc", "cl2news", "news2rss", "cleanRd", "readme")]
        # we also don't need to repeat handling of .Rbuildignore and .Rinstignore
        Rbuildignore <- Rinstignore <- NULL
      } else {}
      roxy.package(
        pck.source.dir=pck.source.dir,
        pck.version=pck.version,
        pck.description=pck.description,
        R.libs=this.libs,
        repo.root=repo.root,
        pck.date=pck.date,
        actions=this.actions,
        cleanup=cleanup,
        rm.vignette=rm.vignette,
        R.homes=this.home,
        html.index=html.index,
        html.title=html.title,
        Rcmd.options=Rcmd.options,
        URL=URL,
        deb.options=this.deb.options,
        readme.options=readme.options,
        ChangeLog=ChangeLog,
        Rbuildignore=Rbuildignore,
        Rinstignore=Rinstignore,
        OSX.repo=OSX.repo,
        ...)
    }
    return(invisible(NULL))
  } else {}

  old.dir <- getwd()
  on.exit(setwd(old.dir))

  # fist see if we need to rename an "AuthorR" field. data.frame definitions tend to
  # replace the "@" with a dot
  if("AuthorR" %in% names(pck.description)){
    pck.description$`Author@R` <- pck.description[["AuthorR"]]
    pck.description <- subset(pck.description, select=-AuthorR)
  } else if("AuthorsR" %in% names(pck.description)){
    pck.description$`Authors@R` <- pck.description[["AuthorsR"]]
    pck.description <- subset(pck.description, select=-AuthorsR)
  } else if("Author.R" %in% names(pck.description)){
    pck.description$`Author@R` <- pck.description[["Author.R"]]
    pck.description <- subset(pck.description, select=-Author.R)
  } else if("Authors.R" %in% names(pck.description)){
    pck.description$`Authors@R` <- pck.description[["Authors.R"]]
    pck.description <- subset(pck.description, select=-Authors.R)
  } else {}

  # normalize all root paths
  R.homes <- normalizePathByOS(path=R.homes, is.unix=unix.OS, mustWork=TRUE)
  repo.root <- normalizePathByOS(path=repo.root, is.unix=unix.OS, mustWork=FALSE)
  pck.source.dir <- normalizePathByOS(path=pck.source.dir, is.unix=unix.OS, mustWork=TRUE)

  # get info on the R version used
  R.Version.full <- getRvers(R.homes=R.homes)
  R.Version.win <- getRvers(R.homes=R.homes, win=TRUE)
  if(nchar(R.Version.full) < 3 | nchar(R.Version.win) < 3){
    stop(simpleError("R version number cannot be detected, it seems."))
  } else {}

  # special cases: if actions do not include "roxy",
  # take infos from the DESCRIPTION file
  if(!"roxy" %in% actions | is.null(pck.description)){
    pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)
    # clean from newlines
    pck.dscrptn <- as.data.frame(t(sapply(pck.dscrptn, function(x) gsub("\n", " ", x))), stringsAsFactors=FALSE)
    pck.version <- getDescField(pck.dscrptn, field="Version")
    # if "Date" is missing, try some fallbacks
    pck.date <- as.character(as.Date(getDescField(pck.dscrptn, field=c("Date","Packaged","Date/Publication"))))
    pck.package <- getDescField(pck.dscrptn, field="Package")
    pck.title <- getDescField(pck.dscrptn, field="Title")
    rss.description <- getDescField(pck.dscrptn, field="Description")
    pckg.license <- getDescField(pck.dscrptn, field="License")
    pckg.dscrptn <- pck.description <- pck.dscrptn
  } else {
    rss.description <- pck.description[["Description"]]
    pck.package <- roxy.description("package", description=pck.description)
    pck.title <- roxy.description("title", description=pck.description)
    pckg.dscrptn <- roxy.description("description", description=pck.description, version=pck.version, date=as.character(pck.date), R.vers=R.Version.full)
    pckg.license <- pck.description[["License"]]
  }
  pckg.package <- roxy.description("pckg.description", description=pck.description, version=pck.version, date=as.character(pck.date))

  ## check for sandboxing
  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use.
    if("check" %in% actions){
      # for check runs we need also all suggested packages
      initSuggests <- TRUE
    } else {
      initSuggests <- FALSE
    }
    adjust.paths <- prepare.sandbox(
      package=pck.package,
      description=pckg.dscrptn,
      pck.source.dir=pck.source.dir,
      R.libs=R.libs,
      R.version=R.Version.full,
      repo.root=repo.root,
      depLevel=c("Depends", "Imports"),
      initSuggests=initSuggests)
    # replace paths with sandbox
    pck.source.dir <- adjust.paths[["pck.source.dir"]]
    R.libs <- adjust.paths[["R.libs"]]
    repo.root <- adjust.paths[["repo.root"]]
    sandbox.status()
  } else {}

  # check environment
  R.bin <- file.path(R.homes, "bin", "R")
  message(paste("R environment\n  R.home:", R.homes, "\n  R.libs:", R.libs))

  repo.src.contrib <- file.path(repo.root, "src", "contrib")
  repo.win <- file.path(repo.root, "bin", "windows", "contrib", R.Version.win)
  # we'll do proper checks in the mac section, no need to freak out here just yet
  # if OSX.repo is set incorrectly
  repo.macosx <- file.path(repo.root, "bin", "macosx")
  repo.macosx.main <- file.path(repo.macosx, OSX.repo[["main"]])
  repo.macosx.R <- file.path(repo.macosx.main, R.Version.win)
  repo.pckg.info.main <- file.path(repo.root, "pckg")
  repo.pckg.info <- file.path(repo.pckg.info.main, pck.package)
  pckg.basename <- paste0(pck.package, "_", pck.version)
  pckg.name.src <- paste0(pckg.basename, ".tar.gz")
  pckg.name.win <- paste0(pckg.basename, ".zip")
  pckg.name.mac <- paste0(pckg.basename, ".tgz")
  pckg.inst.dir <- file.path(pck.source.dir, "inst")
  pckg.cite.file <- file.path(pckg.inst.dir, "CITATION")
  pckg.cite.file.html <- file.path(repo.pckg.info, "citation.html")
  src.changelog <- file.path(pck.source.dir, "ChangeLog")
  pckg.changelog <- file.path(repo.pckg.info, "ChangeLog")
  repo.src.gz <- file.path(repo.src.contrib, pckg.name.src)
  win.package <- file.path(repo.win, pckg.name.win)
  macosx.package <- file.path(repo.macosx.R, pckg.name.mac)
  pckg.NEWS.Rd <- file.path(pckg.inst.dir, "NEWS.Rd")
  pckg.NEWS.inst <- file.path(pckg.inst.dir, "NEWS")
  pckg.NEWS <- file.path(pck.source.dir, "NEWS")
  pckg.NEWS.html <- file.path(repo.pckg.info, "NEWS.html")
  RSS.file.name <- "RSS.xml"
  pckg.NEWS.rss <- file.path(repo.pckg.info, RSS.file.name)
  pckg.license.file <- file.path(pck.source.dir, "LICENSE")
  pckg.license.file.old <- file.path(pck.source.dir, "LICENSE.txt")
  pckg.readme.file <- file.path(pck.source.dir, "README.md")
  pckg.pdf.doc <- paste0(pck.package, ".pdf")
  
  # take care of .Rbuildignore and .Rinstignore
  pckg.Rbuildignore <- configFile(root=pck.source.dir, type="build", content=Rbuildignore)
  pckg.Rinstignore <- configFile(root=pck.source.dir, type="inst", content=Rinstignore)

  # debian package specific stuff, probably needed for "html" action
  deb.defaults <- mergeOptions(
    someFunction=debianize,
    customOptions=deb.options,
    newDefaults=list(
      pck.source.dir=pck.source.dir,
      repo.root=repo.root,
      deb.keyring.options=list(URL=URL)
    )
  )
  # try to set pckg.name.deb and deb.repo.path
  # this will only work if repo.root is unchanged, the rest is too messy now...
  # don't ry to replace this without checking the outcome in the HTML file!
  deb.repo.path.part <- file.path("deb", "dists", deb.defaults[["distribution"]], deb.defaults[["component"]], deb.defaults[["arch"]])
  deb.repo.path <- file.path("..", "..", deb.repo.path.part)
  # need to get repo.name to be able to call eval() on deb.defaults[["origin"]], because that pastes repo.name
  repo.name <- deb.defaults[["repo.name"]]
  deb.defaults[["origin"]] <- eval(deb.defaults[["origin"]])
  pckg.name.deb.part <- debianPkgName(package=pck.package, origin=deb.defaults[["origin"]], version=NULL, replace.dots=FALSE)
  pckg.name.deb <-  paste0(
    pckg.name.deb.part, "_",
    debianPkgVersion(version=pck.version, revision=deb.defaults[["revision"]], epoch=deb.defaults[["epoch"]]), "_",
    deb.defaults[["arch"]], ".deb"
  )
  deb.package <- file.path(repo.root, deb.repo.path.part, pckg.name.deb)

  # check for additional CMD options
  if("build" %in% names(Rcmd.options)){
    Rcmd.opt.build <- paste0(Rcmd.options[["build"]], " ")
    # replace options to enable them in multi-version builds
    if(isTRUE(R_system_version(R.Version.full) < "3.0")){
      Rcmd.opt.build <- gsub("--no-build-vignettes", "--no-vignettes", Rcmd.opt.build)
    } else {
      Rcmd.opt.build <- gsub("--no-vignettes", "--no-build-vignettes", Rcmd.opt.build)
    }
  } else {
    # --no-vignettes is deprecated since R 3.0.0, need version check here
    Rcmd.opt.novignettes <- ifelse(isTRUE(R_system_version(R.Version.full) < "3.0"), "--no-vignettes ", "--no-build-vignettes ")
    # --no-manual was introduced with R 2.12, need version check here
    Rcmd.opt.build <- ifelse(isTRUE(R_system_version(R.Version.full) < "2.12"),
      Rcmd.opt.novignettes,
      paste0("--no-manual ", Rcmd.opt.novignettes))
  }
  
  Rcmd.opt.install <- ifelse("install" %in% names(Rcmd.options), paste0(Rcmd.options[["install"]], " "), "")
  Rcmd.opt.check <- ifelse("check" %in% names(Rcmd.options), paste0(Rcmd.options[["check"]], " "), "")
  # --as-cran was introduced with R 2.15, strip if this is an older version
  if(isTRUE(R_system_version(R.Version.full) < "2.15")){
    Rcmd.opt.check <- gsub("--as-cran", "", Rcmd.opt.check)
  } else {}
  # R 2.15 switched from Rd2dvi to Rd2pdf
  Rcmd.cmd.Rd2pdf <- ifelse(isTRUE(R_system_version(R.Version.full) < "2.15"), "Rd2dvi", "Rd2pdf")
  if("Rd2pdf" %in% names(Rcmd.options)){
    Rcmd.opt.Rd2pdf <- paste0(Rcmd.options[["Rd2pdf"]], " ")
  } else if("Rd2dvi" %in% names(Rcmd.options)){
    warning("Rcmd.options: Rd2dvi is now called Rd2pdf, please update your scripts! used the settings anyway, though.", call.=FALSE)
    Rcmd.opt.Rd2pdf <- paste0(Rcmd.options[["Rd2dvi"]], " ")
  } else {
    Rcmd.opt.Rd2pdf <- "--pdf --no-preview "
  }

  # check for/create info directory
  createMissingDir(dirPath=repo.pckg.info, action="repo")
  
  # clean up
  if(isTRUE(cleanup)){
    unlink(list.files(pck.source.dir, pattern=".*~$", full.names=TRUE, recursive=TRUE))
    unlink(list.files(pck.source.dir, pattern=".*backup$", full.names=TRUE, recursive=TRUE))
  } else {}

  if("license" %in% actions){
    if(checkLicence(pckg.license)){
      copyLicence(pckg.license, pckg.license.file, overwrite=TRUE)
      if(file.exists(pckg.license.file.old)){
        warning("license: you have both LICENSE and LICENSE.txt in your project! if LICENSE.txt is one of the standard licenses, please rename it to prevent its intallation.", call.=FALSE)
      } else {}
    } else {
      stop(simpleError(paste0("license: unrecognized license (", pckg.license, "), please provide your own LICENSE file! ")))
    }
  } else {}

  if("readme" %in% actions){
    if(file.exists(pckg.readme.file)){
      warning("readme: README.md exists, please remove it if you want it re-written!", call.=FALSE)
    } else {
      readme.defaults <- mergeOptions(
        someFunction=readme_text,
        customOptions=readme.options,
        newDefaults=list(
          package=pck.package,
          description=pck.description[["Description"]],
          url=pck.description[["URL"]],
          license=pck.description[["License"]],
          author=paste0(
            gsub(
              "[[:space:]]*<[^>]*>",
              "",
              get.authors(description=pck.description, maintainer=FALSE, contributor=FALSE, copyright=FALSE)[["aut"]]
            ),
            collapse=", "
          ),
          githubRepo=pck.package,
          fl_title=pck.package
        )
      )
      formals(readme_text) <- readme.defaults
      cat(
        readme_text(),
        file=pckg.readme.file
      )
      message(paste0("readme: generated initial ", pckg.readme.file, "."))
    }
  } else {}
  
  if("roxy" %in% actions){
    add.options <- list(...)
    if(any(c("local.roxy.dir", "roxy.unlink.target") %in% names(add.options))){
      stop(simpleError("you are still using \"local.roxy.dir\" and/or \"roxy.unlink.target\", these options have been removed from roxyPackage as of 0.04-2, since they are no longer supported by roxygen2. please change your roxy.package() call!"))
    } else {}

    # re-write DESCRIPTION files
    write.dcf(pckg.dscrptn, file=file.path(pck.source.dir, "DESCRIPTION"))
    # copy DESCRIPTION to pckg directory for easy HTML indexing
    stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
    pckg.package.file.R <- file.path(pck.source.dir, "R", paste0(pck.package, "-package.R"))
    cat(paste(pckg.package), file=pckg.package.file.R)
    message(paste0("roxy: updated ", pckg.package.file.R, "."))

#     # a -- hopefully temporary -- workaround for roxygen2 >= 3.0.0
#     # there's still issues with S4 classes, only said to be working with devtools,
#     # but we don't want to add new dependencies just to get this working.
#     # so here's a call which seems to do the trick, alas we don't yet know why... ;-)
#     if(packageVersion("roxygen2") > "2.2.2"){
#       message("roxy: applying workaround for roxygen2 >= 3.0.0...")
#       local(dummyResult <- roxygen2:::source_package(pck.source.dir))
#     } else {}
    roxygen2::roxygenize(pck.source.dir, ...)
  } else {}

  if("cleanRd" %in% actions){
    pckg.man.dir <- file.path(pck.source.dir, "man")
    Rd.files <- list.files(pckg.man.dir, pattern="*.Rd", ignore.case=TRUE)
    if(length(Rd.files) > 0){
      for (this.file in Rd.files){
        sanitizeRdFiles(this.file, root.dir=pckg.man.dir, maxlength=90)
      }
    } else {}
  } else {}

  if("cite" %in% actions){
    createMissingDir(dirPath=pckg.inst.dir, action="cite")
    # calling internal function to generate citation object
    cite.obj <- citationText(pck.dscr=pck.description, pck.version=pck.version, pck.date=pck.date)
    cat(cite.obj, file=pckg.cite.file)
    message(paste0("cite: updated ", pckg.cite.file, "."))
    if("html" %in% actions){
      cite.obj.html <- roxy.html.cite(cite.obj=eval(parse(text=cite.obj)), page.css="../web.css", package=pck.package)
      cat(cite.obj.html, file=pckg.cite.file.html)
      message(paste0("cite: updated ", pckg.cite.file.html, "."))
    } else {}
  } else {}

  if("log" %in% actions){
    if(!file.exists(src.changelog)){
      newLog <- initChangeLog(entry=ChangeLog, package=pck.package, version=pck.version, date=pck.date)
      writeChangeLog(log=newLog, file=src.changelog)
      message("log: generated initial ChangeLog")
    } else {
      oldLog <- readChangeLog(src.changelog)
      newLog <- updateChangeLog(oldLog, entry=ChangeLog, version=pck.version, date=pck.date, append=TRUE)
      newLogEntry <- getChangeLogEntry(newLog, pck.version)
      if(!identical(oldLog, newLog)){
        writeChangeLog(log=newLog, file=src.changelog)
        message("log: updated ChangeLog")
        print(newLogEntry)
      } else {
        message("log: ChangeLog is up-to-date, skipped")
      }
    }
  } else {}

  ## update PDF docs
  if("doc" %in% actions){
    # had to move this down here, because tools::pkgVignettes() and tools::buildVignettes()
    # don't work without an existing DESCRIPTION file since R 3.0.1... hooooray...
    pckg.vignette.info <- tools::pkgVignettes(dir=pck.source.dir)
    pckg.vignette.dir <- pckg.vignette.info$dir
    pckg.vignette.names <- pckg.vignette.info$names

    if(!is.null(pckg.vignette.names)){
      pckg.vignette.namesPDF <- paste0(pckg.vignette.names, ".pdf")
      # we probably need to clean up first, because buildVignettes() will
      # do nothing if there's already a PDF vignette present folder
      for (thisVignette in pckg.vignette.names){
        local({
          thisVignetteFiles <- list.files(pckg.vignette.dir, pattern=thisVignette)
          thisVignettePDF <- paste0(thisVignette, ".pdf")
          haveVignettePDF <- any(grepl(paste0(thisVignettePDF, "$"), thisVignetteFiles))
          haveVignetteSrc <- any(grepl(paste0(thisVignette, "[.][rRsS](nw|tex)$"), thisVignetteFiles))
          if(haveVignettePDF && haveVignetteSrc){
            message(paste0("build: remove old PDF vignette (", thisVignettePDF, ")"))
            stopifnot(file.remove(file.path(pckg.vignette.dir, thisVignettePDF)))
          } else {}
        })
      }
      # create and move vignette
      tools::buildVignettes(dir=pck.source.dir)
      # check for possible vignette documents
      # becomes character(0) if none found
      pdf.vignette.files <- list.files(pckg.vignette.dir,
        pattern=paste0(paste0(pckg.vignette.names, ".pdf"), collapse="|"),
        ignore.case=TRUE)
      createMissingDir(file.path(R.libs, pck.package, "doc"), action="doc")
      createMissingDir(file.path(pck.source.dir, "inst", "doc"), action="doc")
      for(thisVignette in pdf.vignette.files){
        local({
          pdf.vignette.src <- file.path(pckg.vignette.dir, thisVignette)
          pdf.vignette.dst <- file.path(R.libs, pck.package, "doc", thisVignette)
          stopifnot(file.copy(pdf.vignette.src, pdf.vignette.dst, overwrite=TRUE))
          # special case: dedicated vignettes directory, need to copy to inst/doc and
          # remove vignette PDF from source dir
          if(grepl("vignettes$", pckg.vignette.dir)){
            pdf.vignette.src.files <- file.path(pckg.vignette.dir, list.files(pckg.vignette.dir))
            pdf.vignette.doc.dir <- file.path(pck.source.dir, "inst", "doc")
            stopifnot(file.copy(pdf.vignette.src.files, pdf.vignette.doc.dir, overwrite=TRUE))
            stopifnot(file.remove(pdf.vignette.src))
         } else {}
          if(isTRUE(rm.vignette)){
            stopifnot(file.remove(pdf.vignette.src))
          } else {}
          message(paste0("build: created PDF vignette (", thisVignette, ")"))
          ## copy vignettes
          pdf.vignette.repo <- file.path(repo.pckg.info, thisVignette)
          if(file.exists(pdf.vignette.dst)){
            stopifnot(file.copy(pdf.vignette.dst, pdf.vignette.repo, overwrite=TRUE))
            message(paste0("repo: updated vignette (", thisVignette, ")"))
          } else {}
        })
      }
    } else {}

    pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
    removeIfExists(filePath=pdf.docs)
    if(isTRUE(unix.OS)){
      r.cmd.doc.call <- paste0(R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", pdf.docs, " ", pck.source.dir)
      system(r.cmd.doc.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
    } else {
      r.cmd.doc.call <- paste0(R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", shQuote(pdf.docs, type="cmd"), " ", shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.doc.call, translate=TRUE, ignore.stderr=TRUE, intern=FALSE)
    }
    message("build: created PDF docs")
  } else {}

  if(any(c("package", "binonly") %in% actions)){
    ## fill source repo
    createMissingDir(dirPath=repo.src.contrib, action="repo")
    ## TODO: find a solution without sedwd()
    jmp.back <- getwd()
    pck.source.dir.parent <- dirname(file.path(pck.source.dir))
    setwd(pck.source.dir.parent)
    if(isTRUE(unix.OS)){
      r.cmd.build.call <- paste0(R.bin, " CMD build ", Rcmd.opt.build, pck.source.dir)
      system(r.cmd.build.call, intern=TRUE)
    } else {
      r.cmd.build.call <- paste0(R.bin, " CMD build ", Rcmd.opt.build, shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.build.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
    }
    if(!"binonly" %in% actions){
      file.mv(from=file.path(pck.source.dir.parent,pckg.name.src), to=repo.src.gz, overwrite=TRUE)
      message(paste0("repo: copied ", pckg.name.src, " to src/contrib."))
    } else {}
    setwd(jmp.back)
    # install.packages() doesn't work if we want to build for/with other R installations than
    # the actual running one, so we'll use  R CMD INSTALL instead
    if(isTRUE(unix.OS)){
      r.cmd.install.call <- paste0(R.bin, " CMD INSTALL -l ", R.libs, " ",
        Rcmd.opt.install, pck.source.dir)
      system(r.cmd.install.call, intern=TRUE)
    } else {
      r.cmd.install.call <- paste0(R.bin, " CMD INSTALL -l ", shQuote(R.libs, type="cmd"), " ",
        Rcmd.opt.install, shQuote(pck.source.dir, type="cmd"))
      shell(r.cmd.install.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
    }
    message("build: built and installed package")

    if(!"binonly" %in% actions){
      tools::write_PACKAGES(dir=repo.src.contrib, type="source", verbose=TRUE, latestOnly=FALSE)
      message("repo: updated src/contrib/PACKAGES (source)")

      ## update ChangeLog
      if(file.exists(src.changelog)){
        stopifnot(file.copy(src.changelog, pckg.changelog, overwrite=TRUE))
        message("pckg: updated ChangeLog")
      } else {}
    } else {}
  } else {}

  ## create NEWS.Rd from ChangeLog
  if("cl2news" %in% actions){
    createMissingDir(dirPath=pckg.inst.dir, action="news")
    cl2news(log=src.changelog, news=pckg.NEWS.Rd, codify=TRUE, overwrite=TRUE)
  } else {}

  ## create RSS feed from NEWS.Rd
  add.RSS <- FALSE
  if("news2rss" %in% actions){
    if(is.null(URL)){
      warning("news: no URL specified, RSS feed creation was skipped!", call.=FALSE)
    } else {
      package.URL <- paste(gsub("/*$", "", URL), "pckg", pck.package, sep="/")
      RSS.atom.URL <- paste(package.URL, RSS.file.name, sep="/")
      news2rss(
        news=pckg.NEWS.Rd, rss=pckg.NEWS.rss, html=FALSE, encoding="UTF-8",
        channel=c(
          title=pck.package,
          link=package.URL,
          description=rss.description,
          atom=RSS.atom.URL))
      add.RSS <- TRUE
    }
  } else {}

  ## fill windows repo
  if("win" %in% actions){
    createMissingDir(dirPath=repo.win, action="repo")
    removeIfExists(filePath=win.package)
    ## TODO: find a solution without sedwd()
    jmp.back <- getwd()
    setwd(R.libs)
    # make a list of backup files to exclude
    win.exclude.files <- unique(list.files(pck.package, pattern=".*~$", recursive=TRUE))
    if(length(win.exclude.files) > 0){
      win.exclude.files <- paste0("-x \"", paste(file.path(pck.package, win.exclude.files), collapse="\" \""), "\"")
    } else {}
    suppressWarnings(zip(win.package, pck.package, extras=win.exclude.files))
    message(paste0("repo: created ", pckg.name.win, " (windows)"))
    setwd(jmp.back)
    tools::write_PACKAGES(dir=repo.win, type="win.binary", verbose=TRUE, latestOnly=FALSE)
    message("repo: updated bin/PACKAGES (windows)")
  } else {}

  ## fill macosx repo
  if("macosx" %in% actions){
    # sanity check
    if(nchar(OSX.repo[["main"]]) > 0){
      createMissingDir(dirPath=repo.macosx.R, action="repo")
      if(!is.null(OSX.repo[["symlinks"]])){
        for (thisSymlink in OSX.repo[["symlinks"]]){
          thisSymPath <- file.path(repo.macosx, thisSymlink)
          if(isTRUE(unix.OS)){
            if(!file.exists(thisSymPath)){
              jmp.back <- getwd()
              setwd(repo.macosx)
              r.cmd.symlink.call <- paste0(Sys.which("ln"), " -s . ", thisSymlink)
              message(paste0("repo: creating symbolic link ", thisSymPath))
              system(r.cmd.symlink.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
              setwd(jmp.back)
            } else {}
          } else {
            message(paste0("repo: skip creating symbolic link ", thisSymPath, " (windows)"))
          }
        }
      } else {}
    } else {
      stop(simpleError("repo: you *must* provide a character string for \"main\" via OSX.repo (mac)!"))
    }
    removeIfExists(filePath=macosx.package)
    # since not all tar implementations (especially the BSD default on Mac OS X) support --exclude-vcs,
    # we'll exclude these manually
    VCS.directories <- c(".svn", "CVS", ".git", "_darcs", ".hg")
    VCS.allDirs <- list.dirs(file.path(R.libs,pck.package))
    VCS.excludeDirs <- VCS.allDirs[grepl(paste(paste0(".*", VCS.directories, "$"), collapse="|"), VCS.allDirs)]
    if(length(VCS.excludeDirs) > 0){
      tar.extraFlags <- paste(paste0(" --exclude='", VCS.excludeDirs, "'"), collapse="")
      message(paste("repo: excluded these directories from the mac binary:\n  ", VCS.excludeDirs, collapse="\n  "))
    } else {
      tar.extraFlags <- ""
    }
    if(isTRUE(unix.OS)){
      # make more portable archives, should work with GNU and BSD tar
      tar.extraFlags <- paste0(tar.extraFlags, " --format=ustar")
    } else {}
    # failsafe exclusion of backup files
    # --exclude=*\\~ caused trouble for path names with tilde
    tilde.allFiles <- list.files(file.path(R.libs,pck.package))
    tilde.excludeFiles <- tilde.allFiles[grepl(paste(paste0(".*~$"), collapse="|"), tilde.allFiles)]
    if(length(tilde.excludeFiles) > 0){
      tar.extraFlags <- paste(paste0(" --exclude='", tilde.excludeFiles, "'"), tar.extraFlags, collapse="")
      message(paste("repo: excluded these files from the mac binary:\n  ", tilde.excludeFiles, collapse="\n  "))
    } else {}

    ## TODO: find a solution without sedwd()
    jmp.back <- getwd()
    setwd(R.libs)
    # it seems that tar() in R 3.3 produces archives that contain files twice and
    # cannot be unpacked with OS X's bsdtar, so we'll try this manually
    #     tar(macosx.package, files=pck.package,
    #       tar=Sys.which("tar"),
    #       compression="gzip", extra_flags=paste("-h ", tar.extraFlags))
    mac.tar.call <- paste0(Sys.which("tar"), " -czhf ", macosx.package, " ", tar.extraFlags, " ", pck.package)
    system(mac.tar.call, ignore.stdout=TRUE, ignore.stderr=TRUE, intern=FALSE)
    message(paste0("repo: created ", pckg.name.mac, " (mac OS X)"))
    setwd(jmp.back)
    tools::write_PACKAGES(dir=repo.macosx.R, type="mac.binary", verbose=TRUE, latestOnly=FALSE)
    message("repo: updated bin/PACKAGES (mac OS X)")
  } else {}

  ## fill debian repo
  if("deb" %in% actions){
    formals(debianize) <- deb.defaults
    debianize()
  } else {}

  ## update HTML index
  if("html" %in% actions){
    if(!"roxy" %in% actions){
      # copy DESCRIPTION to pckg directory
      stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
    } else {}
    # write CSS file, if none is present
    css.file <- file.path(repo.pckg.info.main, "web.css")
    if(!file_test("-f", css.file)){
      cat(rx.css(), file=css.file)
      message(paste0("html: created CSS file ", css.file))
    } else {}
    # copy RSS image, if not present
    RSS.image <- file.path(repo.pckg.info.main, "feed-icon-14x14.png")
    if(!file_test("-f", RSS.image)){
      RSS.local.image <- file.path(roxyPackage.lib.dir(), "images", "feed-icon-14x14.png")
      stopifnot(file.copy(RSS.local.image, RSS.image))
      message(paste0("html: copied RSS image to ", RSS.image))
    } else {}
    if(all(!is.null(readme.options[["flattrUser"]]), !is.null(URL))){
      # copy flattr image, if not present
      flattr.image <- file.path(repo.pckg.info.main, "flattr-badge-large.png")
      if(!file_test("-f", flattr.image)){
        flattr.local.image <- file.path(roxyPackage.lib.dir(), "images", "flattr-badge-large.png")
        stopifnot(file.copy(flattr.local.image, flattr.image))
        message(paste0("html: copied Flattr button image to ", flattr.image))
      } else {}
    } else {}
    # check for binaries to link
    url.src <- url.win <- url.mac <- url.deb <- url.doc <- url.vgn <- deb.repo <- NULL
    if(file_test("-f", repo.src.gz)){
      url.src <- pckg.name.src
    } else {}
    if(file_test("-f", win.package)){
      url.win <- binPackageLinks(package=pck.package, version=pck.version, repo.root=repo.root, type="win")[["win"]]
    } else {}
    if(file_test("-f", macosx.package)){
      url.mac <- binPackageLinks(package=pck.package, version=pck.version, repo.root=repo.root, type="mac")[["mac"]]
    } else {}
    url.debRepo.info <- NULL
    if(file_test("-f", deb.package)){
      if(!is.null(URL)){
        # generate repository info
        url.debRepo.info <- file.path(repo.pckg.info, "deb_repo.html")
        cat(debRepoInfo(
          URL=URL,
          dist=deb.defaults[["distribution"]],
          comp=deb.defaults[["component"]],
          package=pckg.name.deb.part,
          repo=deb.defaults[["origin"]],
          repo.name=deb.defaults[["repo.name"]],
          repo.root=repo.root,
          keyring.options=deb.defaults[["keyring.options"]],
          page.css="../web.css",
          package.full=pckg.name.deb,
          repo.path=deb.repo.path),
        file=url.debRepo.info)
        message(paste0("html: updated ", url.debRepo.info))
      } else {
        message("html: you need to specify 'URL' to generate debian repository information!")
      }
    } else {}
    # check if there is actually any built debian package in the repo
    # if not, the link to the debian installation notes will be omitted
    deb.built.packages <- dir(file.path(repo.root, deb.repo.path.part), pattern=paste0(pckg.name.deb.part, ".*", ".deb"))
    url.deb.repo <- NULL
    if(length(deb.built.packages) > 0){
      url.deb.repo <- "deb_repo.html"
    } else {}
    # check for docs to link
    pdf.docs.repo.files <- list.files(repo.pckg.info, pattern="*.pdf", ignore.case=TRUE)
    pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
    pdf.vignette.repo <- pdf.docs.repo.files[!pdf.docs.repo.files %in% pckg.pdf.doc]
    if(file_test("-f", pdf.docs)){
      url.doc <- pckg.pdf.doc
    } else {}
    if(length(pdf.vignette.repo) > 0){
      url.vgn <- pdf.vignette.repo
    } else {}
    # check for NEWS.Rd or NEWS file
    if(file_test("-f", pckg.NEWS.Rd)){
      roxy.NEWS2HTML(newsRd=pckg.NEWS.Rd, newsHTML=file.path(repo.pckg.info, "NEWS.html"), pckg=pck.package, css="../web.css", R.version=R.Version.full)
      url.NEWS <- pckg.NEWS.html
    } else if(file_test("-f", pckg.NEWS.inst)){
      stopifnot(file.copy(pckg.NEWS.inst, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
      url.NEWS <- file.path(repo.pckg.info, "NEWS")
    } else if(file_test("-f", pckg.NEWS)){
      stopifnot(file.copy(pckg.NEWS, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
      url.NEWS <- file.path(repo.pckg.info, "NEWS")
    } else {
      url.NEWS <- ""
    }
    # generate package index file
    if(!file_test("-f", pckg.NEWS.rss)){
      RSS.file.name <- NULL
    } else {}
    package.html <- roxy.html(pckg.dscrptn, index=FALSE, css="web.css", R.version=R.Version.win,
      url.src=url.src, url.win=url.win, url.mac=url.mac, url.doc=url.doc, url.vgn=url.vgn,
      url.deb.repo=url.deb.repo, main.path.mac=OSX.repo[["main"]],
      title=html.title, cite=pckg.cite.file.html, news=url.NEWS,
      changelog=pckg.changelog, rss.file=RSS.file.name,
      flattrUser=readme.options[["flattrUser"]], URL=URL
    )
    target.file.pckg <- file.path(repo.pckg.info, "index.html")
    cat(package.html, file=target.file.pckg)
    message(paste0("html: updated ", target.file.pckg))
    # now generate the global index file by scanning for DESCRIPTIONs in all pckg folders
    all.descs <- list()
    for (this.elmt in file.path(repo.pckg.info.main, dir(repo.pckg.info.main))){
        desc.path <- file.path(this.elmt, "DESCRIPTION")
        if(file_test("-f", desc.path)){
          # ok, there a DESCRIPTION file
          all.descs[[length(all.descs) + 1]] <- read.dcf(desc.path)
        } else {}
      }
    target.file.pckg <- file.path(repo.pckg.info.main, "index.html")
    pckg.index.html <- roxy.html(all.descs, index=TRUE, css="web.css", title=html.index)
    cat(pckg.index.html, file=target.file.pckg)
    message(paste0("html: updated pckg index ", target.file.pckg))
    target.file.glob <- file.path(repo.root, "index.html")
    global.html <- roxy.html(all.descs, index=TRUE, css="web.css", title=html.index, redirect="pckg/")
    cat(global.html, file=target.file.glob)
    message(paste0("html: updated global index ", target.file.glob))
  } else {}

  if("check" %in% actions){
    # check for examples check file before
    chk.ex.file <- file.path(pck.source.dir, paste0(pck.package, "-Ex.R"))
    chk.ex.file.present <- ifelse(file_test("-f", chk.ex.file), TRUE, FALSE)
    tryCatch(chk.out.dir <- tempdir(), error=function(e) stop(e))
    # checks should better be performed on built packages not source directories
    if("package" %in% actions & file_test("-f", repo.src.gz)){
      pck.check.target <- repo.src.gz
    } else {
      pck.check.target <- pck.source.dir
    }
    message(paste0("check: calling R CMD check, this might take a while..."))
    if(isTRUE(unix.OS)){
      r.cmd.check.call <- paste0("R_LIBS_USER=", R.libs, " ; ",
        R.bin, " CMD check --output=", chk.out.dir, " ", Rcmd.opt.check, pck.check.target)
      print(system(r.cmd.check.call, intern=TRUE))
    } else {
      r.cmd.check.call <- paste0("set R_LIBS_USER=", shQuote(R.libs, type="cmd"), " && ",
        R.bin, " CMD check --output=", chk.out.dir, " ", Rcmd.opt.check, shQuote(pck.check.target, type="cmd"))
      print(shell(r.cmd.check.call, translate=TRUE, intern=TRUE))
    }
    on.exit(message(paste0("check: saved results to ", chk.out.dir, "/", pck.package, ".Rcheck")), add=TRUE)
    # need to clean up?
    if(!isTRUE(chk.ex.file.present) & file_test("-f", chk.ex.file)){
      # there's an example file which wasn't here before
      unlink(chk.ex.file)
    } else {}
  } else {}

  return(invisible(NULL))
} ## end function roxy.package()
